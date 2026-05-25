#include <execinfo.h>
#include <ctype.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/mlvalues.h"

#define ARR_SIZE(a)    (sizeof(a) / sizeof(*(a)))

#define RE_FUNC_NAME  "^.*\\((.+)\\+0x[[:xdigit:]]+\\) \\[0x[[:xdigit:]]+\\]$"
#define RE_TRIM_FUNC  "(caml.*)_[[:digit:]]+"
#define CAML_ENTRY    "caml_program"
#define MAX_SYMBOL_LEN 256

typedef struct frame_info
{
  struct frame_info*  prev;     /* rbp */
  void*               retaddr;  /* rip */
} frame_info;


/*
 * A backtrace symbol looks like:
 * ./path/to/binary(camlModule_fn_123+0xAABBCC) [0xAABBCCDDEE]
 */
static const char* backtrace_symbol(const struct frame_info* fi)
{
  char** symbols = backtrace_symbols(&fi->retaddr, 1);
  if (!symbols) {
    perror("backtrace_symbols");
    return NULL;
  }

  const char* symbol = strdup(symbols[0]);
  free(symbols);
  return symbol;
}

static bool is_from_executable(const char* symbol, const char* execname)
{
  if (strncmp(symbol, execname, strlen(execname)) == 0)
    return true;

  const char* execbase = strrchr(execname, '/');
  execbase = execbase ? execbase + 1 : execname;

  const char* p = symbol;
  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;

  const char* image = p;
  while (*p && !isspace((unsigned char)*p)) p++;

  size_t len = p - image;
  return strlen(execbase) == len && strncmp(image, execbase, len) == 0;
}

static regmatch_t no_match(void)
{
  regmatch_t match = {-1, -1};
  return match;
}

static regmatch_t match_from_span(const char* symbol, const char* start,
                                  const char* end)
{
  regmatch_t match;
  match.rm_so = start - symbol;
  match.rm_eo = end - symbol;
  return match;
}

static char* copy_span(const char* start, const char* end)
{
  size_t len = end - start;
  char* result = malloc(len + 1);
  if (result == NULL) return NULL;
  memcpy(result, start, len);
  result[len] = 0;
  return result;
}

static bool has_suffix(const char* s, const char* suffix)
{
  size_t s_len = strlen(s);
  size_t suffix_len = strlen(suffix);
  return s_len >= suffix_len
    && strcmp(s + s_len - suffix_len, suffix) == 0;
}

static int symbol_score(const char* name)
{
  if (strncmp(name, "c_call_wrapper.", 15) == 0)
    return -1;
  if (strncmp(name, "caml", 4) == 0 && has_suffix(name, "_code")
      && strstr(name, "__code_begin") == NULL)
    return 4;
  if (strncmp(name, "caml", 4) == 0 && strstr(name, "__code_begin") == NULL)
    return 3;
  if (strcmp(name, "fp_backtrace_many_args") == 0
      || strcmp(name, "c_fun") == 0)
    return 2;
  if (strncmp(name, "caml_", 5) == 0)
    return 2;
  if (strcmp(name, "callback") == 0)
    return -1;
  return 1;
}

static char* resolve_darwin_symbol_from_nm(const char* execname, uintnat addr,
                                           const char* fallback_start,
                                           const char* fallback_end)
{
  char command[1024];
  char line[1024];
  char fallback_name[MAX_SYMBOL_LEN];
  uintnat fallback_addr = 0;
  uintnat best_addr = 0;
  int best_score = -1;
  char best_name[MAX_SYMBOL_LEN] = {0};
  uintnat slide;

  size_t fallback_len = fallback_end - fallback_start;
  if (fallback_len >= sizeof(fallback_name))
    fallback_len = sizeof(fallback_name) - 1;
  memcpy(fallback_name, fallback_start, fallback_len);
  fallback_name[fallback_len] = 0;

  if (strstr(fallback_name, "__code_begin") == NULL)
    return strdup(fallback_name);

  if (strchr(execname, '\'') != NULL)
    return strdup(fallback_name);

  snprintf(command, sizeof(command), "nm -an '%s'", execname);
  FILE* fp = popen(command, "r");
  if (fp == NULL)
    return strdup(fallback_name);

  while (fgets(line, sizeof(line), fp) != NULL) {
    unsigned long long symaddr;
    char type;
    char raw_name[MAX_SYMBOL_LEN];
    if (sscanf(line, "%llx %c %255s", &symaddr, &type, raw_name) != 3)
      continue;
    if (type != 't' && type != 'T')
      continue;

    const char* name = raw_name[0] == '_' ? raw_name + 1 : raw_name;
    if (strcmp(name, fallback_name) == 0) {
      fallback_addr = (uintnat)symaddr;
      break;
    }
  }
  pclose(fp);

  if (fallback_addr == 0)
    return strdup(fallback_name);

  slide = addr - fallback_addr;
  addr -= slide;

  fp = popen(command, "r");
  if (fp == NULL)
    return strdup(fallback_name);

  while (fgets(line, sizeof(line), fp) != NULL) {
    unsigned long long symaddr;
    char type;
    char raw_name[MAX_SYMBOL_LEN];
    if (sscanf(line, "%llx %c %255s", &symaddr, &type, raw_name) != 3)
      continue;
    if (type != 't' && type != 'T')
      continue;
    if ((uintnat)symaddr > addr)
      continue;

    const char* name = raw_name[0] == '_' ? raw_name + 1 : raw_name;
    int score = symbol_score(name);
    if ((uintnat)symaddr > best_addr
        || ((uintnat)symaddr == best_addr && score > best_score)) {
      best_addr = (uintnat)symaddr;
      best_score = score;
      strncpy(best_name, name, sizeof(best_name) - 1);
      best_name[sizeof(best_name) - 1] = 0;
    }
  }
  pclose(fp);

  if (best_name[0] == 0)
    return copy_span(fallback_start, fallback_end);

  return strdup(best_name);
}

/*
 * A Darwin backtrace symbol looks like:
 * 0   binary  0x0000000100003f40 camlModule__fn_123_code + 32
 */
static regmatch_t func_name_from_darwin_symbol(const char* symbol)
{
  const char* p = symbol;

  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;
  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;

  if (strncmp(p, "0x", 2) != 0)
    return no_match();

  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;

  const char* start = p;
  const char* end = strstr(p, " + ");
  if (end == NULL || end == start)
    return no_match();

  return match_from_span(symbol, start, end);
}

static char* func_name_from_darwin_symbol_string(const char* symbol,
                                                 const char* execname)
{
  const char* p = symbol;

  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;
  while (*p && !isspace((unsigned char)*p)) p++;
  while (isspace((unsigned char)*p)) p++;

  if (strncmp(p, "0x", 2) != 0)
    return NULL;

  char* endptr = NULL;
  uintnat addr = (uintnat)strtoull(p, &endptr, 16);
  p = endptr;
  while (isspace((unsigned char)*p)) p++;

  const char* start = p;
  const char* end = strstr(p, " + ");
  if (end == NULL || end == start)
    return NULL;

  return resolve_darwin_symbol_from_nm(execname, addr, start, end);
}

static regmatch_t func_name_from_symbol(const char* symbol)
{
  regex_t     regex;
  regmatch_t  match[2] = { {-1, -1}, {-1, -1}};
  char        errbuf[128];
  int         err;

  err = regcomp(&regex, RE_FUNC_NAME, REG_EXTENDED);
  if (err) {
    regerror(err, &regex, errbuf, ARR_SIZE(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  err = regexec(&regex, symbol, ARR_SIZE(match), match, 0);
  regfree(&regex);
  if (err == REG_NOMATCH)
    return func_name_from_darwin_symbol(symbol);

  return match[1];
}

static char* func_name_from_symbol_string(const char* symbol,
                                          const char* execname)
{
  regex_t     regex;
  regmatch_t  match[2] = { {-1, -1}, {-1, -1}};
  char        errbuf[128];
  int         err;

  err = regcomp(&regex, RE_FUNC_NAME, REG_EXTENDED);
  if (err) {
    regerror(err, &regex, errbuf, ARR_SIZE(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return NULL;
  }

  err = regexec(&regex, symbol, ARR_SIZE(match), match, 0);
  regfree(&regex);
  if (err == REG_NOMATCH)
    return func_name_from_darwin_symbol_string(symbol, execname);

  return copy_span(symbol + match[1].rm_so, symbol + match[1].rm_eo);
}

static bool is_caml_entry(const char* symbol, const regmatch_t* funcname)
{
  size_t len = funcname->rm_eo - funcname->rm_so;
  return strncmp(symbol + funcname->rm_so, CAML_ENTRY, len) == 0;
}

static bool is_caml_entry_name(const char* funcname)
{
  return strcmp(funcname, CAML_ENTRY) == 0;
}

static bool should_skip_func_name(const char* funcname)
{
  return strncmp(funcname, "c_call_wrapper.", 15) == 0
    || strcmp(funcname, "callback") == 0;
}

static regmatch_t trim_func_name(const char* symbol, const regmatch_t* funcname)
{
  regex_t     regex;
  regmatch_t  match[2] = { {-1, -1}, {-1, -1}};
  char        errbuf[128];
  int         err;

  err = regcomp(&regex, RE_TRIM_FUNC, REG_EXTENDED);
  if (err) {
    regerror(err, &regex, errbuf, ARR_SIZE(errbuf));
    fprintf(stderr, "regcomp: %s\n", errbuf);
    return match[0];
  }

  match[0] = *funcname;
  err = regexec(&regex, symbol, ARR_SIZE(match), match, REG_STARTEND);
  if (err == REG_NOMATCH) {
    /* match[0] has already been overwritten to hold the function full name for
       regexec */
    return match[1];
  }

  return match[1];
}

static void print_symbol(const char* symbol, const regmatch_t* match)
{
  regoff_t off = match->rm_so;
  regoff_t len = match->rm_eo - match->rm_so;

  fprintf(stdout, "%.*s\n", (int)len, symbol + off);
  fflush(stdout);
}

void fp_backtrace(value argv0)
{
  const char* execname = String_val(argv0);
  struct frame_info* next = NULL;
  const char* symbol = NULL;

  for (struct frame_info* fi = __builtin_frame_address(0); fi; fi = next) {
    next = fi->prev;

    /* Detect the simplest kind of infinite loop */
    if (fi == next) {
      fprintf(stderr, "fp_backtrace: loop detected\n");
      break;
    }

    symbol = backtrace_symbol(fi);
    if (!symbol)
      continue;

    /* Skip entries not from the test */
    if (!is_from_executable(symbol, execname))
      goto skip;

    /* Extract the full function name */
    char* funcname = func_name_from_symbol_string(symbol, execname);
    if (funcname == NULL)
      goto skip;
    if (should_skip_func_name(funcname)) {
      free(funcname);
      goto skip;
    }

#if 0
    /* Trim numeric suffix from caml functions */
    regmatch_t functrimmed = trim_func_name(symbol, &funcname);

    /* Use the trimmed caml name if available, otherwise use the full function
       name */
    const regmatch_t* match = (functrimmed.rm_so != -1) ?
      &functrimmed : &funcname;
#endif

    fprintf(stdout, "%s\n", funcname);
    fflush(stdout);

    /* Stop the backtrace at caml_program */
    bool stop = is_caml_entry_name(funcname);
    free(funcname);
    if (stop)
      break;

skip:
    free((void*)symbol);
    symbol = NULL;
  }

  if (symbol)
    free((void*)symbol);
}
