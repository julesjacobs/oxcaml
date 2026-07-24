#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <stdio.h>

#if !defined(_WIN32)
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

CAMLprim value caml_vox_cache_directory_is_private(value path_value)
{
#if defined(_WIN32)
  (void)path_value;
  return Val_false;
#else
  const char *path = String_val(path_value);
  size_t length = strlen(path);
  uid_t owner = geteuid();
  char *prefix;
  size_t index;
  struct stat status;
  int secure = 1;

  if (length == 0 || path[0] != '/') return Val_false;
  prefix = malloc(length + 1);
  if (prefix == NULL) return Val_false;
  memcpy(prefix, path, length + 1);

  for (index = 1; index <= length && secure; index++) {
    if (index == length || prefix[index] == '/') {
      char saved = prefix[index];
      prefix[index] = '\0';
      if (lstat(prefix, &status) != 0 || !S_ISDIR(status.st_mode)
          || ((status.st_mode & 0022) != 0
              && (status.st_mode & S_ISVTX) == 0))
        secure = 0;
      prefix[index] = saved;
    }
  }

  if (secure
      && (lstat(path, &status) != 0 || status.st_uid != owner
          || !S_ISDIR(status.st_mode) || (status.st_mode & 0777) != 0700))
    secure = 0;

  free(prefix);
  return Val_bool(secure);
#endif
}

CAMLprim value caml_vox_cache_file_is_private(value path_value)
{
#if defined(_WIN32)
  (void)path_value;
  return Val_false;
#else
  struct stat status;
  const char *path = String_val(path_value);
  if (lstat(path, &status) != 0) return Val_false;
  return Val_bool(status.st_uid == geteuid() && S_ISREG(status.st_mode)
                  && (status.st_mode & 0777) == 0600 && status.st_nlink == 1);
#endif
}

CAMLprim value caml_vox_file_is_executable(value path_value)
{
#if defined(_WIN32)
  (void)path_value;
  return Val_false;
#else
  struct stat status;
  const char *path = String_val(path_value);
  if (stat(path, &status) != 0) return Val_false;
  return Val_bool(S_ISREG(status.st_mode) && access(path, X_OK) == 0);
#endif
}

CAMLprim value caml_vox_unset_environment_variable(value name_value)
{
#if defined(_WIN32)
  return Val_bool(_putenv_s(String_val(name_value), "") == 0);
#else
  return Val_bool(unsetenv(String_val(name_value)) == 0);
#endif
}

CAMLprim value caml_vox_file_stamp(value path_value)
{
  CAMLparam1(path_value);
#if defined(_WIN32)
  CAMLreturn(caml_copy_string(""));
#else
  struct stat status;
  char stamp[256];
  int length;
  if (stat(String_val(path_value), &status) != 0)
    CAMLreturn(caml_copy_string(""));
#if defined(__APPLE__)
  length = snprintf(stamp, sizeof(stamp), "%llu:%llu:%llu:%lld:%ld:%lld:%ld",
                    (unsigned long long)status.st_dev,
                    (unsigned long long)status.st_ino,
                    (unsigned long long)status.st_size,
                    (long long)status.st_mtimespec.tv_sec,
                    status.st_mtimespec.tv_nsec,
                    (long long)status.st_ctimespec.tv_sec,
                    status.st_ctimespec.tv_nsec);
#else
  length = snprintf(stamp, sizeof(stamp), "%llu:%llu:%llu:%lld:%ld:%lld:%ld",
                    (unsigned long long)status.st_dev,
                    (unsigned long long)status.st_ino,
                    (unsigned long long)status.st_size,
                    (long long)status.st_mtim.tv_sec,
                    status.st_mtim.tv_nsec,
                    (long long)status.st_ctim.tv_sec,
                    status.st_ctim.tv_nsec);
#endif
  if (length < 0 || (size_t)length >= sizeof(stamp))
    CAMLreturn(caml_copy_string(""));
  CAMLreturn(caml_copy_string(stamp));
#endif
}
