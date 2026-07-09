/* Drop-in replacement for ocamlopt.opt that delegates to the warm fork-server,
 * failing OPEN to the real compiler on any problem. Named `ocamlopt.opt` and put
 * first on PATH; the build system needs no changes.
 *
 * Config via environment:
 *   OXFORK_REAL     (required) absolute path to the real ocamlopt.opt.
 *   OXFORK_SOCK     (required to delegate) daemon Unix socket path.
 *   OXFORK_HASH     (optional) expected compiler hash; "" => daemon skips check.
 *   OXFORK_TIMEOUT  (optional) reply timeout seconds, default 300; on timeout we
 *                   fail open. Bounds any hang on a wedged daemon.
 *   OXFORK_LOG      (optional) append one line per invocation: served/fallback.
 *
 * Delegation policy: only `-c` compiles are delegated (that is where the win is
 * and where identity is easiest to guarantee); link/pack/-config/-version and
 * anything else exec the real compiler directly. On daemon-absent, connect
 * error, hash/runparam mismatch, or reply timeout we ALSO exec the real
 * compiler. Net effect: never worse than stock, byte-identical on fallback.
 *
 * Wire format matches the hardened daemon: 4-byte big-endian length + NUL body
 *   "FSRV1"\0 hash\0 OCAMLRUNPARAM\0 cwd\0 argc\0 arg..\0 envc\0 env..\0
 * fds [0,1,2] sent as SCM_RIGHTS.
 * Reply: EXIT=<n> | HASHMISS=.. | RUNPARAMMISS=.. | OCAMLLIBMISS=.. ; any reply
 * other than EXIT= (a config-mismatch refusal or an unrecognized line) fails
 * open to the real compiler.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>

extern char **environ;

static void logline(const char *msg) {
    const char *lp = getenv("OXFORK_LOG");
    if (!lp || !*lp) return;
    FILE *f = fopen(lp, "a");
    if (f) { fprintf(f, "%s\n", msg); fclose(f); }
}

static void put_str(char **p, const char *s) {
    size_t n = strlen(s) + 1;
    memcpy(*p, s, n);
    *p += n;
}

/* exec the real compiler with argv[0] normalized to the real path (so output is
 * identical to a direct invocation). Never returns on success. */
static int fail_open(const char *real, char **argv, const char *why) {
    char buf[128];
    snprintf(buf, sizeof buf, "fallback:%s", why);
    logline(buf);
    argv[0] = (char *)real;
    execv(real, argv);
    execvp(real, argv);
    perror("oxfork shim: exec real compiler");
    return 127;
}

int main(int argc, char **argv) {
    const char *real = getenv("OXFORK_REAL");
    const char *sock = getenv("OXFORK_SOCK");
    const char *hash = getenv("OXFORK_HASH");
    if (!hash) hash = "";
    if (!real || !*real) {
        /* nothing to fall back to; try PATH-resolved ocamlopt.opt sibling */
        fprintf(stderr, "oxfork shim: OXFORK_REAL unset\n");
        return 2;
    }
    /* only delegate -c compiles */
    int is_c = 0;
    for (int i = 1; i < argc; i++)
        if (strcmp(argv[i], "-c") == 0) { is_c = 1; break; }
    if (!sock || !*sock || !is_c) return fail_open(real, argv, is_c ? "no-sock" : "not-c");

    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return fail_open(real, argv, "socket");
    struct sockaddr_un addr;
    memset(&addr, 0, sizeof addr);
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, sock, sizeof(addr.sun_path) - 1);
    if (connect(fd, (struct sockaddr *)&addr, sizeof addr) < 0) {
        close(fd);
        return fail_open(real, argv, "connect");
    }
    int tmo = 300;
    { const char *t = getenv("OXFORK_TIMEOUT"); if (t && *t) tmo = atoi(t); }
    struct timeval tv = { tmo, 0 };
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv);

    /* build body; argv[0] normalized to the real path for identity */
    const char *runparam = getenv("OCAMLRUNPARAM");
    if (!runparam) runparam = "";
    char cwd[4096];
    if (!getcwd(cwd, sizeof cwd)) strcpy(cwd, ".");
    size_t cap = 128 + strlen(hash) + strlen(runparam) + strlen(cwd) + strlen(real);
    for (int i = 1; i < argc; i++) cap += strlen(argv[i]) + 1;
    int envc = 0;
    for (char **e = environ; *e; e++) { cap += strlen(*e) + 1; envc++; }
    char *body = malloc(cap), *p = body;
    put_str(&p, "FSRV1");
    put_str(&p, hash);
    put_str(&p, runparam);
    put_str(&p, cwd);
    char num[32];
    snprintf(num, sizeof num, "%d", argc);
    put_str(&p, num);
    put_str(&p, real);                 /* argv[0] normalized */
    for (int i = 1; i < argc; i++) put_str(&p, argv[i]);
    snprintf(num, sizeof num, "%d", envc);
    put_str(&p, num);
    for (char **e = environ; *e; e++) put_str(&p, *e);
    size_t blen = (size_t)(p - body);

    unsigned char prefix[4] = { (unsigned char)((blen >> 24) & 0xff),
                                (unsigned char)((blen >> 16) & 0xff),
                                (unsigned char)((blen >> 8) & 0xff),
                                (unsigned char)(blen & 0xff) };
    struct msghdr msg;
    memset(&msg, 0, sizeof msg);
    struct iovec iov = { prefix, 4 };
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    char cbuf[CMSG_SPACE(sizeof(int) * 3)];
    memset(cbuf, 0, sizeof cbuf);
    msg.msg_control = cbuf;
    msg.msg_controllen = sizeof cbuf;
    struct cmsghdr *cm = CMSG_FIRSTHDR(&msg);
    cm->cmsg_level = SOL_SOCKET;
    cm->cmsg_type = SCM_RIGHTS;
    cm->cmsg_len = CMSG_LEN(sizeof(int) * 3);
    int fds3[3] = { 0, 1, 2 };
    memcpy(CMSG_DATA(cm), fds3, sizeof fds3);
    if (sendmsg(fd, &msg, 0) < 0) { free(body); close(fd); return fail_open(real, argv, "sendmsg"); }
    size_t off = 0;
    while (off < blen) {
        ssize_t w = write(fd, body + off, blen - off);
        if (w <= 0) { free(body); close(fd); return fail_open(real, argv, "write"); }
        off += (size_t)w;
    }
    free(body);

    char reply[256];
    ssize_t n = read(fd, reply, sizeof reply - 1);
    close(fd);
    if (n <= 0) return fail_open(real, argv, "reply-timeout");
    reply[n] = 0;
    if (strncmp(reply, "EXIT=", 5) == 0) { logline("served"); return atoi(reply + 5); }
    /* Config-mismatch refusals (hash/OCAMLRUNPARAM/OCAMLLIB) and anything else
     * all fail open to the real compiler. */
    if (strncmp(reply, "HASHMISS=", 9) == 0) return fail_open(real, argv, "hash-miss");
    if (strncmp(reply, "RUNPARAMMISS=", 13) == 0) return fail_open(real, argv, "runparam-miss");
    if (strncmp(reply, "OCAMLLIBMISS=", 13) == 0) return fail_open(real, argv, "ocamllib-miss");
    return fail_open(real, argv, "refused");
}
