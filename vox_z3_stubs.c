#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)

CAMLprim value caml_vox_z3_run_persistent(value command, value timeout,
                                           value contents)
{
  CAMLparam3(command, timeout, contents);
  CAMLlocal1(result);
  (void)command;
  (void)timeout;
  (void)contents;
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(-1));
  Store_field(result, 1, caml_copy_string("persistent solver unavailable"));
  CAMLreturn(result);
}

#else

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define VOX_Z3_MAX_OUTPUT (16u * 1024u * 1024u)

struct byte_buffer {
  char *data;
  size_t length;
  size_t capacity;
};

struct solver_session {
  pid_t pid;
  int input;
  int output;
  char *command;
  uint64_t query_id;
};

static struct solver_session session = {-1, -1, -1, NULL, 0};
static int cleanup_registered = 0;

static void buffer_free(struct byte_buffer *buffer)
{
  free(buffer->data);
  buffer->data = NULL;
  buffer->length = 0;
  buffer->capacity = 0;
}

static int buffer_append(struct byte_buffer *buffer, const char *data,
                         size_t length)
{
  size_t required;
  size_t capacity;
  char *grown;

  if (length > VOX_Z3_MAX_OUTPUT - buffer->length) return 0;
  required = buffer->length + length + 1;
  if (required > buffer->capacity) {
    capacity = buffer->capacity == 0 ? 4096 : buffer->capacity;
    while (capacity < required) {
      if (capacity > VOX_Z3_MAX_OUTPUT / 2) {
        capacity = VOX_Z3_MAX_OUTPUT + 1;
        break;
      }
      capacity *= 2;
    }
    grown = realloc(buffer->data, capacity);
    if (grown == NULL) return 0;
    buffer->data = grown;
    buffer->capacity = capacity;
  }
  memcpy(buffer->data + buffer->length, data, length);
  buffer->length += length;
  buffer->data[buffer->length] = '\0';
  return 1;
}

static int buffer_has_non_whitespace(const struct byte_buffer *buffer)
{
  size_t index;
  for (index = 0; index < buffer->length; index++) {
    char character = buffer->data[index];
    if (character != ' ' && character != '\t' && character != '\r'
        && character != '\n')
      return 1;
  }
  return 0;
}

static int64_t monotonic_milliseconds(void)
{
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1;
  return (int64_t)now.tv_sec * 1000 + now.tv_nsec / 1000000;
}

static void close_descriptor(int *descriptor)
{
  if (*descriptor >= 0) close(*descriptor);
  *descriptor = -1;
}

/* The solver has just been sent SIGTERM and takes microseconds to go, but
   never quite fast enough for the first non-blocking reap, so a flat 100 ms
   between attempts was 100 ms of sleeping on every compilation that opened a
   session -- and the sleep was spent waiting for a child that had already
   exited. Start a thousand times shorter and back off, which reaps the usual
   case on the first pause and keeps the same two-second budget before
   escalating to SIGKILL. */
#define WAIT_INITIAL_MICROSECONDS 200
#define WAIT_MAXIMUM_MICROSECONDS 100000
#define WAIT_ATTEMPTS 29

static void wait_briefly(pid_t pid)
{
  int attempts;
  int status;
  useconds_t pause = WAIT_INITIAL_MICROSECONDS;
  for (attempts = 0; attempts < WAIT_ATTEMPTS; attempts++) {
    pid_t waited = waitpid(pid, &status, WNOHANG);
    if (waited == pid || (waited < 0 && errno == ECHILD)) return;
    usleep(pause);
    pause *= 2;
    if (pause > WAIT_MAXIMUM_MICROSECONDS) pause = WAIT_MAXIMUM_MICROSECONDS;
  }
  kill(-pid, SIGKILL);
  kill(pid, SIGKILL);
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
}

static void close_session(void)
{
  pid_t pid = session.pid;
  close_descriptor(&session.input);
  close_descriptor(&session.output);
  session.pid = -1;
  free(session.command);
  session.command = NULL;
  if (pid > 0) {
    kill(-pid, SIGTERM);
    kill(pid, SIGTERM);
    wait_briefly(pid);
  }
}

static void close_session_at_exit(void)
{
  close_session();
}

static int set_nonblocking(int descriptor)
{
  int flags = fcntl(descriptor, F_GETFL, 0);
  return flags >= 0 && fcntl(descriptor, F_SETFL, flags | O_NONBLOCK) == 0;
}

static int set_close_on_exec(int descriptor)
{
  int flags = fcntl(descriptor, F_GETFD, 0);
  return flags >= 0 && fcntl(descriptor, F_SETFD, flags | FD_CLOEXEC) == 0;
}

static int spawn_session(const char *command)
{
  int input_pipe[2];
  int output_pipe[2];
  pid_t pid;

  close_session();
  if (pipe(input_pipe) != 0) return 0;
  if (pipe(output_pipe) != 0) {
    close(input_pipe[0]);
    close(input_pipe[1]);
    return 0;
  }
  pid = fork();
  if (pid == 0) {
    setpgid(0, 0);
    dup2(input_pipe[0], STDIN_FILENO);
    dup2(output_pipe[1], STDOUT_FILENO);
    dup2(output_pipe[1], STDERR_FILENO);
    close(input_pipe[0]);
    close(input_pipe[1]);
    close(output_pipe[0]);
    close(output_pipe[1]);
    execl("/bin/sh", "sh", "-c", command, (char *)NULL);
    _exit(127);
  }
  close(input_pipe[0]);
  close(output_pipe[1]);
  if (pid < 0) {
    close(input_pipe[1]);
    close(output_pipe[0]);
    return 0;
  }
  setpgid(pid, pid);
  if (!set_nonblocking(input_pipe[1]) || !set_nonblocking(output_pipe[0])
      || !set_close_on_exec(input_pipe[1])
      || !set_close_on_exec(output_pipe[0])) {
    session.pid = pid;
    session.input = input_pipe[1];
    session.output = output_pipe[0];
    close_session();
    return 0;
  }
  session.pid = pid;
  session.input = input_pipe[1];
  session.output = output_pipe[0];
  session.command = strdup(command);
  if (session.command == NULL) {
    close_session();
    return 0;
  }
  return 1;
}

static ssize_t write_without_sigpipe(int descriptor, const void *contents,
                                     size_t length)
{
  sigset_t blocked;
  sigset_t previous;
  sigset_t pending;
  int was_pending = 0;
  ssize_t result;
  int saved_errno;

  if (sigemptyset(&blocked) != 0 || sigaddset(&blocked, SIGPIPE) != 0
      || pthread_sigmask(SIG_BLOCK, &blocked, &previous) != 0)
    return write(descriptor, contents, length);
  if (sigpending(&pending) == 0)
    was_pending = sigismember(&pending, SIGPIPE) == 1;
  result = write(descriptor, contents, length);
  saved_errno = errno;
  if (result < 0 && saved_errno == EPIPE && !was_pending) {
    int received;
    if (sigpending(&pending) == 0 && sigismember(&pending, SIGPIPE) == 1)
      (void)sigwait(&blocked, &received);
  }
  pthread_sigmask(SIG_SETMASK, &previous, NULL);
  errno = saved_errno;
  return result;
}

static int timed_write(const char *contents, size_t length, int64_t deadline)
{
  size_t written = 0;
  while (written < length) {
    ssize_t count =
      write_without_sigpipe(session.input, contents + written,
                            length - written);
    if (count > 0) {
      written += (size_t)count;
    } else if (count < 0 && errno == EINTR) {
      return 130;
    } else if (count < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      struct pollfd descriptor = {session.input, POLLOUT, 0};
      int64_t now = monotonic_milliseconds();
      int remaining = now < 0 ? 0 : (int)(deadline - now);
      int result;
      if (remaining <= 0) return 124;
      result = poll(&descriptor, 1, remaining);
      if (result == 0) return 124;
      if (result < 0 && errno == EINTR) return 130;
      if (result < 0) return 125;
    } else {
      return 125;
    }
  }
  return 0;
}

static int wait_status(pid_t pid)
{
  int status;
  pid_t waited;
  do {
    waited = waitpid(pid, &status, WNOHANG);
  } while (waited < 0 && errno == EINTR);
  if (waited == 0) return 125;
  if (waited < 0) {
    if (errno == ECHILD && session.pid == pid) session.pid = -1;
    return errno == ECHILD ? 125 : 126;
  }
  if (session.pid == pid) session.pid = -1;
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
  return 125;
}

static int contains_marker(const struct byte_buffer *output,
                           const char *marker, size_t *position)
{
  size_t marker_length = strlen(marker);
  size_t index;
  if (marker_length == 0 || output->length < marker_length) return 0;
  for (index = 0; index + marker_length <= output->length; index++) {
    int starts_line = index == 0 || output->data[index - 1] == '\n';
    size_t after = index + marker_length;
    int ends_line =
      after < output->length
      && (output->data[after] == '\n' || output->data[after] == '\r');
    if (starts_line && ends_line
        && memcmp(output->data + index, marker, marker_length) == 0) {
      *position = index;
      return 1;
    }
  }
  return 0;
}

static int read_until_marker(const char *marker, int64_t deadline,
                             struct byte_buffer *combined)
{
  struct byte_buffer standard = {NULL, 0, 0};
  char chunk[4096];
  int output_open = 1;
  int result = 125;

  while (output_open) {
    struct pollfd descriptor;
    int64_t now = monotonic_milliseconds();
    int remaining = now < 0 ? 0 : (int)(deadline - now);
    int polled;
    size_t marker_position;

    if (contains_marker(&standard, marker, &marker_position)) {
      if (!buffer_append(combined, standard.data, marker_position)) result = 125;
      else result = 0;
      goto done;
    }
    if (remaining <= 0) {
      result = 124;
      goto done;
    }
    descriptor.fd = session.output;
    descriptor.events = POLLIN | POLLHUP;
    descriptor.revents = 0;
    polled = poll(&descriptor, 1, remaining);
    if (polled == 0) {
      result = 124;
      goto done;
    }
    if (polled < 0) {
      result = errno == EINTR ? 130 : 125;
      goto done;
    }
    if (descriptor.revents & (POLLIN | POLLHUP)) {
      ssize_t read_count = read(session.output, chunk, sizeof(chunk));
      if (read_count > 0) {
        if (!buffer_append(&standard, chunk, (size_t)read_count)) {
          result = 125;
          goto done;
        }
      } else if (read_count == 0) {
        output_open = 0;
      } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
        if (errno == EINTR) result = 130;
        else result = 125;
        goto done;
      }
    }
  }
  if (!buffer_append(combined, standard.data, standard.length)) result = 125;
  else {
    result = wait_status(session.pid);
    if (result == 0) result = 126;
  }

done:
  buffer_free(&standard);
  return result;
}

static int run_framed(const char *contents, int timeout_seconds,
                      struct byte_buffer *output)
{
  static const char frame_prefix[] =
    "(push 1)\n"
    "(declare-const __vox2_z3_scope_probe Int)\n";
  struct byte_buffer framed = {NULL, 0, 0};
  char marker[96];
  char marker_command[128];
  int64_t deadline;
  int status;

  session.query_id++;
  snprintf(marker, sizeof(marker), "__vox2_z3_query_%llu__",
           (unsigned long long)session.query_id);
  snprintf(marker_command, sizeof(marker_command),
           "\n(pop 1)\n(echo \"%s\")\n", marker);
  if (!buffer_append(&framed, frame_prefix, sizeof(frame_prefix) - 1)
      || !buffer_append(&framed, contents, strlen(contents))
      || !buffer_append(&framed, marker_command, strlen(marker_command))) {
    buffer_free(&framed);
    return 125;
  }
  deadline = monotonic_milliseconds();
  if (deadline < 0) {
    buffer_free(&framed);
    return 125;
  }
  deadline += (int64_t)timeout_seconds * 1000;
  status = timed_write(framed.data, framed.length, deadline);
  buffer_free(&framed);
  if (status != 0) return status;
  return read_until_marker(marker, deadline, output);
}

static int probe_session(int timeout_seconds)
{
  struct byte_buffer output = {NULL, 0, 0};
  int64_t deadline = monotonic_milliseconds();
  int probe_timeout = timeout_seconds < 2 ? timeout_seconds : 2;
  const char *command =
    "(set-option :produce-unsat-cores true)\n"
    "(push 1)\n"
    "(pop 1)\n"
    "(echo \"__vox2_z3_ready__\")\n";
  int status;
  if (deadline < 0) return -1;
  deadline += (int64_t)probe_timeout * 1000;
  status = timed_write(command, strlen(command), deadline);
  if (status == 0)
    status = read_until_marker("__vox2_z3_ready__", deadline, &output);
  if (status == 0 && buffer_has_non_whitespace(&output)) status = -1;
  buffer_free(&output);
  if (status == 0) return 0;
  if (status == 127) return 127;
  return -1;
}

static int ensure_session(const char *command, int timeout_seconds)
{
  int status;
  if (session.pid > 0 && session.command != NULL
      && strcmp(session.command, command) == 0)
    return 0;
  if (!spawn_session(command)) return 125;
  status = probe_session(timeout_seconds);
  if (status != 0) close_session();
  return status;
}

static int run_persistent(const char *command, int timeout_seconds,
                          const char *contents, struct byte_buffer *output)
{
  int attempt;
  int status = 125;
  for (attempt = 0; attempt < 2; attempt++) {
    status = ensure_session(command, timeout_seconds);
    if (status != 0) return status;
    status = run_framed(contents, timeout_seconds, output);
    if (status == 0) return 0;
    close_session();
    if (status == 124 || status == 127 || status == 130) return status;
    if (attempt == 0) {
      buffer_free(output);
      continue;
    }
  }
  return status;
}

CAMLprim value caml_vox_z3_run_persistent(value command_value,
                                           value timeout_value,
                                           value contents_value)
{
  CAMLparam3(command_value, timeout_value, contents_value);
  CAMLlocal2(result, output_value);
  struct byte_buffer output = {NULL, 0, 0};
  char *command = strdup(String_val(command_value));
  char *contents = strdup(String_val(contents_value));
  int timeout_seconds = Int_val(timeout_value);
  int status;

  if (!cleanup_registered) {
    atexit(close_session_at_exit);
    cleanup_registered = 1;
  }
  if (command == NULL || contents == NULL) {
    free(command);
    free(contents);
    status = 125;
  } else {
    status = run_persistent(command, timeout_seconds, contents, &output);
    free(command);
    free(contents);
  }
  output_value = caml_alloc_initialized_string(output.length,
                                                output.data == NULL
                                                  ? "" : output.data);
  buffer_free(&output);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(status));
  Store_field(result, 1, output_value);
  CAMLreturn(result);
}

#endif
