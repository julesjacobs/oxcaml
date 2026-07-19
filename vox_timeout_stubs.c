#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <limits.h>

#if defined(__GNUC__) && !defined(_WIN32)
extern value caml_with_async_exns(value) __attribute__((weak));
#endif

CAMLprim value caml_vox_with_async_exns(value function)
{
  CAMLparam1(function);
#if defined(__GNUC__) && !defined(_WIN32)
  if (caml_with_async_exns != NULL)
    CAMLreturn(caml_with_async_exns(function));
#endif
  caml_failwith("OxCaml asynchronous-exception boundary unavailable");
}

#if !defined(_WIN32)
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#endif

CAMLprim value caml_vox_sigalrm_is_blocked(value unit)
{
#if defined(_WIN32)
  (void)unit;
  return Val_int(-1);
#else
  sigset_t current;
  int was_blocked;
  int status;

  (void)unit;
  status = pthread_sigmask(SIG_BLOCK, NULL, &current);
  if (status != 0) return Val_int(-1);
  was_blocked = sigismember(&current, SIGALRM);
  if (was_blocked < 0) return Val_int(-1);
  return Val_int(was_blocked);
#endif
}

CAMLprim value caml_vox_restore_sigalrm(value was_blocked)
{
#if defined(_WIN32)
  (void)was_blocked;
  return Val_false;
#else
  sigset_t sigalrm;
  int how = Bool_val(was_blocked) ? SIG_BLOCK : SIG_UNBLOCK;

  if (sigemptyset(&sigalrm) != 0 || sigaddset(&sigalrm, SIGALRM) != 0)
    return Val_false;
  return Val_bool(pthread_sigmask(how, &sigalrm, NULL) == 0);
#endif
}

CAMLprim value caml_vox_set_alarm(value seconds)
{
#if defined(_WIN32)
  (void)seconds;
  return Val_int(-1);
#else
  intnat requested = Long_val(seconds);
  if (requested < 0 || (uintnat)requested > UINT_MAX) return Val_int(-1);
  return Val_int(alarm((unsigned int)requested));
#endif
}
