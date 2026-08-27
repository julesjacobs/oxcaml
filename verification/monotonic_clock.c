#include <time.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

CAMLprim value caml_vox_smt_monotonic_time(value unit)
{
  struct timespec now;
  (void)unit;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0)
    caml_failwith("Vox_smt_solver: monotonic clock unavailable");
  return caml_copy_double((double)now.tv_sec + (double)now.tv_nsec / 1e9);
}
