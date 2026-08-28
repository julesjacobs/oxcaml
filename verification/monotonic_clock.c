#include <stdint.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

extern uint64_t caml_time_counter(void);

CAMLprim value caml_vox_smt_monotonic_time(value unit)
{
  (void)unit;
  return caml_copy_double((double)caml_time_counter() / 1e9);
}
