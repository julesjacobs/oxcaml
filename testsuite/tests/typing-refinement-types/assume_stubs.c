#include <caml/mlvalues.h>

static int calls;

CAMLprim value caml_assume_counted(value x)
{
  calls++;
  return Val_bool(Long_val(x) == 0);
}

CAMLprim value caml_assume_predicate_calls(value unit)
{
  return Val_int(calls);
}
