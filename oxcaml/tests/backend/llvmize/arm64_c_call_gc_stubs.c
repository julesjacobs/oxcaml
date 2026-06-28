#include <caml/mlvalues.h>

CAMLprim value caml_gc_compaction(value v);

CAMLprim value arm64_force_gc_and_sum(value a, value b, value c, value d) {
  caml_gc_compaction(Val_unit);
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c) + Long_val(d));
}
