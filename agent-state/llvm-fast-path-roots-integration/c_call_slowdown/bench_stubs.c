#include <caml/mlvalues.h>

CAMLprim value bench_noalloc_add1(value x)
{
  return x + 2;
}

CAMLprim value bench_noalloc_sum10(value a0, value a1, value a2, value a3,
                                   value a4, value a5, value a6, value a7,
                                   value a8, value a9)
{
  intnat sum = Long_val(a0) + Long_val(a1) + Long_val(a2) + Long_val(a3)
               + Long_val(a4) + Long_val(a5) + Long_val(a6) + Long_val(a7)
               + Long_val(a8) + Long_val(a9);
  return Val_long(sum);
}

CAMLprim value bench_noalloc_sum10_byte(value *argv, int argn)
{
  (void)argn;
  return bench_noalloc_sum10(argv[0], argv[1], argv[2], argv[3], argv[4],
                             argv[5], argv[6], argv[7], argv[8], argv[9]);
}
