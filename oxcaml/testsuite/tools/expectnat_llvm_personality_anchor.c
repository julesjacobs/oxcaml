#include <caml/mlvalues.h>

extern void caml_llvm_eh_personality(void);

/* Keep an undefined reference to the LLVM EH personality in this object.
   Native toplevel test fragments are loaded with dlopen and may refer to this
   symbol, but expectnat otherwise has no static reference that pulls it out of
   libasmrun. */
static void (*const caml_expectnat_llvm_personality_anchor_symbol)(void)
  __attribute__((used)) = caml_llvm_eh_personality;

CAMLprim value caml_expectnat_llvm_personality_anchor(value unit)
{
  (void)unit;
  return Val_unit;
}
