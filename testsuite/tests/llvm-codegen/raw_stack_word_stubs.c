#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/domain_state.h>
#include <caml/fiber.h>
#include <caml/mlvalues.h>

CAMLprim intnat raw_stack_base_native(value unit)
{
  return (intnat)Stack_base(Caml_state->current_stack);
}

CAMLprim intnat raw_stack_high_native(value unit)
{
  return (intnat)Stack_high(Caml_state->current_stack);
}

CAMLprim value raw_stack_base_bytecode(value unit)
{
  return caml_copy_nativeint((intnat)Stack_base(Caml_state->current_stack));
}

CAMLprim value raw_stack_high_bytecode(value unit)
{
  return caml_copy_nativeint((intnat)Stack_high(Caml_state->current_stack));
}
