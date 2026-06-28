#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef CAML_RUNTIME_5
#include <caml/shared_heap.h>
#endif

struct block {
  value header;
  value v;
};

#define Block_val(v) ((struct block *)&((value *)v)[-1])
#define Val_block(b) ((value)&(b)->v)

CAMLprim value arm64_global_root_register(value v)
{
  CAMLparam1(v);
  struct block *b = caml_stat_alloc(sizeof(struct block));
#ifdef CAML_RUNTIME_5
  b->header = Make_header(1, 0, NOT_MARKABLE);
#else
  b->header = Make_header(1, 0, Caml_black);
#endif
  b->v = v;
  caml_register_generational_global_root(&b->v);
  CAMLreturn(Val_block(b));
}

CAMLprim value arm64_global_root_get(value vblock)
{
  return Block_val(vblock)->v;
}

CAMLprim value arm64_global_root_set(value vblock, value v)
{
  CAMLparam2(vblock, v);
  caml_modify_generational_global_root(&Block_val(vblock)->v, v);
  CAMLreturn(Val_unit);
}

CAMLprim value arm64_global_root_remove(value vblock)
{
  CAMLparam1(vblock);
  caml_remove_generational_global_root(&Block_val(vblock)->v);
  CAMLreturn(Val_unit);
}
