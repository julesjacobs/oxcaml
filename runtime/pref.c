/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Copyright 2026 Jules Jacobs                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of     */
/*   the GNU Lesser General Public License version 2.1, with the           */
/*   special exception on linking described in the file LICENSE.           */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/camlatomic.h"
#include "caml/fail.h"

/* Object identity keeps polymorphic comparison and hashing independent of
   the payload, while the GC scans the payload and collects pref cycles. */
static atomic_uintnat pref_next_id;

static uintnat pref_fresh_id(void)
{
  uintnat id = atomic_load(&pref_next_id);
  do {
    if (id == (uintnat) Max_long) caml_raise_out_of_memory();
  } while (!atomic_compare_exchange_weak(&pref_next_id, &id, id + 1));
  return id;
}

CAMLprim value caml_pref_alloc(value initial)
{
  CAMLparam1(initial);
  CAMLlocal1(cell);
  uintnat id = pref_fresh_id();
  cell = caml_alloc_small(3, Object_tag);
  Field(cell, 0) = Val_unit;
  Field(cell, 1) = Val_long(id);
  Field(cell, 2) = initial;
  CAMLreturn(cell);
}

CAMLprim void caml_pref_empty(value unit) { }
CAMLprim value caml_pref_empty_bytecode(value unit) { return Val_unit; }

CAMLprim value caml_pref_read(value cell) { return Field(cell, 2); }
CAMLprim value caml_pref_read_bytecode(value cell, value token)
{
  return caml_pref_read(cell);
}

CAMLprim void caml_pref_write(value cell, value element)
{
  caml_modify(&Field(cell, 2), element);
}
CAMLprim value caml_pref_write_bytecode(value cell, value element, value token)
{
  caml_pref_write(cell, element);
  return Val_unit;
}

CAMLprim void caml_pref_attach(value cell, value initial) { }
CAMLprim value caml_pref_attach_bytecode(value cell, value initial, value token)
{
  return Val_unit;
}

CAMLprim value caml_pref_own(void) { return Val_unit; }
CAMLprim value caml_pref_own_bytecode(value token) { return Val_unit; }
CAMLprim value caml_pref_heap_empty(value unit) { return Val_unit; }
CAMLprim value caml_pref_heap_mem(value heap, value cell) { return Val_unit; }
CAMLprim value caml_pref_heap_at(value heap, value cell) { return Val_unit; }
CAMLprim value caml_pref_heap_put(value heap, value cell, value initial)
{
  return Val_unit;
}
