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
#include "caml/custom.h"
#include <stdlib.h>

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

static value pref_alloc_step(value initial, mlsize_t fields)
{
  CAMLparam1(initial);
  CAMLlocal2(cell, result);
  cell = caml_pref_alloc(initial);
  result = caml_alloc_small(fields, 0);
  Field(result, 0) = cell;
  if (fields == 2) Field(result, 1) = Val_unit;
  CAMLreturn(result);
}

CAMLprim value caml_pref_alloc_step(value initial)
{
  return pref_alloc_step(initial, 1);
}

CAMLprim value caml_pref_alloc_step_bytecode(value initial, value token)
{
  return pref_alloc_step(initial, 2);
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

CAMLprim void caml_pref_split(value selection) { }
CAMLprim value caml_pref_split_bytecode(value selection, value token)
{
  CAMLparam0();
  CAMLlocal1(pair);
  pair = caml_alloc_small(2, 0);
  Field(pair, 0) = Val_unit;
  Field(pair, 1) = Val_unit;
  CAMLreturn(pair);
}
CAMLprim void caml_pref_join(void) { }
CAMLprim value caml_pref_join_bytecode(value left, value right)
{
  return Val_unit;
}
CAMLprim value caml_pref_heap_union(value left, value right) { return Val_unit; }
CAMLprim value caml_pref_heap_restrict(value heap, value selected)
{
  return Val_unit;
}
CAMLprim value caml_pref_heap_exclude(value heap, value selected)
{
  return Val_unit;
}

CAMLprim value caml_pref_heap_disjoint(value a, value b) { return Val_unit; }
CAMLprim value caml_pref_heap_same_domain(value a, value b) { return Val_unit; }
CAMLprim value caml_pref_heap_law2(value a, value b) { return Val_unit; }
CAMLprim value caml_pref_heap_law3(value a, value b, value c) { return Val_unit; }
CAMLprim value caml_pref_heap_law4(value a, value b, value c, value d)
{
  return Val_unit;
}
CAMLprim value caml_pref_heap_law5(value a, value b, value c, value d, value e)
{
  return Val_unit;
}

CAMLprim value caml_pref_ghost_split_bytecode(value selection, value token)
{
  return Val_unit;
}

/* Verified atomics retain an immutable identity and key. Proof arguments and
   permission results have no runtime authority representation. */
CAMLprim value caml_vox_atomic_create(value key, value initial)
{
  CAMLparam2(key, initial);
  CAMLlocal1(cell);
  uintnat id = pref_fresh_id();
  cell = caml_alloc_small(3, Object_tag);
  Field(cell, 0) = key;
  Field(cell, 1) = Val_long(id);
  Field(cell, 2) = initial;
  CAMLreturn(cell);
}

CAMLprim value caml_vox_atomic_create_bytecode(value key, value initial,
                                              value token)
{
  return caml_vox_atomic_create(key, initial);
}

CAMLprim value caml_vox_atomic_key(value cell)
{
  return Field(cell, 0);
}

CAMLprim value caml_vox_atomic_load(value cell, value post, value transition)
{
  CAMLparam1(cell);
  CAMLlocal1(result);
  result = caml_alloc_small(1, 0);
  Field(result, 0) = caml_atomic_load_field(cell, Val_long(2));
  CAMLreturn(result);
}

CAMLprim value caml_vox_atomic_load_bytecode(value cell, value post,
                                            value token, value transition)
{
  CAMLparam1(cell);
  CAMLlocal1(result);
  result = caml_alloc_small(2, 0);
  Field(result, 1) = Atom(0);
  Field(result, 0) = caml_atomic_load_field(cell, Val_long(2));
  CAMLreturn(result);
}

CAMLprim value caml_vox_atomic_cas(value cell, value expected, value desired,
                                  value post, value transition)
{
  CAMLparam3(cell, expected, desired);
  CAMLlocal1(result);
  result = caml_alloc_small(1, 0);
  Field(result, 0) =
    caml_atomic_cas_field(cell, Val_long(2), expected, desired);
  CAMLreturn(result);
}

CAMLprim value caml_vox_atomic_cas_bytecode(value *argv, int argn)
{
  CAMLparam0();
  CAMLlocal4(cell, expected, desired, result);
  cell = argv[0];
  expected = argv[1];
  desired = argv[2];
  result = caml_alloc_small(2, 0);
  Field(result, 1) = Atom(0);
  Field(result, 0) =
    caml_atomic_cas_field(cell, Val_long(2), expected, desired);
  CAMLreturn(result);
}

CAMLprim value caml_unique_cell_location(value cell) { return Val_unit; }

static value unique_cell_create(value initial, mlsize_t fields)
{
  CAMLparam1(initial);
  CAMLlocal2(cell, result);
  cell = caml_pref_alloc(initial);
  result = caml_alloc_small(fields, 0);
  Field(result, 0) = cell;
  if (fields == 2) Field(result, 1) = Atom(0);
  CAMLreturn(result);
}

CAMLprim value caml_unique_cell_create(value initial)
{
  return unique_cell_create(initial, 1);
}

CAMLprim value caml_unique_cell_create_bytecode(value initial, value token)
{
  return unique_cell_create(initial, 2);
}

static value unique_cell_replace(value cell, value initial, mlsize_t fields)
{
  CAMLparam2(cell, initial);
  CAMLlocal1(result);
  /* Allocate before moving the value, so allocation failure preserves the cell. */
  result = caml_alloc_small(fields, 0);
  Field(result, 0) = Field(cell, 2);
  if (fields == 2) Field(result, 1) = Atom(0);
  caml_modify(&Field(cell, 2), initial);
  CAMLreturn(result);
}

CAMLprim value caml_unique_cell_take(value cell)
{
  return unique_cell_replace(cell, Val_unit, 1);
}

CAMLprim value caml_unique_cell_take_bytecode(value cell, value token)
{
  return unique_cell_replace(cell, Val_unit, 2);
}

CAMLprim void caml_unique_cell_put(value cell, value initial)
{
  caml_modify(&Field(cell, 2), initial);
}

CAMLprim value caml_unique_cell_put_bytecode(value cell, value initial,
                                           value token)
{
  caml_unique_cell_put(cell, initial);
  return Val_unit;
}

CAMLprim value caml_unique_cell_replace(value cell, value initial)
{
  return unique_cell_replace(cell, initial, 1);
}

CAMLprim value caml_unique_cell_replace_bytecode(value cell, value initial,
                                               value token)
{
  return unique_cell_replace(cell, initial, 2);
}

/* The descriptor survives free and keeps allocation identity independent of
   address reuse. Its custom payload is unscanned and has no finalizer. */
static struct custom_operations raw_memory_ops = {
  "vox.raw_memory",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

#define Raw_memory_data(p) \
  (*((unsigned char **) Data_custom_val(Field(p, 2))))

static value raw_memory_malloc(value size, mlsize_t fields)
{
  CAMLparam1(size);
  CAMLlocal4(carrier, handle, some, result);
  uintnat id = pref_fresh_id();
  carrier = caml_alloc_custom(&raw_memory_ops, sizeof(unsigned char *), 0, 1);
  *((unsigned char **) Data_custom_val(carrier)) = NULL;
  handle = caml_alloc_small(3, Object_tag);
  Field(handle, 0) = size;
  Field(handle, 1) = Val_long(id);
  Field(handle, 2) = carrier;
  some = caml_alloc_small(1, 0);
  Field(some, 0) = handle;
  result = caml_alloc_small(fields, 0);
  Field(result, 0) = Val_none;
  if (fields == 2) Field(result, 1) = Val_unit;
  /* No managed allocations or raising operations after acquiring storage. */
  unsigned char *data = malloc(Long_val(size) == 0 ? 1 : Long_val(size));
  if (data != NULL) {
    Raw_memory_data(handle) = data;
    Field(result, 0) = some;
  }
  CAMLreturn(result);
}

CAMLprim value caml_raw_memory_malloc(value size)
{
  return raw_memory_malloc(size, 1);
}

CAMLprim value caml_raw_memory_malloc_bytecode(value size, value token)
{
  return raw_memory_malloc(size, 2);
}

CAMLprim value caml_raw_memory_length(value handle)
{
  return Field(handle, 0);
}

CAMLprim value caml_raw_memory_read(value handle, value index)
{
  return Val_long(Raw_memory_data(handle)[Long_val(index)]);
}

CAMLprim value caml_raw_memory_read_bytecode(value handle, value index,
                                           value token)
{
  return caml_raw_memory_read(handle, index);
}

CAMLprim void caml_raw_memory_write(value handle, value index, value byte)
{
  Raw_memory_data(handle)[Long_val(index)] = Long_val(byte);
}

CAMLprim value caml_raw_memory_write_bytecode(value handle, value index,
                                            value byte, value token)
{
  caml_raw_memory_write(handle, index, byte);
  return Val_unit;
}

CAMLprim void caml_raw_memory_free(value handle)
{
  free(Raw_memory_data(handle));
}

CAMLprim value caml_raw_memory_free_bytecode(value handle, value token)
{
  caml_raw_memory_free(handle);
  return Val_unit;
}

CAMLprim value caml_raw_memory_location(value handle, value index)
{
  return Val_unit;
}
