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
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

CAMLextern value caml_array_get(value, value);
CAMLextern value caml_array_set(value, value, value);
CAMLextern value caml_array_sub(value, value, value);
CAMLextern value caml_bigint_of_int(value);

/* Every handle is heap allocated, including handles whose public type is
   local to a borrow scope. Frames retain storage but expose no access to it. */
static value borrow_handle(value base, intnat offset, intnat length)
{
  CAMLparam1(base);
  CAMLlocal1(handle);
  handle = caml_alloc_small(3, 0);
  Field(handle, 0) = base;
  Field(handle, 1) = Val_long(offset);
  Field(handle, 2) = Val_long(length);
  CAMLreturn(handle);
}

static value borrow_pair(value first, value second)
{
  CAMLparam2(first, second);
  CAMLlocal1(pair);
  pair = caml_alloc_small(2, 0);
  Field(pair, 0) = first;
  Field(pair, 1) = second;
  CAMLreturn(pair);
}

CAMLprim value caml_borrow_of_iarray(value array)
{
  CAMLparam1(array);
  CAMLlocal1(copy);
  intnat length = caml_array_length(array);
  copy = caml_array_sub(array, Val_long(0), Val_long(length));
  CAMLreturn(borrow_handle(copy, 0, length));
}

CAMLprim value caml_borrow_into_iarray(value owner)
{
  return Field(owner, 0);
}

CAMLprim value caml_borrow_open(value owner)
{
  return borrow_pair(owner, owner);
}

CAMLprim value caml_borrow_restore(value frame)
{
  return frame;
}

CAMLprim value caml_borrow_length(value loan)
{
  return Field(loan, 2);
}

static intnat borrow_index(value loan, value index)
{
  intnat i = Long_val(index);
  if (i < 0 || i >= Long_val(Field(loan, 2)))
    caml_invalid_argument("Borrow.Slice: index out of bounds");
  return Long_val(Field(loan, 1)) + i;
}

CAMLprim value caml_borrow_get(value loan, value index)
{
  CAMLparam2(loan, index);
  CAMLlocal1(element);
  intnat i = borrow_index(loan, index);
  element = caml_array_get(Field(loan, 0), Val_long(i));
  CAMLreturn(element);
}

CAMLprim value caml_borrow_set(value loan, value index, value element)
{
  CAMLparam3(loan, index, element);
  intnat i = borrow_index(loan, index);
  caml_array_set(Field(loan, 0), Val_long(i), element);
  CAMLreturn(loan);
}

CAMLprim value caml_borrow_snapshot(value loan)
{
  CAMLparam1(loan);
  CAMLlocal1(copy);
  copy = caml_array_sub(Field(loan, 0), Field(loan, 1), Field(loan, 2));
  CAMLreturn(copy);
}

CAMLprim value caml_borrow_split(value loan, value index)
{
  CAMLparam2(loan, index);
  CAMLlocal3(left, right, result);
  intnat k = Long_val(index);
  intnat offset = Long_val(Field(loan, 1));
  intnat length = Long_val(Field(loan, 2));
  if (k < 0 || k > length)
    caml_invalid_argument("Borrow.Slice.split_at: index out of bounds");
  left = borrow_handle(Field(loan, 0), offset, k);
  right = borrow_handle(Field(loan, 0), offset + k, length - k);
  result = caml_alloc_small(3, 0);
  Field(result, 0) = loan;
  Field(result, 1) = left;
  Field(result, 2) = right;
  CAMLreturn(result);
}

CAMLprim value caml_borrow_recombine(value frame)
{
  return frame;
}

CAMLprim value caml_borrow_finish(value loan)
{
  return Val_unit;
}

/* Private scoped task transfer. The enclosing fork/join must join before
   returning or raising; this primitive never globalizes an arbitrary closure. */
CAMLprim value caml_borrow_transfer(value loan)
{
  return loan;
}

CAMLprim value caml_borrow_contents(value owner) { return Val_unit; }
CAMLprim value caml_borrow_current(value loan) { return Val_unit; }
CAMLprim value caml_borrow_final(value loan) { return Val_unit; }
CAMLprim value caml_borrow_frame_final(value frame) { return Val_unit; }
CAMLprim value caml_borrow_frame_left(value frame) { return Val_unit; }
CAMLprim value caml_borrow_frame_right(value frame) { return Val_unit; }

CAMLprim value caml_vox_sequence_length(value sequence)
{
  intnat length = 0;
  while (sequence != Val_emptylist) {
    length++;
    sequence = Field(sequence, 1);
  }
  return caml_bigint_of_int(Val_long(length));
}
