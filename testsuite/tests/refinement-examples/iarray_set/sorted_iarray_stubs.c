#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value vox_sorted_iarray_empty(value unit)
{
  (void)unit;
  return Atom(0);
}

static mlsize_t lower_bound(value array, intnat query)
{
  mlsize_t low = 0;
  mlsize_t high = Wosize_val(array);

  /* The interval is always [low, high), so every Field access is in bounds. */
  while (low < high) {
    mlsize_t middle = low + ((high - low) / 2);
    intnat candidate = Long_val(Field(array, middle));
    if (candidate < query) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  return low;
}

CAMLprim value vox_sorted_iarray_member(value query_value, value array)
{
#ifdef VOX_WRONG_MEMBER
  (void)query_value;
  (void)array;
  return Val_false;
#else
  intnat query = Long_val(query_value);
  mlsize_t index = lower_bound(array, query);
  mlsize_t length = Wosize_val(array);
  return Val_bool(index < length && Long_val(Field(array, index)) == query);
#endif
}

CAMLprim value vox_sorted_iarray_insert(value inserted_value, value array)
{
  CAMLparam2(inserted_value, array);
  CAMLlocal1(result);
  intnat inserted = Long_val(inserted_value);
  mlsize_t length = Wosize_val(array);
  mlsize_t index = lower_bound(array, inserted);
  mlsize_t source;

#ifdef VOX_WRONG_INSERT
  CAMLreturn(array);
#endif

  if (index < length && Long_val(Field(array, index)) == inserted) {
    CAMLreturn(array);
  }

  /* This is the usual finite-runtime caveat behind the logical totality
     declaration: allocation failure and an array already at the runtime's
     maximum representable length are treated as resource exhaustion. */
  if (length == Max_wosize) {
    caml_invalid_argument("sorted iarray capacity");
  }

  result = caml_alloc(length + 1, 0);
  for (source = 0; source < index; source++) {
    Store_field(result, source, Field(array, source));
  }
  Store_field(result, index, inserted_value);
  for (source = index; source < length; source++) {
    Store_field(result, source + 1, Field(array, source));
  }
  CAMLreturn(result);
}

CAMLprim value vox_sorted_iarray_view(value array)
{
  CAMLparam1(array);
  CAMLlocal2(result, cell);
  mlsize_t remaining = Wosize_val(array);

  result = Val_emptylist;
  while (remaining > 0) {
    remaining--;
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, Field(array, remaining));
    Store_field(cell, 1, result);
    result = cell;
  }
  CAMLreturn(result);
}

/* Runtime bodies for the two multi-argument ghost laws.  Refinement checking
   treats their declarations as axioms; executable code only returns unit. */
CAMLprim value vox_sorted_iarray_view_insert_law(value inserted, value array)
{
  (void)inserted;
  (void)array;
  return Val_unit;
}

CAMLprim value vox_sorted_iarray_member_view_law(value query, value array)
{
  (void)query;
  (void)array;
  return Val_unit;
}
