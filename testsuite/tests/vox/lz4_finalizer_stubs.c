#include <assert.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

CAMLextern value caml_raw_memory_malloc(value size);
CAMLextern void caml_raw_memory_free(value handle);

static void (*release_buffer)(value);
static int reclaimed_buffers;

static void tracked_finalize(value carrier)
{
  int had_data = *(unsigned char **) Data_custom_val(carrier) != NULL;
  release_buffer(carrier);
  assert(*(unsigned char **) Data_custom_val(carrier) == NULL);
  if (had_data) reclaimed_buffers++;
}

CAMLprim value caml_lz4_watch_finalizers(value unit)
{
  CAMLparam1(unit);
  CAMLlocal2(allocation, handle);
  allocation = caml_raw_memory_malloc(Val_long(1));
  assert(Field(allocation, 0) != Val_none);
  handle = Field(Field(allocation, 0), 0);
  struct custom_operations *ops =
    (struct custom_operations *) Custom_ops_val(Field(handle, 2));
  assert(release_buffer == NULL);
  assert(ops->finalize != NULL);
  release_buffer = ops->finalize;
  ops->finalize = tracked_finalize;
  caml_raw_memory_free(handle);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_lz4_reclaimed_buffers(value unit)
{
  return Val_long(reclaimed_buffers);
}

CAMLprim value caml_lz4_make_buffer(value size)
{
  CAMLparam1(size);
  CAMLlocal2(allocation, handle);
  allocation = caml_raw_memory_malloc(size);
  assert(Field(allocation, 0) != Val_none);
  handle = Field(Field(allocation, 0), 0);
  unsigned char *data = *(unsigned char **) Data_custom_val(Field(handle, 2));
  data[0] = 165;
  CAMLreturn(handle);
}

CAMLprim value caml_lz4_check_buffer(value handle)
{
  unsigned char *data = *(unsigned char **) Data_custom_val(Field(handle, 2));
  assert(data != NULL && data[0] == 165);
  return Val_unit;
}

CAMLprim value caml_lz4_release_buffer(value handle)
{
  caml_raw_memory_free(handle);
  return Val_unit;
}
