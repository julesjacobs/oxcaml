/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                  */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdint.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/hash.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#define BIGINT_BASE UINT32_C(1000000000)

struct bigint {
  intnat size;
  uint32_t digit[];
};

#define Bigint_val(v) ((struct bigint *) Data_custom_val(v))

static uintnat bigint_size(const struct bigint *b)
{
  return b->size < 0 ? -b->size : b->size;
}

static uintnat trim(const uint32_t *digit, uintnat n)
{
  while (n > 0 && digit[n - 1] == 0) n--;
  return n;
}

static void normalize(struct bigint *b, uintnat n, int sign)
{
  b->size = sign * (intnat) trim(b->digit, n);
}

static int compare_magnitude(const uint32_t *a, uintnat na,
                             const uint32_t *b, uintnat nb)
{
  if (na != nb) return na > nb ? 1 : -1;
  while (na > 0) {
    na--;
    if (a[na] != b[na]) return a[na] > b[na] ? 1 : -1;
  }
  return 0;
}

static int bigint_compare(value va, value vb)
{
  const struct bigint *a = Bigint_val(va), *b = Bigint_val(vb);
  if (a->size != b->size) return a->size > b->size ? 1 : -1;
  int cmp = compare_magnitude(a->digit, bigint_size(a),
                              b->digit, bigint_size(b));
  return a->size < 0 ? -cmp : cmp;
}

static intnat bigint_hash(value v)
{
  const struct bigint *b = Bigint_val(v);
  uint32_t hash = b->size < 0;
  for (uintnat i = 0; i < bigint_size(b); i++)
    hash = caml_hash_mix_uint32(hash, b->digit[i]);
  return hash;
}

static void bigint_serialize(value v, uintnat *size32, uintnat *size64)
{
  const struct bigint *b = Bigint_val(v);
  uintnat n = bigint_size(b);
  if (n > (UINT32_MAX - 4) / 4)
    caml_invalid_argument("Bigint: value too large to marshal");
  caml_serialize_int_8(b->size);
  for (uintnat i = 0; i < n; i++) caml_serialize_int_4(b->digit[i]);
  *size32 = 4 + 4 * n;
  *size64 = 8 + 4 * n;
}

static uintnat bigint_deserialize(void *dst)
{
  struct bigint *b = dst;
  uintnat capacity =
    (Wosize_val(Custom_val_data(dst)) - 1) * sizeof(value);
  int64_t size = caml_deserialize_sint_8();
  if (capacity < sizeof(intnat))
    caml_deserialize_error("Bigint: invalid block size");
  uintnat max_digits = (capacity - sizeof(intnat)) / sizeof(uint32_t);
  if (size < -(int64_t) max_digits || size > (int64_t) max_digits)
    caml_deserialize_error("Bigint: invalid digit count");
  uintnat n = size < 0 ? -size : size;
  for (uintnat i = 0; i < n; i++) {
    uint32_t digit = caml_deserialize_uint_4();
    if (digit >= BIGINT_BASE)
      caml_deserialize_error("Bigint: invalid digit");
    b->digit[i] = digit;
  }
  if (n > 0 && b->digit[n - 1] == 0)
    caml_deserialize_error("Bigint: noncanonical digits");
  b->size = size;
  return sizeof(intnat) + n * sizeof(uint32_t);
}

CAMLexport const struct custom_operations caml_bigint_ops = {
  "vox.bigint.v1",
  custom_finalize_default,
  bigint_compare,
  bigint_hash,
  bigint_serialize,
  bigint_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value bigint_alloc(uintnat n)
{
  uintnat max_bytes = (Max_wosize - 1) * sizeof(value);
  if (n > (max_bytes - sizeof(intnat)) / sizeof(uint32_t))
    caml_raise_out_of_memory();
  uintnat bytes = sizeof(intnat) + n * sizeof(uint32_t);
  value v = caml_alloc_custom(&caml_bigint_ops, bytes, 0, 1);
  memset(Data_custom_val(v), 0, bytes);
  return v;
}

CAMLprim value caml_bigint_of_int(value v)
{
  intnat i = Long_val(v);
  uintnat magnitude = i < 0 ? 0 - (uintnat) i : (uintnat) i;
  uintnat n = 0;
  for (uintnat remaining = magnitude; remaining; remaining /= BIGINT_BASE)
    n++;
  value result = bigint_alloc(n);
  struct bigint *b = Bigint_val(result);
  b->size = i < 0 ? -(intnat) n : (intnat) n;
  for (uintnat j = 0; j < n; j++) {
    b->digit[j] = magnitude % BIGINT_BASE;
    magnitude /= BIGINT_BASE;
  }
  return result;
}

CAMLprim value caml_bigint_to_int_opt(value v)
{
  CAMLparam1(v);
  CAMLlocal1(result);
  const struct bigint *b = Bigint_val(v);
  uintnat limit = b->size < 0 ? (uintnat) Max_long + 1 : (uintnat) Max_long;
  uintnat magnitude = 0;
  for (uintnat i = bigint_size(b); i > 0; i--) {
    uintnat digit = b->digit[i - 1];
    if (magnitude > (limit - digit) / BIGINT_BASE) CAMLreturn(Val_none);
    magnitude = magnitude * BIGINT_BASE + digit;
  }
  intnat i = b->size < 0 ? -(intnat) magnitude : (intnat) magnitude;
  result = caml_alloc_small(1, 0);
  Field(result, 0) = Val_long(i);
  CAMLreturn(result);
}

CAMLprim value caml_bigint_neg(value v)
{
  CAMLparam1(v);
  CAMLlocal1(result);
  uintnat n = bigint_size(Bigint_val(v));
  if (n == 0) CAMLreturn(v);
  result = bigint_alloc(n);
  const struct bigint *b = Bigint_val(v);
  memcpy(Bigint_val(result)->digit, b->digit, n * sizeof(uint32_t));
  Bigint_val(result)->size = -b->size;
  CAMLreturn(result);
}

static uintnat add_magnitude(uint32_t *out, const uint32_t *a, uintnat na,
                            const uint32_t *b, uintnat nb)
{
  uintnat n = na > nb ? na : nb;
  uint32_t carry = 0;
  for (uintnat i = 0; i < n; i++) {
    uint32_t sum = carry + (i < na ? a[i] : 0) + (i < nb ? b[i] : 0);
    carry = sum >= BIGINT_BASE;
    out[i] = carry ? sum - BIGINT_BASE : sum;
  }
  out[n] = carry;
  return n + carry;
}

static uintnat sub_magnitude(uint32_t *out, const uint32_t *a, uintnat na,
                            const uint32_t *b, uintnat nb)
{
  uint32_t borrow = 0;
  for (uintnat i = 0; i < na; i++) {
    uint32_t sub = borrow + (i < nb ? b[i] : 0);
    borrow = a[i] < sub;
    out[i] = a[i] + (borrow ? BIGINT_BASE : 0) - sub;
  }
  return trim(out, na);
}

static value bigint_add_sub(value va, value vb, int subtract)
{
  CAMLparam2(va, vb);
  CAMLlocal1(result);
  uintnat na = bigint_size(Bigint_val(va)), nb = bigint_size(Bigint_val(vb));
  int sa = Bigint_val(va)->size < 0 ? -1 : 1;
  int sb = Bigint_val(vb)->size < 0 ? -1 : 1;
  if (subtract) sb = -sb;
  result = bigint_alloc((na > nb ? na : nb) + 1);
  const uint32_t *a = Bigint_val(va)->digit, *b = Bigint_val(vb)->digit;
  struct bigint *out = Bigint_val(result);
  if (sa == sb) {
    normalize(out, add_magnitude(out->digit, a, na, b, nb), sa);
  } else if (compare_magnitude(a, na, b, nb) >= 0) {
    normalize(out, sub_magnitude(out->digit, a, na, b, nb), sa);
  } else {
    normalize(out, sub_magnitude(out->digit, b, nb, a, na), sb);
  }
  CAMLreturn(result);
}

CAMLprim value caml_bigint_add(value a, value b)
{
  return bigint_add_sub(a, b, 0);
}

CAMLprim value caml_bigint_sub(value a, value b)
{
  return bigint_add_sub(a, b, 1);
}

CAMLprim value caml_bigint_mul(value va, value vb)
{
  CAMLparam2(va, vb);
  CAMLlocal1(result);
  uintnat na = bigint_size(Bigint_val(va)), nb = bigint_size(Bigint_val(vb));
  result = bigint_alloc(na + nb);
  const struct bigint *a = Bigint_val(va), *b = Bigint_val(vb);
  struct bigint *out = Bigint_val(result);
  for (uintnat i = 0; i < na; i++) {
    uint64_t carry = 0;
    for (uintnat j = 0; j < nb; j++) {
      uint64_t product = (uint64_t) a->digit[i] * b->digit[j]
                        + out->digit[i + j] + carry;
      out->digit[i + j] = product % BIGINT_BASE;
      carry = product / BIGINT_BASE;
    }
    out->digit[i + nb] = carry;
  }
  normalize(out, na + nb, (a->size < 0) == (b->size < 0) ? 1 : -1);
  CAMLreturn(result);
}

static uintnat mul_digit(uint32_t *out, const uint32_t *a, uintnat n,
                         uint32_t digit)
{
  uint64_t carry = 0;
  for (uintnat i = 0; i < n; i++) {
    uint64_t product = (uint64_t) a[i] * digit + carry;
    out[i] = product % BIGINT_BASE;
    carry = product / BIGINT_BASE;
  }
  out[n] = carry;
  return trim(out, n + 1);
}

static value bigint_divrem(value va, value vb, int remainder)
{
  CAMLparam2(va, vb);
  CAMLlocal3(vq, vr, scratch);
  uintnat na = bigint_size(Bigint_val(va)), nb = bigint_size(Bigint_val(vb));
  if (nb == 0) CAMLreturn(remainder ? va : bigint_alloc(0));
  vq = bigint_alloc(na + 1);
  vr = bigint_alloc(nb + 1);
  scratch = bigint_alloc(nb + 1);
  const struct bigint *a = Bigint_val(va), *b = Bigint_val(vb);
  struct bigint *q = Bigint_val(vq), *r = Bigint_val(vr);
  uint32_t *tmp = Bigint_val(scratch)->digit;
  uintnat nr = 0;
  for (uintnat i = na; i > 0; i--) {
    memmove(r->digit + 1, r->digit, nr * sizeof(uint32_t));
    r->digit[0] = a->digit[i - 1];
    nr = trim(r->digit, nr + 1);
    uint32_t low = 0, high = BIGINT_BASE - 1;
    while (low < high) {
      uint32_t mid = low + (high - low + 1) / 2;
      uintnat nt = mul_digit(tmp, b->digit, nb, mid);
      if (compare_magnitude(tmp, nt, r->digit, nr) <= 0) low = mid;
      else high = mid - 1;
    }
    q->digit[i - 1] = low;
    uintnat nt = mul_digit(tmp, b->digit, nb, low);
    nr = sub_magnitude(r->digit, r->digit, nr, tmp, nt);
  }
  uintnat nq = trim(q->digit, na);
  if (a->size < 0 && nr > 0) {
    uint32_t one = 1;
    nq = add_magnitude(q->digit, q->digit, nq, &one, 1);
    nr = sub_magnitude(r->digit, b->digit, nb, r->digit, nr);
  }
  normalize(q, nq, (a->size < 0) == (b->size < 0) ? 1 : -1);
  normalize(r, nr, 1);
  CAMLreturn(remainder ? vr : vq);
}

CAMLprim value caml_bigint_div(value a, value b)
{
  return bigint_divrem(a, b, 0);
}

CAMLprim value caml_bigint_modulo(value a, value b)
{
  return bigint_divrem(a, b, 1);
}

CAMLprim value caml_bigint_to_string(value v)
{
  CAMLparam1(v);
  CAMLlocal1(result);
  const struct bigint *b = Bigint_val(v);
  uintnat n = bigint_size(b);
  if (n == 0) CAMLreturn(caml_copy_string("0"));
  uintnat leading = 1;
  for (uint32_t digit = b->digit[n - 1]; digit >= 10; digit /= 10) leading++;
  uintnat negative = b->size < 0;
  if (n - 1 > ((Max_wosize - 1) * sizeof(value) - 10) / 9)
    caml_raise_out_of_memory();
  uintnat length = negative + leading + 9 * (n - 1);
  result = caml_alloc_string(length);
  b = Bigint_val(v);
  char *out = (char *) String_val(result);
  if (negative) out[0] = '-';
  uintnat position = length;
  for (uintnat i = 0; i < n; i++) {
    uint32_t digit = b->digit[i];
    uintnat width = i + 1 == n ? leading : 9;
    for (uintnat j = 0; j < width; j++) {
      out[--position] = '0' + digit % 10;
      digit /= 10;
    }
  }
  CAMLreturn(result);
}
