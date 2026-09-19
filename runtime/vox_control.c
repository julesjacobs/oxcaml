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

#include "caml/mlvalues.h"
#include <stdint.h>
#if defined(__aarch64__) && !defined(VOX_CONTROL_SCALAR)
#include <arm_neon.h>
#elif defined(__SSE2__) && !defined(VOX_CONTROL_SCALAR)
#include <emmintrin.h>
#endif

/* No interior pointer survives a call or safepoint. Bit i describes byte i. */
static unsigned match16(const unsigned char *p, unsigned char needle)
{
#if defined(__aarch64__) && !defined(VOX_CONTROL_SCALAR)
  static const unsigned char weights[16] =
    {1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128};
  uint8x16_t equal = vceqq_u8(vld1q_u8(p), vdupq_n_u8(needle));
  uint8x16_t bits = vandq_u8(equal, vld1q_u8(weights));
  return vaddv_u8(vget_low_u8(bits))
       | ((unsigned)vaddv_u8(vget_high_u8(bits)) << 8);
#elif defined(__SSE2__) && !defined(VOX_CONTROL_SCALAR)
  __m128i equal = _mm_cmpeq_epi8(_mm_loadu_si128((const __m128i *)p),
                               _mm_set1_epi8((char)needle));
  return (unsigned)_mm_movemask_epi8(equal);
#else
  unsigned mask = 0;
  for (unsigned i = 0; i < 16; i++)
    mask |= (unsigned)(p[i] == needle) << i;
  return mask;
#endif
}

/* Called only after the OCaml wrapper checks bounds and the byte range. */
CAMLprim value caml_vox_control_match16(value bytes, value offset, value byte)
{
  return Val_long(match16((const unsigned char *)String_val(bytes)
                          + Long_val(offset), (unsigned char)Long_val(byte)));
}
