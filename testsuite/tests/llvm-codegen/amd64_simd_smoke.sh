#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_smoke_generated.ml"
stub_src="$build_dir/amd64_simd_smoke_stubs.c"
out="$build_dir/amd64_simd_smoke_generated.exe"
stdout_file="$build_dir/amd64_simd_smoke_stdout.txt"

search_dir=$build_dir
ocamlopt=""
while [ "$search_dir" != "/" ]; do
  if [ -f "$search_dir/ocamlopt.opt" ] && [ -x "$search_dir/ocamlopt.opt" ]; then
    ocamlopt="$search_dir/ocamlopt.opt"
    break
  fi
  search_dir=$(dirname "$search_dir")
done

if [ -z "$ocamlopt" ]; then
  if [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
    ocamlopt="$OCAMLSRCDIR/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

stdlib_flags=""
ocamlopt_dir=$(dirname "$ocamlopt")
for stdlib_dir in \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_flags="-I $stdlib_dir"
    break
  fi
done

cat > "$src" <<'EOF'
type nonrec int32x4 = int32x4#
type nonrec int32x8 = int32x8#
type nonrec int64x2 = int64x2#
type nonrec int64x4 = int64x4#

external min_float64 : float -> float -> float
  = "caml_vec128_unreachable" "caml_sse2_float64_min"
[@@noalloc] [@@unboxed] [@@builtin]

external max_float64 : float -> float -> float
  = "caml_vec128_unreachable" "caml_sse2_float64_max"
[@@noalloc] [@@unboxed] [@@builtin]

external int32x4_of_int64s : int64 -> int64 -> int32x4
  = "caml_vec128_unreachable" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external int32x4_low_int64 : int32x4 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
[@@noalloc] [@@unboxed]

external int32x4_high_int64 : int32x4 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
[@@noalloc] [@@unboxed]

external int64x2_of_int64s : int64 -> int64 -> int64x2
  = "caml_vec128_unreachable" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external int64x2_low_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
[@@noalloc] [@@unboxed]

external int64x2_high_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
[@@noalloc] [@@unboxed]

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "caml_vec256_unreachable" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external int64x4_lane0 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_lane0"
[@@noalloc] [@@unboxed]

external int64x4_lane1 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_lane1"
[@@noalloc] [@@unboxed]

external int64x4_lane2 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_lane2"
[@@noalloc] [@@unboxed]

external int64x4_lane3 : int64x4 -> int64
  = "caml_vec256_unreachable" "vec256_lane3"
[@@noalloc] [@@unboxed]

external int32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int32x8
  = "caml_vec256_unreachable" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external int32x8_lane01 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_lane0"
[@@noalloc] [@@unboxed]

external int32x8_lane23 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_lane1"
[@@noalloc] [@@unboxed]

external int32x8_lane45 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_lane2"
[@@noalloc] [@@unboxed]

external int32x8_lane67 : int32x8 -> int64
  = "caml_vec256_unreachable" "vec256_lane3"
[@@noalloc] [@@unboxed]

external blend_32 :
  (int[@untagged]) ->
  (int32x4[@unboxed]) ->
  (int32x4[@unboxed]) ->
  (int32x4[@unboxed])
  = "caml_vec128_unreachable" "caml_sse41_vec128_blend_32"
[@@noalloc] [@@builtin]

external blend_64 :
  (int[@untagged]) ->
  (int64x2[@unboxed]) ->
  (int64x2[@unboxed]) ->
  (int64x2[@unboxed])
  = "caml_vec128_unreachable" "caml_sse41_vec128_blend_64"
[@@noalloc] [@@builtin]

external blend_256_64 :
  (int[@untagged]) ->
  (int64x4[@unboxed]) ->
  (int64x4[@unboxed]) ->
  (int64x4[@unboxed])
  = "caml_vec256_unreachable" "caml_avx_vec256_blend_64"
[@@noalloc] [@@builtin]

external blend_256_32 :
  (int[@untagged]) ->
  (int32x8[@unboxed]) ->
  (int32x8[@unboxed]) ->
  (int32x8[@unboxed])
  = "caml_vec256_unreachable" "caml_avx_vec256_blend_32"
[@@noalloc] [@@builtin]

external extract_128 :
  (int[@untagged]) -> (int64x4[@unboxed]) -> (int64x2[@unboxed])
  = "caml_vec128_unreachable" "caml_avx_vec256_extract_128"
[@@noalloc] [@@builtin]

external insert_128 :
  (int[@untagged]) ->
  (int64x4[@unboxed]) ->
  (int64x2[@unboxed]) ->
  (int64x4[@unboxed])
  = "caml_vec256_unreachable" "caml_avx_vec256_insert_128"
[@@noalloc] [@@builtin]

let[@inline never] force_alloc () =
  ignore (Array.init 128 (fun i -> i + 1))

let[@inline never] opaque_vec128_sum v =
  let v = Sys.opaque_identity v in
  force_alloc ();
  Int64.add (int64x2_low_int64 v) (int64x2_high_int64 v)

let[@inline never] opaque_vec256_sum v =
  let v = Sys.opaque_identity v in
  force_alloc ();
  Int64.add
    (Int64.add (int64x4_lane0 v) (int64x4_lane1 v))
    (Int64.add (int64x4_lane2 v) (int64x4_lane3 v))

let () =
  Printf.printf "%.1f %.1f\n" (min_float64 3.0 4.0) (max_float64 3.0 4.0);
  let v0 = int32x4_of_int64s 0x00000001_00000000L 0x00000003_00000002L in
  let v1 = int32x4_of_int64s 0x00000005_00000004L 0x00000007_00000006L in
  let r32 = blend_32 0b0101 v0 v1 in
  Printf.printf "%016Lx %016Lx\n"
    (int32x4_low_int64 r32)
    (int32x4_high_int64 r32);
  let v0 = int64x2_of_int64s 0L 1L in
  let v1 = int64x2_of_int64s 2L 3L in
  let r64 = blend_64 0b01 v0 v1 in
  Printf.printf "%Ld %Ld\n" (int64x2_low_int64 r64) (int64x2_high_int64 r64);
  let v0 = int64x4_of_int64s 0L 1L 2L 3L in
  let v1 = int64x4_of_int64s 4L 5L 6L 7L in
  let r64x4 = blend_256_64 0b0101 v0 v1 in
  Printf.printf "%Ld %Ld %Ld %Ld\n"
    (int64x4_lane0 r64x4)
    (int64x4_lane1 r64x4)
    (int64x4_lane2 r64x4)
    (int64x4_lane3 r64x4);
  let v0 =
    int32x8_of_int64s 0x00000001_00000000L 0x00000003_00000002L
      0x00000005_00000004L 0x00000007_00000006L
  in
  let v1 =
    int32x8_of_int64s 0x00000009_00000008L 0x0000000b_0000000aL
      0x0000000d_0000000cL 0x0000000f_0000000eL
  in
  let r32x8 = blend_256_32 0b01010101 v0 v1 in
  Printf.printf "%016Lx %016Lx %016Lx %016Lx\n"
    (int32x8_lane01 r32x8)
    (int32x8_lane23 r32x8)
    (int32x8_lane45 r32x8)
    (int32x8_lane67 r32x8);
  let base = int64x4_of_int64s 0L 1L 2L 3L in
  let half = int64x2_of_int64s 8L 9L in
  let low_half = extract_128 0 base in
  let high_half = extract_128 1 base in
  Printf.printf "%Ld %Ld / %Ld %Ld\n"
    (int64x2_low_int64 low_half)
    (int64x2_high_int64 low_half)
    (int64x2_low_int64 high_half)
    (int64x2_high_int64 high_half);
  let insert_low = insert_128 0 base half in
  let insert_high = insert_128 1 base half in
  Printf.printf "%Ld %Ld %Ld %Ld / %Ld %Ld %Ld %Ld\n"
    (int64x4_lane0 insert_low)
    (int64x4_lane1 insert_low)
    (int64x4_lane2 insert_low)
    (int64x4_lane3 insert_low)
    (int64x4_lane0 insert_high)
    (int64x4_lane1 insert_high)
    (int64x4_lane2 insert_high)
    (int64x4_lane3 insert_high);
  Printf.printf "opaque:%Ld\n"
    (opaque_vec128_sum (int64x2_of_int64s 21L 34L));
  Printf.printf "opaque256:%Ld\n"
    (opaque_vec256_sum (int64x4_of_int64s 1L 2L 3L 4L))
EOF

cat > "$stub_src" <<'EOF'
#include <immintrin.h>
#include <stdint.h>
#include <stdlib.h>

#define BUILTIN(name) void name(void) { abort(); }

BUILTIN(caml_vec128_unreachable)
BUILTIN(caml_vec256_unreachable)
BUILTIN(caml_sse2_float64_min)
BUILTIN(caml_sse2_float64_max)
BUILTIN(caml_sse41_vec128_blend_32)
BUILTIN(caml_sse41_vec128_blend_64)
BUILTIN(caml_avx_vec256_blend_64)
BUILTIN(caml_avx_vec256_blend_32)
BUILTIN(caml_avx_vec256_extract_128)
BUILTIN(caml_avx_vec256_insert_128)

typedef union {
  __m128i vec;
  int64_t i64[2];
} vec128_words;

__m128i vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

int64_t vec128_low_int64(__m128i v)
{
  vec128_words words;
  words.vec = v;
  return words.i64[0];
}

int64_t vec128_high_int64(__m128i v)
{
  vec128_words words;
  words.vec = v;
  return words.i64[1];
}

typedef union {
  __m256i vec;
  int64_t i64[4];
} vec256_words;

__m256i vec256_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d)
{
  return _mm256_set_epi64x(d, c, b, a);
}

int64_t vec256_lane0(__m256i v)
{
  vec256_words words;
  words.vec = v;
  return words.i64[0];
}

int64_t vec256_lane1(__m256i v)
{
  vec256_words words;
  words.vec = v;
  return words.i64[1];
}

int64_t vec256_lane2(__m256i v)
{
  vec256_words words;
  words.vec = v;
  return words.i64[2];
}

int64_t vec256_lane3(__m256i v)
{
  vec256_words words;
  words.vec = v;
  return words.i64[3];
}
EOF

"$ocamlopt" $stdlib_flags -extension simd_beta -favx -ccopt -mavx -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$stub_src" "$src"

"$out" > "$stdout_file"

expected='3.0 4.0
0000000100000004 0000000300000006
2 1
4 1 6 3
0000000100000008 000000030000000a 000000050000000c 000000070000000e
0 1 / 2 3
8 9 2 3 / 0 1 8 9
opaque:55
opaque256:10'

actual=$(cat "$stdout_file")
if [ "$actual" != "$expected" ]; then
  echo "unexpected output:" >&2
  cat "$stdout_file" >&2
  exit 1
fi
