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
type nonrec int64x2 = int64x2#

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
  Printf.printf "%Ld %Ld\n" (int64x2_low_int64 r64) (int64x2_high_int64 r64)
EOF

cat > "$stub_src" <<'EOF'
#include <immintrin.h>
#include <stdint.h>
#include <stdlib.h>

#define BUILTIN(name) void name(void) { abort(); }

BUILTIN(caml_vec128_unreachable)
BUILTIN(caml_sse2_float64_min)
BUILTIN(caml_sse2_float64_max)
BUILTIN(caml_sse41_vec128_blend_32)
BUILTIN(caml_sse41_vec128_blend_64)

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
EOF

"$ocamlopt" $stdlib_flags -extension simd_beta -ccopt -msse4.2 -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$stub_src" "$src"

"$out" > "$stdout_file"

expected='3.0 4.0
0000000100000004 0000000300000006
2 1'

actual=$(cat "$stdout_file")
if [ "$actual" != "$expected" ]; then
  echo "unexpected output:" >&2
  cat "$stdout_file" >&2
  exit 1
fi
