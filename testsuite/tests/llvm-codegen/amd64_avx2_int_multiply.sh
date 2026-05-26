#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_avx2_int_multiply_generated.ml"
out="$build_dir/amd64_avx2_int_multiply_generated.o"
ir="$build_dir/amd64_avx2_int_multiply_generated.ll"

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
  elif [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/_build/main/oxcaml_main_native.exe" ]; then
    ocamlopt="$OCAMLSRCDIR/_build/main/oxcaml_main_native.exe"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

cat > "$src" <<'EOF'
external mul_unsigned_hadd_saturating_int16x16 :
  int8x32# -> int8x32# -> int16x16#
  = "" "caml_avx2_int8x32_mul_unsigned_hadd_saturating_int16x16"
[@@noalloc] [@@builtin]

external sad_unsigned : int8x32# -> int8x32# -> int64x4#
  = "" "caml_avx2_int8x32_sad_unsigned"
[@@noalloc] [@@builtin]

external mulsign8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_mulsign"
[@@noalloc] [@@builtin]

external mul_hadd16 : int16x16# -> int16x16# -> int32x8#
  = "" "caml_avx2_int16x16_mul_hadd_int32x8"
[@@noalloc] [@@builtin]

external mul_high16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_mul_high"
[@@noalloc] [@@builtin]

external mul_high_unsigned16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_mul_high_unsigned"
[@@noalloc] [@@builtin]

external mul_low16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_mul_low"
[@@noalloc] [@@builtin]

external mul_round16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_mul_round"
[@@noalloc] [@@builtin]

external mulsign16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_mulsign"
[@@noalloc] [@@builtin]

external mul_even32 : int32x8# -> int32x8# -> int64x4#
  = "" "caml_avx2_int32x8_mul_even"
[@@noalloc] [@@builtin]

external mul_even_unsigned32 : int32x8# -> int32x8# -> int64x4#
  = "" "caml_avx2_int32x8_mul_even_unsigned"
[@@noalloc] [@@builtin]

external mul_low32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_mul_low"
[@@noalloc] [@@builtin]

external mulsign32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_mulsign"
[@@noalloc] [@@builtin]

let[@inline never] use_maddubs a b = mul_unsigned_hadd_saturating_int16x16 a b
let[@inline never] use_sad a b = sad_unsigned a b
let[@inline never] use_mulsign8 a b = mulsign8 a b
let[@inline never] use_mul_hadd16 a b = mul_hadd16 a b
let[@inline never] use_mul_high16 a b = mul_high16 a b
let[@inline never] use_mul_high_unsigned16 a b = mul_high_unsigned16 a b
let[@inline never] use_mul_low16 a b = mul_low16 a b
let[@inline never] use_mul_round16 a b = mul_round16 a b
let[@inline never] use_mulsign16 a b = mulsign16 a b
let[@inline never] use_mul_even32 a b = mul_even32 a b
let[@inline never] use_mul_even_unsigned32 a b = mul_even_unsigned32 a b
let[@inline never] use_mul_low32 a b = mul_low32 a b
let[@inline never] use_mulsign32 a b = mulsign32 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'mul < 16 x i32 >' "$ir"
grep -q 'mul < 32 x i32 >' "$ir"
grep -q 'mul < 16 x i16 >' "$ir"
grep -q 'mul < 8 x i32 >' "$ir"
grep -q 'sext < 16 x i16 >' "$ir"
grep -q 'zext < 16 x i16 >' "$ir"
grep -q 'sext < 32 x i8 >' "$ir"
grep -q 'zext < 32 x i8 >' "$ir"
grep -q 'ashr < 16 x i32 >' "$ir"
grep -q 'lshr < 16 x i32 >' "$ir"
grep -q 'trunc < 16 x i32 >' "$ir"
grep -q 'sext i32' "$ir"
grep -q 'zext i32' "$ir"
grep -q 'mul i64' "$ir"
grep -q 'icmp slt < 32 x i8 >' "$ir"
grep -q 'icmp eq < 8 x i32 >' "$ir"
grep -q 'select < 16 x i1 >' "$ir"
grep -q 'extractelement < 8 x i32 >' "$ir"
grep -q 'insertelement < 4 x i64 >' "$ir"

if grep -q 'llvm[.]x86[.]avx2' "$ir"; then
  echo "unexpected target-feature-specific AVX2 intrinsic in generated IR" >&2
  exit 1
fi
