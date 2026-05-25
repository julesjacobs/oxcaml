#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_int16_mul_generated.ml"
out="$build_dir/amd64_simd_int16_mul_generated.o"
ir="$build_dir/amd64_simd_int16_mul_generated.ll"

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
external mul_high : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_mul_high"
[@@noalloc] [@@builtin]

external mul_high_unsigned : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_mul_high_unsigned"
[@@noalloc] [@@builtin]

external mul_low : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_mul_low"
[@@noalloc] [@@builtin]

let[@inline never] test_mul_high a b = mul_high a b
let[@inline never] test_mul_high_unsigned a b = mul_high_unsigned a b
let[@inline never] test_mul_low a b = mul_low a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'sext < 8 x i16 > .* to < 8 x i32 >' "$ir"
grep -q 'zext < 8 x i16 > .* to < 8 x i32 >' "$ir"
grep -q 'mul < 8 x i32 >' "$ir"
grep -q 'ashr < 8 x i32 >' "$ir"
grep -q 'lshr < 8 x i32 >' "$ir"
grep -q 'trunc < 8 x i32 > .* to < 8 x i16 >' "$ir"
grep -q 'mul < 8 x i16 >' "$ir"
