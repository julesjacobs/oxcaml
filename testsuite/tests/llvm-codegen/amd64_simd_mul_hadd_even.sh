#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_mul_hadd_even_generated.ml"
out="$build_dir/amd64_simd_mul_hadd_even_generated.o"
ir="$build_dir/amd64_simd_mul_hadd_even_generated.ll"

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
external mul_hadd_i32 : int16x8# -> int16x8# -> int32x4#
  = "" "caml_sse2_int16x8_mul_hadd_int32x4"
[@@noalloc] [@@builtin]

external mul_even_unsigned : int32x4# -> int32x4# -> int64x2#
  = "" "caml_sse2_int32x4_mul_even_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_mul_hadd_i32 a b = mul_hadd_i32 a b
let[@inline never] test_mul_even_unsigned a b = mul_even_unsigned a b
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
grep -q 'mul < 8 x i32 >' "$ir"
grep -q 'extractelement < 8 x i32 > .* i64 7' "$ir"
grep -q 'add i32' "$ir"
grep -q 'insertelement < 4 x i32 > .* i64 3' "$ir"
grep -q 'extractelement < 4 x i32 > .* i64 0' "$ir"
grep -q 'extractelement < 4 x i32 > .* i64 2' "$ir"
grep -q 'zext i32 .* to i64' "$ir"
grep -q 'mul i64' "$ir"
grep -q 'insertelement < 2 x i64 > .* i64 1' "$ir"
