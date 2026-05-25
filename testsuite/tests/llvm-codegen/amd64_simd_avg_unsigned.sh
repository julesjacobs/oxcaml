#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_avg_unsigned_generated.ml"
out="$build_dir/amd64_simd_avg_unsigned_generated.o"
ir="$build_dir/amd64_simd_avg_unsigned_generated.ll"

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
external avg_u8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_avg_unsigned"
[@@noalloc] [@@builtin]

external avg_u16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_avg_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_avg_u8x16 a b = avg_u8x16 a b
let[@inline never] test_avg_u16x8 a b = avg_u16x8 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'zext < 16 x i8 > .* to < 16 x i16 >' "$ir"
grep -q 'zext < 8 x i16 > .* to < 8 x i32 >' "$ir"
grep -q 'add < 16 x i16 >' "$ir"
grep -q 'add < 8 x i32 >' "$ir"
grep -q 'lshr < 16 x i16 >' "$ir"
grep -q 'lshr < 8 x i32 >' "$ir"
grep -q 'trunc < 16 x i16 > .* to < 16 x i8 >' "$ir"
grep -q 'trunc < 8 x i32 > .* to < 8 x i16 >' "$ir"
