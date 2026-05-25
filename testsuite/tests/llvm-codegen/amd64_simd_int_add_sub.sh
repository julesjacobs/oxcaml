#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_int_add_sub_generated.ml"
out="$build_dir/amd64_simd_int_add_sub_generated.o"
ir="$build_dir/amd64_simd_int_add_sub_generated.ll"

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
external add_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_add"
[@@noalloc] [@@builtin]

external add_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_add"
[@@noalloc] [@@builtin]

external add_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_add"
[@@noalloc] [@@builtin]

external sub_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_sub"
[@@noalloc] [@@builtin]

external sub_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_sub"
[@@noalloc] [@@builtin]

external sub_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_sub"
[@@noalloc] [@@builtin]

let[@inline never] test_add_i8x16 a b = add_i8x16 a b
let[@inline never] test_add_i16x8 a b = add_i16x8 a b
let[@inline never] test_add_i32x4 a b = add_i32x4 a b
let[@inline never] test_sub_i8x16 a b = sub_i8x16 a b
let[@inline never] test_sub_i16x8 a b = sub_i16x8 a b
let[@inline never] test_sub_i32x4 a b = sub_i32x4 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'add < 16 x i8 >' "$ir"
grep -q 'add < 8 x i16 >' "$ir"
grep -q 'add < 4 x i32 >' "$ir"
grep -q 'sub < 16 x i8 >' "$ir"
grep -q 'sub < 8 x i16 >' "$ir"
grep -q 'sub < 4 x i32 >' "$ir"
