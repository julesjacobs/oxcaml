#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_int_compare_generated.ml"
out="$build_dir/amd64_simd_int_compare_generated.o"
ir="$build_dir/amd64_simd_int_compare_generated.ll"

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
external cmpeq_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_cmpeq"
[@@noalloc] [@@builtin]

external cmpeq_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_cmpeq"
[@@noalloc] [@@builtin]

external cmpeq_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_cmpeq"
[@@noalloc] [@@builtin]

external cmpgt_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_cmpgt"
[@@noalloc] [@@builtin]

external cmpgt_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_cmpgt"
[@@noalloc] [@@builtin]

external cmpgt_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_cmpgt"
[@@noalloc] [@@builtin]

let[@inline never] test_cmpeq_i8x16 a b = cmpeq_i8x16 a b
let[@inline never] test_cmpeq_i16x8 a b = cmpeq_i16x8 a b
let[@inline never] test_cmpeq_i32x4 a b = cmpeq_i32x4 a b
let[@inline never] test_cmpgt_i8x16 a b = cmpgt_i8x16 a b
let[@inline never] test_cmpgt_i16x8 a b = cmpgt_i16x8 a b
let[@inline never] test_cmpgt_i32x4 a b = cmpgt_i32x4 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'icmp eq < 16 x i8 >' "$ir"
grep -q 'icmp eq < 8 x i16 >' "$ir"
grep -q 'icmp eq < 4 x i32 >' "$ir"
grep -q 'icmp sgt < 16 x i8 >' "$ir"
grep -q 'icmp sgt < 8 x i16 >' "$ir"
grep -q 'icmp sgt < 4 x i32 >' "$ir"
grep -q 'sext < 16 x i1 >' "$ir"
grep -q 'sext < 8 x i1 >' "$ir"
grep -q 'sext < 4 x i1 >' "$ir"
