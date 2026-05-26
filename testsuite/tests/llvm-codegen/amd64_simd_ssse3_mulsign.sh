#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_ssse3_mulsign_generated.ml"
out="$build_dir/amd64_simd_ssse3_mulsign_generated.o"
ir="$build_dir/amd64_simd_ssse3_mulsign_generated.ll"

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
external mulsign_i8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_ssse3_int8x16_mulsign"
[@@noalloc] [@@builtin]

external mulsign_i16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_ssse3_int16x8_mulsign"
[@@noalloc] [@@builtin]

external mulsign_i32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_ssse3_int32x4_mulsign"
[@@noalloc] [@@builtin]

let[@inline never] test_mulsign_i8 v s = mulsign_i8 v s
let[@inline never] test_mulsign_i16 v s = mulsign_i16 v s
let[@inline never] test_mulsign_i32 v s = mulsign_i32 v s
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'icmp slt < 16 x i8 >' "$ir"
grep -q 'icmp eq < 16 x i8 >' "$ir"
grep -q 'sub < 16 x i8 >' "$ir"
grep -q 'icmp slt < 8 x i16 >' "$ir"
grep -q 'icmp eq < 8 x i16 >' "$ir"
grep -q 'sub < 8 x i16 >' "$ir"
grep -q 'icmp slt < 4 x i32 >' "$ir"
grep -q 'icmp eq < 4 x i32 >' "$ir"
grep -q 'sub < 4 x i32 >' "$ir"
