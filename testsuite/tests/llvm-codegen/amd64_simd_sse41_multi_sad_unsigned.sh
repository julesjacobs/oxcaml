#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_multi_sad_unsigned_generated.ml"
out="$build_dir/amd64_simd_sse41_multi_sad_unsigned_generated.o"
ir="$build_dir/amd64_simd_sse41_multi_sad_unsigned_generated.ll"

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
external msad_u8x16 : (int[@untagged]) -> int8x16# -> int8x16# -> int16x8#
  = "" "caml_sse41_int8x16_multi_sad_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_msad_first_blocks a b = msad_u8x16 0 a b
let[@inline never] test_msad_last_blocks a b = msad_u8x16 7 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 16 x i8 >' "$ir")" -ge 128
test "$(grep -c 'zext i8 .* to i16' "$ir")" -ge 128
test "$(grep -c 'icmp ugt i16' "$ir")" -ge 64
test "$(grep -c 'select i1' "$ir")" -ge 128
test "$(grep -c 'sub i16' "$ir")" -ge 64
test "$(grep -c 'add i16' "$ir")" -ge 64
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 16
