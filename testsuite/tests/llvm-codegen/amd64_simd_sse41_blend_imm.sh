#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_blend_imm_generated.ml"
out="$build_dir/amd64_simd_sse41_blend_imm_generated.o"
ir="$build_dir/amd64_simd_sse41_blend_imm_generated.ll"

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
external blend_16 : (int[@untagged]) -> int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse41_vec128_blend_16"
[@@noalloc] [@@builtin]

external blend_32 : (int[@untagged]) -> int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_vec128_blend_32"
[@@noalloc] [@@builtin]

external blend_64 : (int[@untagged]) -> int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse41_vec128_blend_64"
[@@noalloc] [@@builtin]

let[@inline never] test_blend_16 a b = blend_16 0b01010101 a b
let[@inline never] test_blend_32 a b = blend_32 0b0101 a b
let[@inline never] test_blend_64 a b = blend_64 0b01 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 8 x i16 >' "$ir")" -ge 8
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 8
test "$(grep -c 'extractelement < 4 x i32 >' "$ir")" -ge 4
test "$(grep -c 'insertelement < 4 x i32 >' "$ir")" -ge 4
test "$(grep -c 'extractelement < 2 x i64 >' "$ir")" -ge 2
test "$(grep -c 'insertelement < 2 x i64 >' "$ir")" -ge 2
