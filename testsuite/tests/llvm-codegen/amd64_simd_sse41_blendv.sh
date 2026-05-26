#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_blendv_generated.ml"
out="$build_dir/amd64_simd_sse41_blendv_generated.o"
ir="$build_dir/amd64_simd_sse41_blendv_generated.ll"

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
external blendv_8 : int8x16# -> int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse41_vec128_blendv_8"
[@@noalloc] [@@builtin]

external blendv_32 : int32x4# -> int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_vec128_blendv_32"
[@@noalloc] [@@builtin]

external blendv_64 : int64x2# -> int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse41_vec128_blendv_64"
[@@noalloc] [@@builtin]

let[@inline never] test_blendv_8 a b mask = blendv_8 a b mask
let[@inline never] test_blendv_32 a b mask = blendv_32 a b mask
let[@inline never] test_blendv_64 a b mask = blendv_64 a b mask
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'icmp slt < 16 x i8 >' "$ir")" -ge 1
test "$(grep -c 'select < 16 x i1 >' "$ir")" -ge 1
test "$(grep -c 'icmp slt < 4 x i32 >' "$ir")" -ge 1
test "$(grep -c 'select < 4 x i1 >' "$ir")" -ge 1
test "$(grep -c 'icmp slt < 2 x i64 >' "$ir")" -ge 1
test "$(grep -c 'select < 2 x i1 >' "$ir")" -ge 1
