#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_dot_product_generated.ml"
out="$build_dir/amd64_simd_sse41_dot_product_generated.o"
ir="$build_dir/amd64_simd_sse41_dot_product_generated.ll"

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
external dp32 : (int[@untagged]) -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse41_float32x4_dp"
[@@noalloc] [@@builtin]

external dp64 : (int[@untagged]) -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse41_float64x2_dp"
[@@noalloc] [@@builtin]

let[@inline never] test_dp32 a b = dp32 0b11110001 a b
let[@inline never] test_dp64 a b = dp64 0b00110001 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 4 x float >' "$ir")" -ge 8
test "$(grep -c 'fmul float' "$ir")" -ge 4
test "$(grep -c 'fadd float' "$ir")" -ge 4
test "$(grep -c 'insertelement < 4 x float >' "$ir")" -ge 4
test "$(grep -c 'extractelement < 2 x double >' "$ir")" -ge 4
test "$(grep -c 'fmul double' "$ir")" -ge 2
test "$(grep -c 'fadd double' "$ir")" -ge 2
test "$(grep -c 'insertelement < 2 x double >' "$ir")" -ge 2
