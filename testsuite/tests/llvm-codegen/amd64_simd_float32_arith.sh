#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_float32_arith_generated.ml"
out="$build_dir/amd64_simd_float32_arith_generated.o"
ir="$build_dir/amd64_simd_float32_arith_generated.ll"

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
external add : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_add"
[@@noalloc] [@@builtin]

external sub : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_sub"
[@@noalloc] [@@builtin]

external mul : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_mul"
[@@noalloc] [@@builtin]

external div : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_div"
[@@noalloc] [@@builtin]

external sqrt : float32x4# -> float32x4#
  = "" "caml_sse_float32x4_sqrt"
[@@noalloc] [@@builtin]

let[@inline never] test_add a b = add a b
let[@inline never] test_sub a b = sub a b
let[@inline never] test_mul a b = mul a b
let[@inline never] test_div a b = div a b
let[@inline never] test_sqrt a = sqrt a
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'fadd < 4 x float >' "$ir"
grep -q 'fsub < 4 x float >' "$ir"
grep -q 'fmul < 4 x float >' "$ir"
grep -q 'fdiv < 4 x float >' "$ir"
grep -q 'call  *< 4 x float > @llvm.sqrt.v4f32' "$ir"
