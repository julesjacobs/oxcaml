#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_float64_arith_generated.ml"
out="$build_dir/amd64_simd_float64_arith_generated.o"
ir="$build_dir/amd64_simd_float64_arith_generated.ll"

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
external add : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_add"
[@@noalloc] [@@builtin]

external sub : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_sub"
[@@noalloc] [@@builtin]

external mul : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_mul"
[@@noalloc] [@@builtin]

external div : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_div"
[@@noalloc] [@@builtin]

external sqrt : float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_sqrt"
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

grep -q 'fadd < 2 x double >' "$ir"
grep -q 'fsub < 2 x double >' "$ir"
grep -q 'fmul < 2 x double >' "$ir"
grep -q 'fdiv < 2 x double >' "$ir"
grep -q 'call  *< 2 x double > @llvm.sqrt.v2f64' "$ir"
