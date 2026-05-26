#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse3_float_generated.ml"
out="$build_dir/amd64_simd_sse3_float_generated.o"
ir="$build_dir/amd64_simd_sse3_float_generated.ll"

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
external addsub_f32 : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse3_float32x4_addsub"
[@@noalloc] [@@builtin]

external hadd_f32 : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse3_float32x4_hadd"
[@@noalloc] [@@builtin]

external hsub_f32 : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse3_float32x4_hsub"
[@@noalloc] [@@builtin]

external addsub_f64 : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse3_float64x2_addsub"
[@@noalloc] [@@builtin]

external hadd_f64 : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse3_float64x2_hadd"
[@@noalloc] [@@builtin]

external hsub_f64 : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse3_float64x2_hsub"
[@@noalloc] [@@builtin]

let[@inline never] test_addsub_f32 a b = addsub_f32 a b
let[@inline never] test_hadd_f32 a b = hadd_f32 a b
let[@inline never] test_hsub_f32 a b = hsub_f32 a b
let[@inline never] test_addsub_f64 a b = addsub_f64 a b
let[@inline never] test_hadd_f64 a b = hadd_f64 a b
let[@inline never] test_hsub_f64 a b = hsub_f64 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 4 x float >' "$ir")" -ge 24
test "$(grep -c 'extractelement < 2 x double >' "$ir")" -ge 12
test "$(grep -c 'fadd float' "$ir")" -ge 4
test "$(grep -c 'fsub float' "$ir")" -ge 4
test "$(grep -c 'fadd double' "$ir")" -ge 2
test "$(grep -c 'fsub double' "$ir")" -ge 2
grep -q 'insertelement < 4 x float > .* i64 3' "$ir"
grep -q 'insertelement < 2 x double > .* i64 1' "$ir"
