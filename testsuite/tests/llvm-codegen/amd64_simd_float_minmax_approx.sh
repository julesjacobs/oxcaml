#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_float_minmax_approx_generated.ml"
out="$build_dir/amd64_simd_float_minmax_approx_generated.o"
ir="$build_dir/amd64_simd_float_minmax_approx_generated.ll"

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
external max_f32 : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_max"
[@@noalloc] [@@builtin]

external min_f32 : float32x4# -> float32x4# -> float32x4#
  = "" "caml_sse_float32x4_min"
[@@noalloc] [@@builtin]

external rcp_f32 : float32x4# -> float32x4#
  = "" "caml_sse_float32x4_rcp"
[@@noalloc] [@@builtin]

external rsqrt_f32 : float32x4# -> float32x4#
  = "" "caml_sse_float32x4_rsqrt"
[@@noalloc] [@@builtin]

external max_f64 : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_max"
[@@noalloc] [@@builtin]

external min_f64 : float64x2# -> float64x2# -> float64x2#
  = "" "caml_sse2_float64x2_min"
[@@noalloc] [@@builtin]

let[@inline never] test_max_f32 a b = max_f32 a b
let[@inline never] test_min_f32 a b = min_f32 a b
let[@inline never] test_rcp_f32 a = rcp_f32 a
let[@inline never] test_rsqrt_f32 a = rsqrt_f32 a
let[@inline never] test_max_f64 a b = max_f64 a b
let[@inline never] test_min_f64 a b = min_f64 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q '@llvm.x86.sse.max.ps' "$ir"
grep -q '@llvm.x86.sse.min.ps' "$ir"
grep -q '@llvm.x86.sse.rcp.ps' "$ir"
grep -q '@llvm.x86.sse.rsqrt.ps' "$ir"
grep -q '@llvm.x86.sse2.max.pd' "$ir"
grep -q '@llvm.x86.sse2.min.pd' "$ir"
