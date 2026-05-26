#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_avx_float_generated.ml"
out="$build_dir/amd64_avx_float_generated.o"
ir="$build_dir/amd64_avx_float_generated.ll"

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
external f32_add : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_add"
[@@noalloc] [@@builtin]

external f32_sub : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_sub"
[@@noalloc] [@@builtin]

external f32_mul : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_mul"
[@@noalloc] [@@builtin]

external f32_div : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_div"
[@@noalloc] [@@builtin]

external f32_sqrt : float32x8# -> float32x8#
  = "" "caml_avx_float32x8_sqrt"
[@@noalloc] [@@builtin]

external f32_min : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_min"
[@@noalloc] [@@builtin]

external f32_max : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_max"
[@@noalloc] [@@builtin]

external f32_addsub : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_addsub"
[@@noalloc] [@@builtin]

external f32_hadd : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x4x2_hadd"
[@@noalloc] [@@builtin]

external f32_hsub : float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x4x2_hsub"
[@@noalloc] [@@builtin]

external f32_cmp : (int[@untagged]) -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_cmp"
[@@noalloc] [@@builtin]

external f32_round : (int[@untagged]) -> float32x8# -> float32x8#
  = "" "caml_avx_float32x8_round"
[@@noalloc] [@@builtin]

external f64_add : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_add"
[@@noalloc] [@@builtin]

external f64_sub : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_sub"
[@@noalloc] [@@builtin]

external f64_mul : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_mul"
[@@noalloc] [@@builtin]

external f64_div : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_div"
[@@noalloc] [@@builtin]

external f64_sqrt : float64x4# -> float64x4#
  = "" "caml_avx_float64x4_sqrt"
[@@noalloc] [@@builtin]

external f64_min : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_min"
[@@noalloc] [@@builtin]

external f64_max : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_max"
[@@noalloc] [@@builtin]

external f64_addsub : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_addsub"
[@@noalloc] [@@builtin]

external f64_hadd : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x2x2_hadd"
[@@noalloc] [@@builtin]

external f64_hsub : float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x2x2_hsub"
[@@noalloc] [@@builtin]

external f64_cmp : (int[@untagged]) -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_cmp"
[@@noalloc] [@@builtin]

external f64_round : (int[@untagged]) -> float64x4# -> float64x4#
  = "" "caml_avx_float64x4_round"
[@@noalloc] [@@builtin]

let[@inline never] test_f32_add a b = f32_add a b
let[@inline never] test_f32_sub a b = f32_sub a b
let[@inline never] test_f32_mul a b = f32_mul a b
let[@inline never] test_f32_div a b = f32_div a b
let[@inline never] test_f32_sqrt a = f32_sqrt a
let[@inline never] test_f32_min a b = f32_min a b
let[@inline never] test_f32_max a b = f32_max a b
let[@inline never] test_f32_addsub a b = f32_addsub a b
let[@inline never] test_f32_hadd a b = f32_hadd a b
let[@inline never] test_f32_hsub a b = f32_hsub a b
let[@inline never] test_f32_cmp a b = f32_cmp 1 a b
let[@inline never] test_f32_round a = f32_round 0x8 a

let[@inline never] test_f64_add a b = f64_add a b
let[@inline never] test_f64_sub a b = f64_sub a b
let[@inline never] test_f64_mul a b = f64_mul a b
let[@inline never] test_f64_div a b = f64_div a b
let[@inline never] test_f64_sqrt a = f64_sqrt a
let[@inline never] test_f64_min a b = f64_min a b
let[@inline never] test_f64_max a b = f64_max a b
let[@inline never] test_f64_addsub a b = f64_addsub a b
let[@inline never] test_f64_hadd a b = f64_hadd a b
let[@inline never] test_f64_hsub a b = f64_hsub a b
let[@inline never] test_f64_cmp a b = f64_cmp 1 a b
let[@inline never] test_f64_round a = f64_round 0x8 a
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'fadd < 8 x float >' "$ir"
grep -q 'fsub < 8 x float >' "$ir"
grep -q 'fmul < 8 x float >' "$ir"
grep -q 'fdiv < 8 x float >' "$ir"
grep -q '@llvm\.sqrt\.v8f32' "$ir"
grep -q '@llvm\.roundeven\.v8f32' "$ir"
grep -q 'fcmp olt < 8 x float >' "$ir"
grep -q 'fcmp ogt < 8 x float >' "$ir"
grep -q 'sext < 8 x i1 >' "$ir"

grep -q 'fadd < 4 x double >' "$ir"
grep -q 'fsub < 4 x double >' "$ir"
grep -q 'fmul < 4 x double >' "$ir"
grep -q 'fdiv < 4 x double >' "$ir"
grep -q '@llvm\.sqrt\.v4f64' "$ir"
grep -q '@llvm\.roundeven\.v4f64' "$ir"
grep -q 'fcmp olt < 4 x double >' "$ir"
grep -q 'fcmp ogt < 4 x double >' "$ir"
grep -q 'sext < 4 x i1 >' "$ir"

if grep -Eq 'llvm\.x86\.avx\.(min|max|rcp|rsqrt)' "$ir"; then
  echo "unexpected target-feature-specific AVX float intrinsic in generated IR" >&2
  exit 1
fi
