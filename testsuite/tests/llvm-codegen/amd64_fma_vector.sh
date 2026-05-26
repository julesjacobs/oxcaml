#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_fma_vector_generated.ml"
out="$build_dir/amd64_fma_vector_generated.o"
ir="$build_dir/amd64_fma_vector_generated.ll"

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
external f64x2_mul_add : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_mul_add"
[@@noalloc] [@@builtin]

external f64x2_mul_sub : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_mul_sub"
[@@noalloc] [@@builtin]

external f64x2_mul_addsub : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_mul_addsub"
[@@noalloc] [@@builtin]

external f64x2_mul_subadd : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_mul_subadd"
[@@noalloc] [@@builtin]

external f64x2_neg_mul_add : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_neg_mul_add"
[@@noalloc] [@@builtin]

external f64x2_neg_mul_sub : float64x2# -> float64x2# -> float64x2# -> float64x2#
  = "" "caml_fma_float64x2_neg_mul_sub"
[@@noalloc] [@@builtin]

external f32x4_mul_add : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_mul_add"
[@@noalloc] [@@builtin]

external f32x4_mul_sub : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_mul_sub"
[@@noalloc] [@@builtin]

external f32x4_mul_addsub : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_mul_addsub"
[@@noalloc] [@@builtin]

external f32x4_mul_subadd : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_mul_subadd"
[@@noalloc] [@@builtin]

external f32x4_neg_mul_add : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_neg_mul_add"
[@@noalloc] [@@builtin]

external f32x4_neg_mul_sub : float32x4# -> float32x4# -> float32x4# -> float32x4#
  = "" "caml_fma_float32x4_neg_mul_sub"
[@@noalloc] [@@builtin]

external f64x4_mul_add : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_mul_add"
[@@noalloc] [@@builtin]

external f64x4_mul_sub : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_mul_sub"
[@@noalloc] [@@builtin]

external f64x4_mul_addsub : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_mul_addsub"
[@@noalloc] [@@builtin]

external f64x4_mul_subadd : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_mul_subadd"
[@@noalloc] [@@builtin]

external f64x4_neg_mul_add : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_neg_mul_add"
[@@noalloc] [@@builtin]

external f64x4_neg_mul_sub : float64x4# -> float64x4# -> float64x4# -> float64x4#
  = "" "caml_fma_float64x4_neg_mul_sub"
[@@noalloc] [@@builtin]

external f32x8_mul_add : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_mul_add"
[@@noalloc] [@@builtin]

external f32x8_mul_sub : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_mul_sub"
[@@noalloc] [@@builtin]

external f32x8_mul_addsub : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_mul_addsub"
[@@noalloc] [@@builtin]

external f32x8_mul_subadd : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_mul_subadd"
[@@noalloc] [@@builtin]

external f32x8_neg_mul_add : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_neg_mul_add"
[@@noalloc] [@@builtin]

external f32x8_neg_mul_sub : float32x8# -> float32x8# -> float32x8# -> float32x8#
  = "" "caml_fma_float32x8_neg_mul_sub"
[@@noalloc] [@@builtin]

let[@inline never] test_f64x2_mul_add a b c = f64x2_mul_add a b c
let[@inline never] test_f64x2_mul_sub a b c = f64x2_mul_sub a b c
let[@inline never] test_f64x2_mul_addsub a b c = f64x2_mul_addsub a b c
let[@inline never] test_f64x2_mul_subadd a b c = f64x2_mul_subadd a b c
let[@inline never] test_f64x2_neg_mul_add a b c = f64x2_neg_mul_add a b c
let[@inline never] test_f64x2_neg_mul_sub a b c = f64x2_neg_mul_sub a b c

let[@inline never] test_f32x4_mul_add a b c = f32x4_mul_add a b c
let[@inline never] test_f32x4_mul_sub a b c = f32x4_mul_sub a b c
let[@inline never] test_f32x4_mul_addsub a b c = f32x4_mul_addsub a b c
let[@inline never] test_f32x4_mul_subadd a b c = f32x4_mul_subadd a b c
let[@inline never] test_f32x4_neg_mul_add a b c = f32x4_neg_mul_add a b c
let[@inline never] test_f32x4_neg_mul_sub a b c = f32x4_neg_mul_sub a b c

let[@inline never] test_f64x4_mul_add a b c = f64x4_mul_add a b c
let[@inline never] test_f64x4_mul_sub a b c = f64x4_mul_sub a b c
let[@inline never] test_f64x4_mul_addsub a b c = f64x4_mul_addsub a b c
let[@inline never] test_f64x4_mul_subadd a b c = f64x4_mul_subadd a b c
let[@inline never] test_f64x4_neg_mul_add a b c = f64x4_neg_mul_add a b c
let[@inline never] test_f64x4_neg_mul_sub a b c = f64x4_neg_mul_sub a b c

let[@inline never] test_f32x8_mul_add a b c = f32x8_mul_add a b c
let[@inline never] test_f32x8_mul_sub a b c = f32x8_mul_sub a b c
let[@inline never] test_f32x8_mul_addsub a b c = f32x8_mul_addsub a b c
let[@inline never] test_f32x8_mul_subadd a b c = f32x8_mul_subadd a b c
let[@inline never] test_f32x8_neg_mul_add a b c = f32x8_neg_mul_add a b c
let[@inline never] test_f32x8_neg_mul_sub a b c = f32x8_neg_mul_sub a b c
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c '@llvm\.fma\.v2f64' "$ir")" -ge 6
test "$(grep -c '@llvm\.fma\.v4f32' "$ir")" -ge 6
test "$(grep -c '@llvm\.fma\.v4f64' "$ir")" -ge 6
test "$(grep -c '@llvm\.fma\.v8f32' "$ir")" -ge 6
grep -q 'fneg < 2 x double >' "$ir"
grep -q 'fneg < 4 x float >' "$ir"
grep -q 'fneg < 4 x double >' "$ir"
grep -q 'fneg < 8 x float >' "$ir"

if grep -Eq 'llvm\.x86\.fma' "$ir"; then
  echo "unexpected target-feature-specific FMA intrinsic in generated IR" >&2
  exit 1
fi
