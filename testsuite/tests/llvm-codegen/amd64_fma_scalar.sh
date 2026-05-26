#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_fma_scalar_generated.ml"
stub="$build_dir/amd64_fma_scalar_stubs.c"
out="$build_dir/amd64_fma_scalar_generated.o"
exe="$build_dir/amd64_fma_scalar_generated.exe"
ir="$build_dir/amd64_fma_scalar_generated.ll"

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
external f32_of_float : float -> float32 = "%float32offloat"
external float_of_f32 : float32 -> float = "%floatoffloat32"

external f64_mul_add : float -> float -> float -> float
  = "" "caml_fma_float64_mul_add"
[@@noalloc] [@@unboxed] [@@builtin]

external f64_mul_sub : float -> float -> float -> float
  = "" "caml_fma_float64_mul_sub"
[@@noalloc] [@@unboxed] [@@builtin]

external f64_neg_mul_add : float -> float -> float -> float
  = "" "caml_fma_float64_neg_mul_add"
[@@noalloc] [@@unboxed] [@@builtin]

external f64_neg_mul_sub : float -> float -> float -> float
  = "" "caml_fma_float64_neg_mul_sub"
[@@noalloc] [@@unboxed] [@@builtin]

external f32_mul_add : float32 -> float32 -> float32 -> float32
  = "" "caml_fma_float32_mul_add"
[@@noalloc] [@@unboxed] [@@builtin]

external f32_mul_sub : float32 -> float32 -> float32 -> float32
  = "" "caml_fma_float32_mul_sub"
[@@noalloc] [@@unboxed] [@@builtin]

external f32_neg_mul_add : float32 -> float32 -> float32 -> float32
  = "" "caml_fma_float32_neg_mul_add"
[@@noalloc] [@@unboxed] [@@builtin]

external f32_neg_mul_sub : float32 -> float32 -> float32 -> float32
  = "" "caml_fma_float32_neg_mul_sub"
[@@noalloc] [@@unboxed] [@@builtin]

let[@inline never] test_f64_mul_add a b c = f64_mul_add a b c
let[@inline never] test_f64_mul_sub a b c = f64_mul_sub a b c
let[@inline never] test_f64_neg_mul_add a b c = f64_neg_mul_add a b c
let[@inline never] test_f64_neg_mul_sub a b c = f64_neg_mul_sub a b c

let[@inline never] test_f32_mul_add a b c = f32_mul_add a b c
let[@inline never] test_f32_mul_sub a b c = f32_mul_sub a b c
let[@inline never] test_f32_neg_mul_add a b c = f32_neg_mul_add a b c
let[@inline never] test_f32_neg_mul_sub a b c = f32_neg_mul_sub a b c

let eq_float name actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "%s: %.17g <> %.17g" name actual expected)

let eq_f32 name actual expected =
  eq_float name (float_of_f32 actual) (float_of_f32 expected)

let f32 x = f32_of_float x

let () =
  eq_float "f64_mul_add" (test_f64_mul_add 2.0 3.0 5.0) 11.0;
  eq_float "f64_mul_sub" (test_f64_mul_sub 2.0 3.0 5.0) 1.0;
  eq_float "f64_neg_mul_add" (test_f64_neg_mul_add 2.0 3.0 5.0) (-1.0);
  eq_float "f64_neg_mul_sub" (test_f64_neg_mul_sub 2.0 3.0 5.0) (-11.0);
  eq_f32 "f32_mul_add"
    (test_f32_mul_add (f32 2.0) (f32 3.0) (f32 5.0))
    (f32 11.0);
  eq_f32 "f32_mul_sub"
    (test_f32_mul_sub (f32 2.0) (f32 3.0) (f32 5.0))
    (f32 1.0);
  eq_f32 "f32_neg_mul_add"
    (test_f32_neg_mul_add (f32 2.0) (f32 3.0) (f32 5.0))
    (f32 (-1.0));
  eq_f32 "f32_neg_mul_sub"
    (test_f32_neg_mul_sub (f32 2.0) (f32 3.0) (f32 5.0))
    (f32 (-11.0))
EOF

cat > "$stub" <<'EOF'
#include <caml/mlvalues.h>

#define STUB(name) CAMLprim value name(value unit) { return Val_unit; }

STUB(caml_fma_float64_mul_add)
STUB(caml_fma_float64_mul_sub)
STUB(caml_fma_float64_neg_mul_add)
STUB(caml_fma_float64_neg_mul_sub)
STUB(caml_fma_float32_mul_add)
STUB(caml_fma_float32_mul_sub)
STUB(caml_fma_float32_neg_mul_add)
STUB(caml_fma_float32_neg_mul_sub)
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c '@llvm\.fma\.f64' "$ir")" -ge 4
test "$(grep -c '@llvm\.fma\.f32' "$ir")" -ge 4

if grep -Eq 'llvm\.x86\.fma' "$ir"; then
  echo "unexpected target-feature-specific FMA intrinsic in generated IR" >&2
  exit 1
fi

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$stub" "$src"
"$exe"
