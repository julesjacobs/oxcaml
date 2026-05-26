#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_float_cmp_generated.ml"
out="$build_dir/amd64_simd_float_cmp_generated.o"
ir="$build_dir/amd64_simd_float_cmp_generated.ll"

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
external cmp_f32 : (int[@untagged]) -> float32x4# -> float32x4# -> int32x4#
  = "" "caml_sse_float32x4_cmp"
[@@noalloc] [@@builtin]

external cmp_f64 : (int[@untagged]) -> float64x2# -> float64x2# -> int64x2#
  = "" "caml_sse2_float64x2_cmp"
[@@noalloc] [@@builtin]

let[@inline never] test_f32_eq a b = cmp_f32 0 a b
let[@inline never] test_f32_lt a b = cmp_f32 1 a b
let[@inline never] test_f32_le a b = cmp_f32 2 a b
let[@inline never] test_f32_unord a b = cmp_f32 3 a b
let[@inline never] test_f32_neq a b = cmp_f32 4 a b
let[@inline never] test_f32_nlt a b = cmp_f32 5 a b
let[@inline never] test_f32_nle a b = cmp_f32 6 a b
let[@inline never] test_f32_ord a b = cmp_f32 7 a b

let[@inline never] test_f64_eq a b = cmp_f64 0 a b
let[@inline never] test_f64_lt a b = cmp_f64 1 a b
let[@inline never] test_f64_le a b = cmp_f64 2 a b
let[@inline never] test_f64_unord a b = cmp_f64 3 a b
let[@inline never] test_f64_neq a b = cmp_f64 4 a b
let[@inline never] test_f64_nlt a b = cmp_f64 5 a b
let[@inline never] test_f64_nle a b = cmp_f64 6 a b
let[@inline never] test_f64_ord a b = cmp_f64 7 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

for pred in oeq olt ole uno une uge ugt ord; do
  grep -q "fcmp $pred < 4 x float >" "$ir"
  grep -q "fcmp $pred < 2 x double >" "$ir"
done
grep -q 'sext < 4 x i1 > .* to < 4 x i32 >' "$ir"
grep -q 'sext < 2 x i1 > .* to < 2 x i64 >' "$ir"
