#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_int32_mul_generated.ml"
out="$build_dir/amd64_simd_sse41_int32_mul_generated.o"
ir="$build_dir/amd64_simd_sse41_int32_mul_generated.ll"

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
external mul_even : int32x4# -> int32x4# -> int64x2#
  = "" "caml_sse41_int32x4_mul_even"
[@@noalloc] [@@builtin]

external mul_low : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_int32x4_mul_low"
[@@noalloc] [@@builtin]

let[@inline never] test_mul_even a b = mul_even a b
let[@inline never] test_mul_low a b = mul_low a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 4 x i32 >' "$ir")" -ge 4
test "$(grep -c 'sext i32 .* to i64' "$ir")" -ge 4
test "$(grep -c 'mul i64' "$ir")" -ge 2
test "$(grep -c 'insertelement < 2 x i64 >' "$ir")" -ge 2
grep -q 'mul < 4 x i32 >' "$ir"
