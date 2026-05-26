#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_ssse3_pmaddubsw_generated.ml"
out="$build_dir/amd64_simd_ssse3_pmaddubsw_generated.o"
ir="$build_dir/amd64_simd_ssse3_pmaddubsw_generated.ll"

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
external pmaddubsw : int8x16# -> int8x16# -> int16x8#
  = "" "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8"
[@@noalloc] [@@builtin]

let[@inline never] test_pmaddubsw a b = pmaddubsw a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'zext < 16 x i8 > .* to < 16 x i32 >' "$ir"
grep -q 'sext < 16 x i8 > .* to < 16 x i32 >' "$ir"
grep -q 'mul < 16 x i32 >' "$ir"
test "$(grep -c 'extractelement < 16 x i32 >' "$ir")" -ge 16
test "$(grep -c 'add i32' "$ir")" -ge 8
test "$(grep -c 'icmp slt i32 .* -32768' "$ir")" -ge 8
test "$(grep -c 'icmp sgt i32 .* 32767' "$ir")" -ge 8
test "$(grep -c 'trunc i32 .* to i16' "$ir")" -ge 8
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 8
