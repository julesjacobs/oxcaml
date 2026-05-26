#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_clmul_generated.ml"
out="$build_dir/amd64_simd_clmul_generated.o"
ir="$build_dir/amd64_simd_clmul_generated.ll"

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
external clmul : (int[@untagged]) -> int64x2# -> int64x2# -> int64x2#
  = "" "caml_clmul_int64x2"
[@@noalloc] [@@builtin]

let[@inline never] test_clmul_low_low a b = clmul 0x00 a b
let[@inline never] test_clmul_high_high a b = clmul 0x11 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 2 x i64 >' "$ir")" -ge 4
test "$(grep -c 'zext i64 .* to i128' "$ir")" -ge 4
test "$(grep -c 'lshr i128' "$ir")" -ge 128
test "$(grep -c 'shl i128' "$ir")" -ge 128
test "$(grep -c 'and i128' "$ir")" -ge 128
test "$(grep -c 'select i1' "$ir")" -ge 128
test "$(grep -c 'xor i128' "$ir")" -ge 128
test "$(grep -c 'trunc i128 .* to i64' "$ir")" -ge 4
test "$(grep -c 'insertelement < 2 x i64 >' "$ir")" -ge 4
