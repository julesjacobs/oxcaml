#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_byte_shift_generated.ml"
out="$build_dir/amd64_simd_byte_shift_generated.o"
ir="$build_dir/amd64_simd_byte_shift_generated.ll"

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
external shift_left_bytes : (int[@untagged]) -> int8x16# -> int8x16#
  = "" "caml_sse2_vec128_shift_left_bytes"
[@@noalloc] [@@builtin]

external shift_right_bytes : (int[@untagged]) -> int8x16# -> int8x16#
  = "" "caml_sse2_vec128_shift_right_bytes"
[@@noalloc] [@@builtin]

let[@inline never] test_shift_left_bytes v = shift_left_bytes 3 v
let[@inline never] test_shift_right_bytes v = shift_right_bytes 5 v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'extractelement < 16 x i8 > .* i64 0' "$ir"
grep -q 'extractelement < 16 x i8 > .* i64 12' "$ir"
grep -q 'extractelement < 16 x i8 > .* i64 15' "$ir"
test "$(grep -c 'insertelement < 16 x i8 > .* i8 0' "$ir")" -ge 8
grep -q 'insertelement < 16 x i8 > .* i64 15' "$ir"
