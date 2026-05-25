#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_interleave_8_16_generated.ml"
out="$build_dir/amd64_simd_interleave_8_16_generated.o"
ir="$build_dir/amd64_simd_interleave_8_16_generated.ll"

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
external interleave_high_8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_vec128_interleave_high_8"
[@@noalloc] [@@builtin]

external interleave_low_8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_vec128_interleave_low_8"
[@@noalloc] [@@builtin]

external interleave_high_16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_vec128_interleave_high_16"
[@@noalloc] [@@builtin]

external interleave_low_16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_vec128_interleave_low_16"
[@@noalloc] [@@builtin]

let[@inline never] test_interleave_high_8 a b = interleave_high_8 a b
let[@inline never] test_interleave_low_8 a b = interleave_low_8 a b
let[@inline never] test_interleave_high_16 a b = interleave_high_16 a b
let[@inline never] test_interleave_low_16 a b = interleave_low_16 a b
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
grep -q 'extractelement < 16 x i8 > .* i64 8' "$ir"
grep -q 'insertelement < 16 x i8 > .* i64 15' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 0' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 4' "$ir"
grep -q 'insertelement < 8 x i16 > .* i64 7' "$ir"
test "$(grep -c 'insertelement < 16 x i8 >' "$ir")" -ge 32
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 16
