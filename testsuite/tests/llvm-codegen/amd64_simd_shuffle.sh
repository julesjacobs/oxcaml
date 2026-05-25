#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_shuffle_generated.ml"
out="$build_dir/amd64_simd_shuffle_generated.o"
ir="$build_dir/amd64_simd_shuffle_generated.ll"

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
external shuffle_64 : (int[@untagged]) -> int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse2_vec128_shuffle_64"
[@@noalloc] [@@builtin]

external shuffle_high_16 : (int[@untagged]) -> int16x8# -> int16x8#
  = "" "caml_sse2_vec128_shuffle_high_16"
[@@noalloc] [@@builtin]

external shuffle_low_16 : (int[@untagged]) -> int16x8# -> int16x8#
  = "" "caml_sse2_vec128_shuffle_low_16"
[@@noalloc] [@@builtin]

let[@inline never] test_shuffle_64 a b = shuffle_64 2 a b
let[@inline never] test_shuffle_high_16 a = shuffle_high_16 0b11100100 a
let[@inline never] test_shuffle_low_16 a = shuffle_low_16 0b00011011 a
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'extractelement < 2 x i64 > .* i64 0' "$ir"
grep -q 'extractelement < 2 x i64 > .* i64 1' "$ir"
grep -q 'insertelement < 2 x i64 > .* i64 1' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 0' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 3' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 4' "$ir"
grep -q 'extractelement < 8 x i16 > .* i64 7' "$ir"
grep -q 'insertelement < 8 x i16 > .* i64 7' "$ir"
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 16
