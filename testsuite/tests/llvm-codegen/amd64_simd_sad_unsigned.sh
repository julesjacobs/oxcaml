#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sad_unsigned_generated.ml"
out="$build_dir/amd64_simd_sad_unsigned_generated.o"
ir="$build_dir/amd64_simd_sad_unsigned_generated.ll"

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
external sad_u8x16 : int8x16# -> int8x16# -> int64x2#
  = "" "caml_sse2_int8x16_sad_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_sad_u8x16 a b = sad_u8x16 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'zext < 16 x i8 > .* to < 16 x i16 >' "$ir"
grep -q 'icmp ugt i16' "$ir"
grep -q 'select i1' "$ir"
grep -q 'sub i16' "$ir"
grep -q 'zext i16 .* to i64' "$ir"
test "$(grep -c 'add i64' "$ir")" -ge 16
grep -q 'insertelement < 2 x i64 > .* i64 0' "$ir"
grep -q 'insertelement < 2 x i64 > .* i64 1' "$ir"
