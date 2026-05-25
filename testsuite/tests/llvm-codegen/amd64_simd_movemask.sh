#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_movemask_generated.ml"
out="$build_dir/amd64_simd_movemask_generated.o"
ir="$build_dir/amd64_simd_movemask_generated.ll"

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
external movemask8 : int8x16# -> (int[@untagged])
  = "" "caml_sse2_vec128_movemask_8"
[@@noalloc] [@@builtin]

external movemask32 : int32x4# -> (int[@untagged])
  = "" "caml_sse_vec128_movemask_32"
[@@noalloc] [@@builtin]

external movemask64 : int64x2# -> (int[@untagged])
  = "" "caml_sse2_vec128_movemask_64"
[@@noalloc] [@@builtin]

let[@inline never] test_movemask8 v = movemask8 v
let[@inline never] test_movemask32 v = movemask32 v
let[@inline never] test_movemask64 v = movemask64 v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'extractelement < 16 x i8 >' "$ir"
grep -q 'extractelement < 4 x i32 >' "$ir"
grep -q 'extractelement < 2 x i64 >' "$ir"
grep -q 'lshr i8 .* 7' "$ir"
grep -q 'lshr i32 .* 31' "$ir"
grep -q 'lshr i64 .* 63' "$ir"
grep -q 'zext i8 .* to i64' "$ir"
grep -q 'zext i32 .* to i64' "$ir"
grep -q 'shl i64 .* 15' "$ir"
grep -q 'shl i64 .* 3' "$ir"
grep -q 'or i64' "$ir"
