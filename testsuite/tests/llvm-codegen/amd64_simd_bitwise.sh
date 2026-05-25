#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_bitwise_generated.ml"
out="$build_dir/amd64_simd_bitwise_generated.o"
ir="$build_dir/amd64_simd_bitwise_generated.ll"

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
external vec_and : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_and"
[@@noalloc] [@@builtin]

external vec_andnot : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_andnot"
[@@noalloc] [@@builtin]

external vec_or : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_or"
[@@noalloc] [@@builtin]

external vec_xor : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_xor"
[@@noalloc] [@@builtin]

let[@inline never] test_and a b = vec_and a b
let[@inline never] test_andnot a b = vec_andnot a b
let[@inline never] test_or a b = vec_or a b
let[@inline never] test_xor a b = vec_xor a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'and < 2 x i64 >' "$ir"
grep -q 'or < 2 x i64 >' "$ir"
grep -q 'xor < 2 x i64 >' "$ir"
grep -q '<i64 -1, i64 -1>' "$ir"
