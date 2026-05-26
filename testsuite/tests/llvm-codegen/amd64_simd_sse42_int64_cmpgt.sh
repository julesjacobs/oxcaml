#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse42_int64_cmpgt_generated.ml"
out="$build_dir/amd64_simd_sse42_int64_cmpgt_generated.o"
ir="$build_dir/amd64_simd_sse42_int64_cmpgt_generated.ll"

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
external cmpgt_i64 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse42_int64x2_cmpgt"
[@@noalloc] [@@builtin]

let[@inline never] test_cmpgt_i64 a b = cmpgt_i64 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'icmp sgt < 2 x i64 >' "$ir"
grep -q 'sext < 2 x i1 > .* to < 2 x i64 >' "$ir"
