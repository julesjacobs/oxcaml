#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_mem_generated.ml"
out="$build_dir/amd64_simd_mem_generated.o"
ir="$build_dir/amd64_simd_mem_generated.ll"

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
type void : void

external load128 : nativeint# -> int64x2#
  = "" "caml_sse_vec128_load_unaligned"
[@@noalloc] [@@builtin]

external store128 : nativeint# -> int64x2# -> void
  = "" "caml_sse_vec128_store_unaligned"
[@@noalloc] [@@builtin]

external load256 : nativeint# -> int64x4#
  = "" "caml_avx_vec256_load_unaligned"
[@@noalloc] [@@builtin]

external store256 : nativeint# -> int64x4# -> void
  = "" "caml_avx_vec256_store_unaligned"
[@@noalloc] [@@builtin]

let[@inline never] test128 p v =
  let _ = store128 p v in
  load128 p

let[@inline never] test256 p v =
  let _ = store256 p v in
  load256 p
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'store < 2 x i64 > .* align 1' "$ir"
grep -q 'load < 2 x i64 >, ptr .* align 1' "$ir"
grep -q 'store < 4 x i64 > .* align 1' "$ir"
grep -q 'load < 4 x i64 >, ptr .* align 1' "$ir"
