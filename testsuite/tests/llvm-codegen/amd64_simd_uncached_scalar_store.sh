#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_uncached_scalar_store_generated.ml"
out="$build_dir/amd64_simd_uncached_scalar_store_generated.o"
ir="$build_dir/amd64_simd_uncached_scalar_store_generated.ll"

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

external store_int32_uncached : nativeint# -> int32# -> void
  = "" "caml_sse2_int32_store_uncached"
[@@noalloc] [@@builtin]

external store_int64_uncached : nativeint# -> int64# -> void
  = "" "caml_sse2_int64_store_uncached"
[@@noalloc] [@@builtin]

let[@inline never] test_store_int32 p v =
  let _ = store_int32_uncached p v in
  ()

let[@inline never] test_store_int64 p v =
  let _ = store_int64_uncached p v in
  ()
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'store  *i32' "$ir"
grep -q 'store  *i64' "$ir"
grep -q 'trunc i64 .* to i32' "$ir"
