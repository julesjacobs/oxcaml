#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_mask8_store_generated.ml"
out="$build_dir/amd64_simd_mask8_store_generated.o"
ir="$build_dir/amd64_simd_mask8_store_generated.ll"

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

external store_mask8 : int8x16# -> int8x16# -> nativeint# -> void
  = "" "caml_sse2_vec128_store_mask8"
[@@noalloc] [@@builtin]

let[@inline never] test_store data mask p =
  let _ = store_mask8 data mask p in
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

grep -q 'call  *void @llvm.masked.store.v16i8.p0' "$ir"
grep -q 'icmp slt < 16 x i8 >' "$ir"
grep -q 'inttoptr i64 .* to ptr' "$ir"
