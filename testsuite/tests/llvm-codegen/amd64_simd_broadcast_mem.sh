#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_broadcast_mem_generated.ml"
out="$build_dir/amd64_simd_broadcast_mem_generated.o"
ir="$build_dir/amd64_simd_broadcast_mem_generated.ll"

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
external broadcast128 : nativeint# -> int64x4#
  = "" "caml_avx_vec256_load_broadcast128"
[@@noalloc] [@@builtin]

external broadcast64 : nativeint# -> int64x4#
  = "" "caml_avx_vec256_load_broadcast64"
[@@noalloc] [@@builtin]

external broadcast32x8 : nativeint# -> int32x8#
  = "" "caml_avx_vec256_load_broadcast32"
[@@noalloc] [@@builtin]

external broadcast32x4 : nativeint# -> int32x4#
  = "" "caml_avx_vec128_load_broadcast32"
[@@noalloc] [@@builtin]

let[@inline never] test_broadcast128 p = broadcast128 p
let[@inline never] test_broadcast64 p = broadcast64 p
let[@inline never] test_broadcast32x8 p = broadcast32x8 p
let[@inline never] test_broadcast32x4 p = broadcast32x4 p
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'load < 2 x i64 >, ptr .* align 1' "$ir"
grep -q 'load i64, ptr .* align 1' "$ir"
test "$(grep -c 'load i32, ptr .* align 1' "$ir")" -ge 2
grep -q 'insertelement < 4 x i64 > .* i64 3' "$ir"
grep -q 'insertelement < 8 x i32 > .* i64 7' "$ir"
grep -q 'insertelement < 4 x i32 > .* i64 3' "$ir"
