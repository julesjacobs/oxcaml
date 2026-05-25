#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_low64_mem_generated.ml"
out="$build_dir/amd64_simd_low64_mem_generated.o"
ir="$build_dir/amd64_simd_low64_mem_generated.ll"

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

external load_low64 : nativeint# -> int64x2#
  = "" "caml_sse2_vec128_load_low64"
[@@noalloc] [@@builtin]

external load_low64_copy_high64 : int64x2# -> nativeint# -> int64x2#
  = "" "caml_sse2_vec128_load_low64_copy_high64"
[@@noalloc] [@@builtin]

external load_high64_copy_low64 : int64x2# -> nativeint# -> int64x2#
  = "" "caml_sse2_vec128_load_high64_copy_low64"
[@@noalloc] [@@builtin]

external load_zero_low64 : nativeint# -> int64x2#
  = "" "caml_sse2_vec128_load_zero_low64"
[@@noalloc] [@@builtin]

external load_broadcast64 : nativeint# -> int64x2#
  = "" "caml_sse3_vec128_load_broadcast64"
[@@noalloc] [@@builtin]

external store_low64 : nativeint# -> int64x2# -> void
  = "" "caml_sse2_vec128_store_low64"
[@@noalloc] [@@builtin]

let[@inline never] test_load_low p = load_low64 p
let[@inline never] test_load_zero p = load_zero_low64 p
let[@inline never] test_load_copy_low v p = load_low64_copy_high64 v p
let[@inline never] test_load_copy_high v p = load_high64_copy_low64 v p
let[@inline never] test_load_broadcast p = load_broadcast64 p

let[@inline never] test_store p v =
  let _ = store_low64 p v in
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

test "$(grep -c 'load i64, ptr .* align 1' "$ir")" -ge 5
grep -q 'store i64 .* align 1' "$ir"
grep -q 'extractelement < 2 x i64 > .* i64 0' "$ir"
grep -q 'insertelement < 2 x i64 > .* i64 0' "$ir"
grep -q 'insertelement < 2 x i64 > .* i64 1' "$ir"
