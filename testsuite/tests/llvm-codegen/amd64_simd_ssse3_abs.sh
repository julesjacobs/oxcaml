#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_ssse3_abs_generated.ml"
out="$build_dir/amd64_simd_ssse3_abs_generated.o"
ir="$build_dir/amd64_simd_ssse3_abs_generated.ll"

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
external abs_i8 : int8x16# -> int8x16#
  = "" "caml_ssse3_int8x16_abs"
[@@noalloc] [@@builtin]

external abs_i16 : int16x8# -> int16x8#
  = "" "caml_ssse3_int16x8_abs"
[@@noalloc] [@@builtin]

external abs_i32 : int32x4# -> int32x4#
  = "" "caml_ssse3_int32x4_abs"
[@@noalloc] [@@builtin]

let[@inline never] test_abs_i8 v = abs_i8 v
let[@inline never] test_abs_i16 v = abs_i16 v
let[@inline never] test_abs_i32 v = abs_i32 v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q '@llvm.abs.v16i8' "$ir"
grep -q '@llvm.abs.v8i16' "$ir"
grep -q '@llvm.abs.v4i32' "$ir"
test "$(grep -c 'i1 0' "$ir")" -ge 3
