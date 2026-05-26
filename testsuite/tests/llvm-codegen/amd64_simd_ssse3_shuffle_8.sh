#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_ssse3_shuffle_8_generated.ml"
out="$build_dir/amd64_simd_ssse3_shuffle_8_generated.o"
ir="$build_dir/amd64_simd_ssse3_shuffle_8_generated.ll"

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
external shuffle_8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_ssse3_vec128_shuffle_8"
[@@noalloc] [@@builtin]

let[@inline never] test_shuffle_8 v mask = shuffle_8 v mask
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 16 x i8 >' "$ir")" -ge 32
test "$(grep -c 'icmp slt i8' "$ir")" -ge 16
test "$(grep -c 'and i8 .*15' "$ir")" -ge 16
test "$(grep -c 'zext i8 .* to i32' "$ir")" -ge 16
test "$(grep -c 'select i1 .* i8 0' "$ir")" -ge 16
test "$(grep -c 'insertelement < 16 x i8 >' "$ir")" -ge 16
