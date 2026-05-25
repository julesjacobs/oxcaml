#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_shuffle_32_generated.ml"
out="$build_dir/amd64_simd_shuffle_32_generated.o"
ir="$build_dir/amd64_simd_shuffle_32_generated.ll"

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
external shuffle_32 : (int[@untagged]) -> int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse_vec128_shuffle_32"
[@@noalloc] [@@builtin]

let[@inline never] test_shuffle_32 a b = shuffle_32 0b11100100 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'extractelement < 4 x i32 > .* i64 0' "$ir"
grep -q 'extractelement < 4 x i32 > .* i64 1' "$ir"
grep -q 'extractelement < 4 x i32 > .* i64 2' "$ir"
grep -q 'extractelement < 4 x i32 > .* i64 3' "$ir"
grep -q 'insertelement < 4 x i32 > .* i64 0' "$ir"
grep -q 'insertelement < 4 x i32 > .* i64 3' "$ir"
test "$(grep -c 'insertelement < 4 x i32 >' "$ir")" -ge 4
