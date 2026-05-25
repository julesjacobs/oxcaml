#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_move64_generated.ml"
out="$build_dir/amd64_simd_move64_generated.o"
ir="$build_dir/amd64_simd_move64_generated.ll"

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
external high_64_to_low_64 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_high_64_to_low_64"
[@@noalloc] [@@builtin]

external low_64_to_high_64 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse_vec128_low_64_to_high_64"
[@@noalloc] [@@builtin]

let[@inline never] test_high_64_to_low_64 a b = high_64_to_low_64 a b
let[@inline never] test_low_64_to_high_64 a b = low_64_to_high_64 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 2 x i64 > .* i64 0' "$ir")" -ge 2
test "$(grep -c 'extractelement < 2 x i64 > .* i64 1' "$ir")" -ge 2
test "$(grep -c 'insertelement < 2 x i64 > .* i64 0' "$ir")" -ge 2
test "$(grep -c 'insertelement < 2 x i64 > .* i64 1' "$ir")" -ge 2
