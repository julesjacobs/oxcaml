#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse3_dup_generated.ml"
out="$build_dir/amd64_simd_sse3_dup_generated.o"
ir="$build_dir/amd64_simd_sse3_dup_generated.ll"

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
external dup_low_64 : int64x2# -> int64x2#
  = "" "caml_sse3_vec128_dup_low_64"
[@@noalloc] [@@builtin]

external dup_odd_32 : int32x4# -> int32x4#
  = "" "caml_sse3_vec128_dup_odd_32"
[@@noalloc] [@@builtin]

external dup_even_32 : int32x4# -> int32x4#
  = "" "caml_sse3_vec128_dup_even_32"
[@@noalloc] [@@builtin]

let[@inline never] test_dup_low_64 v = dup_low_64 v
let[@inline never] test_dup_odd_32 v = dup_odd_32 v
let[@inline never] test_dup_even_32 v = dup_even_32 v
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
grep -q 'insertelement < 2 x i64 > .* i64 1' "$ir"
test "$(grep -c 'extractelement < 4 x i32 > .* i64 1' "$ir")" -ge 2
test "$(grep -c 'extractelement < 4 x i32 > .* i64 3' "$ir")" -ge 2
test "$(grep -c 'extractelement < 4 x i32 > .* i64 0' "$ir")" -ge 2
test "$(grep -c 'extractelement < 4 x i32 > .* i64 2' "$ir")" -ge 2
grep -q 'insertelement < 4 x i32 > .* i64 3' "$ir"
