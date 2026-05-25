#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_interleave_32_generated.ml"
out="$build_dir/amd64_simd_interleave_32_generated.o"
ir="$build_dir/amd64_simd_interleave_32_generated.ll"

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
external interleave_high_32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse_vec128_interleave_high_32"
[@@noalloc] [@@builtin]

external interleave_low_32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse_vec128_interleave_low_32"
[@@noalloc] [@@builtin]

external interleave_low_32_alias : int32x4# -> int32x4# -> int32x4#
  = "" "caml_simd_vec128_interleave_low_32"
[@@noalloc] [@@builtin]

let[@inline never] test_interleave_high_32 a b = interleave_high_32 a b
let[@inline never] test_interleave_low_32 a b = interleave_low_32 a b
let[@inline never] test_interleave_low_32_alias a b = interleave_low_32_alias a b
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
grep -q 'insertelement < 4 x i32 > .* i64 3' "$ir"
test "$(grep -c 'insertelement < 4 x i32 >' "$ir")" -ge 12
