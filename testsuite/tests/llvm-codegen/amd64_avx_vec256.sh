#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_avx_vec256_generated.ml"
out="$build_dir/amd64_avx_vec256_generated.o"
ir="$build_dir/amd64_avx_vec256_generated.ll"

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
external testz : int64x4# -> int64x4# -> (int[@untagged])
  = "" "caml_avx_vec256_testz"
[@@noalloc] [@@builtin]

external testc : int64x4# -> int64x4# -> (int[@untagged])
  = "" "caml_avx_vec256_testc"
[@@noalloc] [@@builtin]

external testnzc : int64x4# -> int64x4# -> (int[@untagged])
  = "" "caml_avx_vec256_testnzc"
[@@noalloc] [@@builtin]

external bitwise_and : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_and"
[@@noalloc] [@@builtin]

external andnot : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_andnot"
[@@noalloc] [@@builtin]

external bitwise_or : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_or"
[@@noalloc] [@@builtin]

external bitwise_xor : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_xor"
[@@noalloc] [@@builtin]

external blend_64 : (int[@untagged]) -> int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_blend_64"
[@@noalloc] [@@builtin]

external blend_32 : (int[@untagged]) -> int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx_vec256_blend_32"
[@@noalloc] [@@builtin]

external blendv_64 : int64x4# -> int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_blendv_64"
[@@noalloc] [@@builtin]

external blendv_32 : int32x8# -> int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx_vec256_blendv_32"
[@@noalloc] [@@builtin]

external dup_even_64 : int64x4# -> int64x4#
  = "" "caml_avx_vec256_dup_even_64"
[@@noalloc] [@@builtin]

external dup_odd_32 : int32x8# -> int32x8#
  = "" "caml_avx_vec256_dup_odd_32"
[@@noalloc] [@@builtin]

external dup_even_32 : int32x8# -> int32x8#
  = "" "caml_avx_vec256_dup_even_32"
[@@noalloc] [@@builtin]

external movemask_64 : int64x4# -> (int[@untagged])
  = "" "caml_avx_vec256_movemask_64"
[@@noalloc] [@@builtin]

external movemask_32 : int32x8# -> (int[@untagged])
  = "" "caml_avx_vec256_movemask_32"
[@@noalloc] [@@builtin]

let[@inline never] use_testz a b = testz a b
let[@inline never] use_testc a b = testc a b
let[@inline never] use_testnzc a b = testnzc a b
let[@inline never] use_and a b = bitwise_and a b
let[@inline never] use_andnot a b = andnot a b
let[@inline never] use_or a b = bitwise_or a b
let[@inline never] use_xor a b = bitwise_xor a b
let[@inline never] use_blend_64 a b = blend_64 5 a b
let[@inline never] use_blend_32 a b = blend_32 0x55 a b
let[@inline never] use_blendv_64 a b mask = blendv_64 a b mask
let[@inline never] use_blendv_32 a b mask = blendv_32 a b mask
let[@inline never] use_dup_even_64 a = dup_even_64 a
let[@inline never] use_dup_odd_32 a = dup_odd_32 a
let[@inline never] use_dup_even_32 a = dup_even_32 a
let[@inline never] use_movemask_64 a = movemask_64 a
let[@inline never] use_movemask_32 a = movemask_32 a
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'and < 4 x i64 >' "$ir"
grep -q 'xor < 4 x i64 >' "$ir"
grep -q 'or < 4 x i64 >' "$ir"
grep -q 'select < 4 x i1 >' "$ir"
grep -q 'select < 8 x i1 >' "$ir"
grep -q 'extractelement < 4 x i64 >' "$ir"
grep -q 'extractelement < 8 x i32 >' "$ir"
grep -q 'icmp eq i64' "$ir"
grep -q 'icmp ne i64' "$ir"
grep -q 'zext i1' "$ir"

if grep -q 'llvm\.x86\.avx' "$ir"; then
  echo "unexpected target-feature-specific AVX intrinsic in generated IR" >&2
  exit 1
fi
