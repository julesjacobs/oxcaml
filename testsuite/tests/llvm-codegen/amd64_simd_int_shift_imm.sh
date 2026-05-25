#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_int_shift_imm_generated.ml"
out="$build_dir/amd64_simd_int_shift_imm_generated.o"
ir="$build_dir/amd64_simd_int_shift_imm_generated.ll"

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
external slli_i16x8 : (int[@untagged]) -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_slli"
[@@noalloc] [@@builtin]

external slli_i32x4 : (int[@untagged]) -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_slli"
[@@noalloc] [@@builtin]

external slli_i64x2 : (int[@untagged]) -> int64x2# -> int64x2#
  = "" "caml_sse2_int64x2_slli"
[@@noalloc] [@@builtin]

external srli_i16x8 : (int[@untagged]) -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_srli"
[@@noalloc] [@@builtin]

external srli_i32x4 : (int[@untagged]) -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_srli"
[@@noalloc] [@@builtin]

external srli_i64x2 : (int[@untagged]) -> int64x2# -> int64x2#
  = "" "caml_sse2_int64x2_srli"
[@@noalloc] [@@builtin]

external srai_i16x8 : (int[@untagged]) -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_srai"
[@@noalloc] [@@builtin]

external srai_i32x4 : (int[@untagged]) -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_srai"
[@@noalloc] [@@builtin]

let[@inline never] test_slli_i16x8 v = slli_i16x8 1 v
let[@inline never] test_slli_i32x4 v = slli_i32x4 2 v
let[@inline never] test_slli_i64x2 v = slli_i64x2 3 v
let[@inline never] test_srli_i16x8 v = srli_i16x8 4 v
let[@inline never] test_srli_i32x4 v = srli_i32x4 5 v
let[@inline never] test_srli_i64x2 v = srli_i64x2 6 v
let[@inline never] test_srai_i16x8 v = srai_i16x8 7 v
let[@inline never] test_srai_i32x4 v = srai_i32x4 8 v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'shl < 8 x i16 >' "$ir"
grep -q 'shl < 4 x i32 >' "$ir"
grep -q 'shl < 2 x i64 >' "$ir"
grep -q 'lshr < 8 x i16 >' "$ir"
grep -q 'lshr < 4 x i32 >' "$ir"
grep -q 'lshr < 2 x i64 >' "$ir"
grep -q 'ashr < 8 x i16 >' "$ir"
grep -q 'ashr < 4 x i32 >' "$ir"
grep -q '<i16 1' "$ir"
grep -q '<i32 2' "$ir"
grep -q '<i64 3' "$ir"
