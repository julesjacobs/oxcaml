#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_int_extend_generated.ml"
out="$build_dir/amd64_simd_sse41_int_extend_generated.o"
ir="$build_dir/amd64_simd_sse41_int_extend_generated.ll"

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
external sx_i8_i16 : int8x16# -> int16x8#
  = "" "caml_sse41_cvtsx_int8x16_int16x8"
[@@noalloc] [@@builtin]

external sx_i8_i32 : int8x16# -> int32x4#
  = "" "caml_sse41_cvtsx_int8x16_int32x4"
[@@noalloc] [@@builtin]

external sx_i8_i64 : int8x16# -> int64x2#
  = "" "caml_sse41_cvtsx_int8x16_int64x2"
[@@noalloc] [@@builtin]

external sx_i16_i32 : int16x8# -> int32x4#
  = "" "caml_sse41_cvtsx_int16x8_int32x4"
[@@noalloc] [@@builtin]

external sx_i16_i64 : int16x8# -> int64x2#
  = "" "caml_sse41_cvtsx_int16x8_int64x2"
[@@noalloc] [@@builtin]

external sx_i32_i64 : int32x4# -> int64x2#
  = "" "caml_sse41_cvtsx_int32x4_int64x2"
[@@noalloc] [@@builtin]

external zx_i8_i16 : int8x16# -> int16x8#
  = "" "caml_sse41_cvtzx_int8x16_int16x8"
[@@noalloc] [@@builtin]

external zx_i8_i32 : int8x16# -> int32x4#
  = "" "caml_sse41_cvtzx_int8x16_int32x4"
[@@noalloc] [@@builtin]

external zx_i8_i64 : int8x16# -> int64x2#
  = "" "caml_sse41_cvtzx_int8x16_int64x2"
[@@noalloc] [@@builtin]

external zx_i16_i32 : int16x8# -> int32x4#
  = "" "caml_sse41_cvtzx_int16x8_int32x4"
[@@noalloc] [@@builtin]

external zx_i16_i64 : int16x8# -> int64x2#
  = "" "caml_sse41_cvtzx_int16x8_int64x2"
[@@noalloc] [@@builtin]

external zx_i32_i64 : int32x4# -> int64x2#
  = "" "caml_sse41_cvtzx_int32x4_int64x2"
[@@noalloc] [@@builtin]

let[@inline never] test_sx_i8_i16 v = sx_i8_i16 v
let[@inline never] test_sx_i8_i32 v = sx_i8_i32 v
let[@inline never] test_sx_i8_i64 v = sx_i8_i64 v
let[@inline never] test_sx_i16_i32 v = sx_i16_i32 v
let[@inline never] test_sx_i16_i64 v = sx_i16_i64 v
let[@inline never] test_sx_i32_i64 v = sx_i32_i64 v
let[@inline never] test_zx_i8_i16 v = zx_i8_i16 v
let[@inline never] test_zx_i8_i32 v = zx_i8_i32 v
let[@inline never] test_zx_i8_i64 v = zx_i8_i64 v
let[@inline never] test_zx_i16_i32 v = zx_i16_i32 v
let[@inline never] test_zx_i16_i64 v = zx_i16_i64 v
let[@inline never] test_zx_i32_i64 v = zx_i32_i64 v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'sext i8 .* to i16' "$ir"
grep -q 'sext i8 .* to i32' "$ir"
grep -q 'sext i8 .* to i64' "$ir"
grep -q 'sext i16 .* to i32' "$ir"
grep -q 'sext i16 .* to i64' "$ir"
grep -q 'sext i32 .* to i64' "$ir"
grep -q 'zext i8 .* to i16' "$ir"
grep -q 'zext i8 .* to i32' "$ir"
grep -q 'zext i8 .* to i64' "$ir"
grep -q 'zext i16 .* to i32' "$ir"
grep -q 'zext i16 .* to i64' "$ir"
grep -q 'zext i32 .* to i64' "$ir"
grep -q 'insertelement < 8 x i16 >' "$ir"
grep -q 'insertelement < 4 x i32 >' "$ir"
grep -q 'insertelement < 2 x i64 >' "$ir"
