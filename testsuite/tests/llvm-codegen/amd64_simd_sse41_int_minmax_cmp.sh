#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_int_minmax_cmp_generated.ml"
out="$build_dir/amd64_simd_sse41_int_minmax_cmp_generated.o"
ir="$build_dir/amd64_simd_sse41_int_minmax_cmp_generated.ll"

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
external cmpeq_i64 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse41_int64x2_cmpeq"
[@@noalloc] [@@builtin]

external max_i8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse41_int8x16_max"
[@@noalloc] [@@builtin]

external max_i32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_int32x4_max"
[@@noalloc] [@@builtin]

external max_u16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse41_int16x8_max_unsigned"
[@@noalloc] [@@builtin]

external max_u32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_int32x4_max_unsigned"
[@@noalloc] [@@builtin]

external min_i8 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse41_int8x16_min"
[@@noalloc] [@@builtin]

external min_i32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_int32x4_min"
[@@noalloc] [@@builtin]

external min_u16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse41_int16x8_min_unsigned"
[@@noalloc] [@@builtin]

external min_u32 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse41_int32x4_min_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_cmpeq_i64 a b = cmpeq_i64 a b
let[@inline never] test_max_i8 a b = max_i8 a b
let[@inline never] test_max_i32 a b = max_i32 a b
let[@inline never] test_max_u16 a b = max_u16 a b
let[@inline never] test_max_u32 a b = max_u32 a b
let[@inline never] test_min_i8 a b = min_i8 a b
let[@inline never] test_min_i32 a b = min_i32 a b
let[@inline never] test_min_u16 a b = min_u16 a b
let[@inline never] test_min_u32 a b = min_u32 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'icmp eq < 2 x i64 >' "$ir"
grep -q 'sext < 2 x i1 > .* to < 2 x i64 >' "$ir"
grep -q 'icmp sgt < 16 x i8 >' "$ir"
grep -q 'icmp slt < 16 x i8 >' "$ir"
grep -q 'icmp sgt < 4 x i32 >' "$ir"
grep -q 'icmp slt < 4 x i32 >' "$ir"
grep -q 'icmp ugt < 8 x i16 >' "$ir"
grep -q 'icmp ult < 8 x i16 >' "$ir"
grep -q 'icmp ugt < 4 x i32 >' "$ir"
grep -q 'icmp ult < 4 x i32 >' "$ir"
test "$(grep -c 'select < .* x i1 >' "$ir")" -ge 8
