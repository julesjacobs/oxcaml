#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_int_shift_variable_generated.ml"
out="$build_dir/amd64_simd_int_shift_variable_generated.o"
ir="$build_dir/amd64_simd_int_shift_variable_generated.ll"

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
external sll_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_sll"
[@@noalloc] [@@builtin]

external sll_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_sll"
[@@noalloc] [@@builtin]

external sll_i64x2 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse2_int64x2_sll"
[@@noalloc] [@@builtin]

external srl_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_srl"
[@@noalloc] [@@builtin]

external srl_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_srl"
[@@noalloc] [@@builtin]

external srl_i64x2 : int64x2# -> int64x2# -> int64x2#
  = "" "caml_sse2_int64x2_srl"
[@@noalloc] [@@builtin]

external sra_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_sra"
[@@noalloc] [@@builtin]

external sra_i32x4 : int32x4# -> int32x4# -> int32x4#
  = "" "caml_sse2_int32x4_sra"
[@@noalloc] [@@builtin]

let[@inline never] test_sll_i16x8 v c = sll_i16x8 v c
let[@inline never] test_sll_i32x4 v c = sll_i32x4 v c
let[@inline never] test_sll_i64x2 v c = sll_i64x2 v c
let[@inline never] test_srl_i16x8 v c = srl_i16x8 v c
let[@inline never] test_srl_i32x4 v c = srl_i32x4 v c
let[@inline never] test_srl_i64x2 v c = srl_i64x2 v c
let[@inline never] test_sra_i16x8 v c = sra_i16x8 v c
let[@inline never] test_sra_i32x4 v c = sra_i32x4 v c
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'extractelement < 2 x i64 >' "$ir"
grep -q 'icmp uge i64 .* 16' "$ir"
grep -q 'icmp uge i64 .* 32' "$ir"
grep -q 'icmp uge i64 .* 64' "$ir"
grep -q 'select i1 .* i64 0' "$ir"
grep -q 'select i1 .* i64 15' "$ir"
grep -q 'select i1 .* i64 31' "$ir"
grep -q 'insertelement < 8 x i16 >' "$ir"
grep -q 'insertelement < 4 x i32 >' "$ir"
grep -q 'insertelement < 2 x i64 >' "$ir"
grep -q 'shl < 8 x i16 >' "$ir"
grep -q 'shl < 4 x i32 >' "$ir"
grep -q 'shl < 2 x i64 >' "$ir"
grep -q 'lshr < 8 x i16 >' "$ir"
grep -q 'lshr < 4 x i32 >' "$ir"
grep -q 'lshr < 2 x i64 >' "$ir"
grep -q 'ashr < 8 x i16 >' "$ir"
grep -q 'ashr < 4 x i32 >' "$ir"
