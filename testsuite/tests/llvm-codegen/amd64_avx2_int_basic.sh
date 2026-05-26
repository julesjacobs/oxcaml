#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_avx2_int_basic_generated.ml"
out="$build_dir/amd64_avx2_int_basic_generated.o"
ir="$build_dir/amd64_avx2_int_basic_generated.ll"

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
external add8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_add"
[@@noalloc] [@@builtin]

external add16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_add"
[@@noalloc] [@@builtin]

external add32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_add"
[@@noalloc] [@@builtin]

external add64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx2_int64x4_add"
[@@noalloc] [@@builtin]

external sub8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_sub"
[@@noalloc] [@@builtin]

external sub16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_sub"
[@@noalloc] [@@builtin]

external sub32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_sub"
[@@noalloc] [@@builtin]

external sub64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx2_int64x4_sub"
[@@noalloc] [@@builtin]

external adds8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_add_saturating"
[@@noalloc] [@@builtin]

external adds16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_add_saturating"
[@@noalloc] [@@builtin]

external addu8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_add_saturating_unsigned"
[@@noalloc] [@@builtin]

external addu16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_add_saturating_unsigned"
[@@noalloc] [@@builtin]

external subs8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_sub_saturating"
[@@noalloc] [@@builtin]

external subs16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_sub_saturating"
[@@noalloc] [@@builtin]

external subu8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_sub_saturating_unsigned"
[@@noalloc] [@@builtin]

external subu16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_sub_saturating_unsigned"
[@@noalloc] [@@builtin]

external avg8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_avg_unsigned"
[@@noalloc] [@@builtin]

external avg16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_avg_unsigned"
[@@noalloc] [@@builtin]

external cmpeq8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_cmpeq"
[@@noalloc] [@@builtin]

external cmpeq16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_cmpeq"
[@@noalloc] [@@builtin]

external cmpeq32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_cmpeq"
[@@noalloc] [@@builtin]

external cmpeq64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx2_int64x4_cmpeq"
[@@noalloc] [@@builtin]

external cmpgt8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_cmpgt"
[@@noalloc] [@@builtin]

external cmpgt16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_cmpgt"
[@@noalloc] [@@builtin]

external cmpgt32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_cmpgt"
[@@noalloc] [@@builtin]

external cmpgt64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx2_int64x4_cmpgt"
[@@noalloc] [@@builtin]

external max8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_max"
[@@noalloc] [@@builtin]

external max16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_max"
[@@noalloc] [@@builtin]

external max32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_max"
[@@noalloc] [@@builtin]

external maxu8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_max_unsigned"
[@@noalloc] [@@builtin]

external maxu16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_max_unsigned"
[@@noalloc] [@@builtin]

external maxu32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_max_unsigned"
[@@noalloc] [@@builtin]

external min8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_min"
[@@noalloc] [@@builtin]

external min16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_min"
[@@noalloc] [@@builtin]

external min32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_min"
[@@noalloc] [@@builtin]

external minu8 : int8x32# -> int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_min_unsigned"
[@@noalloc] [@@builtin]

external minu16 : int16x16# -> int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_min_unsigned"
[@@noalloc] [@@builtin]

external minu32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_min_unsigned"
[@@noalloc] [@@builtin]

external abs8 : int8x32# -> int8x32#
  = "" "caml_avx2_int8x32_abs"
[@@noalloc] [@@builtin]

external abs16 : int16x16# -> int16x16#
  = "" "caml_avx2_int16x16_abs"
[@@noalloc] [@@builtin]

external abs32 : int32x8# -> int32x8#
  = "" "caml_avx2_int32x8_abs"
[@@noalloc] [@@builtin]

let[@inline never] use_add8 a b = add8 a b
let[@inline never] use_add16 a b = add16 a b
let[@inline never] use_add32 a b = add32 a b
let[@inline never] use_add64 a b = add64 a b
let[@inline never] use_sub8 a b = sub8 a b
let[@inline never] use_sub16 a b = sub16 a b
let[@inline never] use_sub32 a b = sub32 a b
let[@inline never] use_sub64 a b = sub64 a b
let[@inline never] use_adds8 a b = adds8 a b
let[@inline never] use_adds16 a b = adds16 a b
let[@inline never] use_addu8 a b = addu8 a b
let[@inline never] use_addu16 a b = addu16 a b
let[@inline never] use_subs8 a b = subs8 a b
let[@inline never] use_subs16 a b = subs16 a b
let[@inline never] use_subu8 a b = subu8 a b
let[@inline never] use_subu16 a b = subu16 a b
let[@inline never] use_avg8 a b = avg8 a b
let[@inline never] use_avg16 a b = avg16 a b
let[@inline never] use_cmpeq8 a b = cmpeq8 a b
let[@inline never] use_cmpeq16 a b = cmpeq16 a b
let[@inline never] use_cmpeq32 a b = cmpeq32 a b
let[@inline never] use_cmpeq64 a b = cmpeq64 a b
let[@inline never] use_cmpgt8 a b = cmpgt8 a b
let[@inline never] use_cmpgt16 a b = cmpgt16 a b
let[@inline never] use_cmpgt32 a b = cmpgt32 a b
let[@inline never] use_cmpgt64 a b = cmpgt64 a b
let[@inline never] use_max8 a b = max8 a b
let[@inline never] use_max16 a b = max16 a b
let[@inline never] use_max32 a b = max32 a b
let[@inline never] use_maxu8 a b = maxu8 a b
let[@inline never] use_maxu16 a b = maxu16 a b
let[@inline never] use_maxu32 a b = maxu32 a b
let[@inline never] use_min8 a b = min8 a b
let[@inline never] use_min16 a b = min16 a b
let[@inline never] use_min32 a b = min32 a b
let[@inline never] use_minu8 a b = minu8 a b
let[@inline never] use_minu16 a b = minu16 a b
let[@inline never] use_minu32 a b = minu32 a b
let[@inline never] use_abs8 a = abs8 a
let[@inline never] use_abs16 a = abs16 a
let[@inline never] use_abs32 a = abs32 a
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'add < 32 x i8 >' "$ir"
grep -q 'add < 16 x i16 >' "$ir"
grep -q 'add < 8 x i32 >' "$ir"
grep -q 'add < 4 x i64 >' "$ir"
grep -q 'sub < 32 x i8 >' "$ir"
grep -q 'sub < 16 x i16 >' "$ir"
grep -q 'sub < 8 x i32 >' "$ir"
grep -q 'sub < 4 x i64 >' "$ir"
grep -q 'icmp eq < 32 x i8 >' "$ir"
grep -q 'icmp sgt < 16 x i16 >' "$ir"
grep -q 'icmp ugt < 8 x i32 >' "$ir"
grep -q 'icmp ult < 32 x i8 >' "$ir"
grep -q 'select < 32 x i1 >' "$ir"
grep -q 'select < 16 x i1 >' "$ir"
grep -q 'select < 8 x i1 >' "$ir"
grep -q '@llvm[.]abs[.]v32i8' "$ir"
grep -q '@llvm[.]abs[.]v16i16' "$ir"
grep -q '@llvm[.]abs[.]v8i32' "$ir"
grep -q '@llvm[.]sadd[.]sat[.]v32i8' "$ir"
grep -q '@llvm[.]uadd[.]sat[.]v16i16' "$ir"
grep -q '@llvm[.]ssub[.]sat[.]v32i8' "$ir"
grep -q '@llvm[.]usub[.]sat[.]v16i16' "$ir"
grep -q 'zext < 32 x i8 >' "$ir"
grep -q 'trunc < 32 x i16 >' "$ir"

if grep -q 'llvm[.]x86[.]avx2' "$ir"; then
  echo "unexpected target-feature-specific AVX2 intrinsic in generated IR" >&2
  exit 1
fi
