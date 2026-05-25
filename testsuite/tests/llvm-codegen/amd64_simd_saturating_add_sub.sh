#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_saturating_add_sub_generated.ml"
out="$build_dir/amd64_simd_saturating_add_sub_generated.o"
ir="$build_dir/amd64_simd_saturating_add_sub_generated.ll"

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
external sadd_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_add_saturating"
[@@noalloc] [@@builtin]

external sadd_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_add_saturating"
[@@noalloc] [@@builtin]

external uadd_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_add_saturating_unsigned"
[@@noalloc] [@@builtin]

external uadd_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_add_saturating_unsigned"
[@@noalloc] [@@builtin]

external ssub_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_sub_saturating"
[@@noalloc] [@@builtin]

external ssub_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_sub_saturating"
[@@noalloc] [@@builtin]

external usub_i8x16 : int8x16# -> int8x16# -> int8x16#
  = "" "caml_sse2_int8x16_sub_saturating_unsigned"
[@@noalloc] [@@builtin]

external usub_i16x8 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_sse2_int16x8_sub_saturating_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_sadd_i8x16 a b = sadd_i8x16 a b
let[@inline never] test_sadd_i16x8 a b = sadd_i16x8 a b
let[@inline never] test_uadd_i8x16 a b = uadd_i8x16 a b
let[@inline never] test_uadd_i16x8 a b = uadd_i16x8 a b
let[@inline never] test_ssub_i8x16 a b = ssub_i8x16 a b
let[@inline never] test_ssub_i16x8 a b = ssub_i16x8 a b
let[@inline never] test_usub_i8x16 a b = usub_i8x16 a b
let[@inline never] test_usub_i16x8 a b = usub_i16x8 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'call  *< 16 x i8 > @llvm.sadd.sat.v16i8' "$ir"
grep -q 'call  *< 8 x i16 > @llvm.sadd.sat.v8i16' "$ir"
grep -q 'call  *< 16 x i8 > @llvm.uadd.sat.v16i8' "$ir"
grep -q 'call  *< 8 x i16 > @llvm.uadd.sat.v8i16' "$ir"
grep -q 'call  *< 16 x i8 > @llvm.ssub.sat.v16i8' "$ir"
grep -q 'call  *< 8 x i16 > @llvm.ssub.sat.v8i16' "$ir"
grep -q 'call  *< 16 x i8 > @llvm.usub.sat.v16i8' "$ir"
grep -q 'call  *< 8 x i16 > @llvm.usub.sat.v8i16' "$ir"
