#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_pack_saturating_generated.ml"
out="$build_dir/amd64_simd_pack_saturating_generated.o"
ir="$build_dir/amd64_simd_pack_saturating_generated.ll"

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
external pack_i16_i8 : int16x8# -> int16x8# -> int8x16#
  = "" "caml_sse2_cvt_int16x8_int8x16_saturating"
[@@noalloc] [@@builtin]

external pack_i32_i16 : int32x4# -> int32x4# -> int16x8#
  = "" "caml_sse2_cvt_int32x4_int16x8_saturating"
[@@noalloc] [@@builtin]

external pack_u16_u8 : int16x8# -> int16x8# -> int8x16#
  = "" "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned"
[@@noalloc] [@@builtin]

external pack_u32_u16 : int32x4# -> int32x4# -> int16x8#
  = "" "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned"
[@@noalloc] [@@builtin]

let[@inline never] test_pack_i16_i8 a b = pack_i16_i8 a b
let[@inline never] test_pack_i32_i16 a b = pack_i32_i16 a b
let[@inline never] test_pack_u16_u8 a b = pack_u16_u8 a b
let[@inline never] test_pack_u32_u16 a b = pack_u32_u16 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'icmp slt < 8 x i16 >' "$ir"
grep -q 'icmp sgt < 8 x i16 >' "$ir"
grep -q 'icmp slt < 4 x i32 >' "$ir"
grep -q 'icmp sgt < 4 x i32 >' "$ir"
grep -q '<i16 -128' "$ir"
grep -q '<i16 255' "$ir"
grep -q '<i32 -32768' "$ir"
grep -q '<i32 65535' "$ir"
grep -q 'trunc i16 .* to i8' "$ir"
grep -q 'trunc i32 .* to i16' "$ir"
grep -q 'insertelement < 16 x i8 >' "$ir"
grep -q 'insertelement < 8 x i16 >' "$ir"
