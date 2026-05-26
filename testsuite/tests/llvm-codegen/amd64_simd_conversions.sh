#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_conversions_generated.ml"
out="$build_dir/amd64_simd_conversions_generated.o"
ir="$build_dir/amd64_simd_conversions_generated.ll"

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
external cvt_i32_f64 : int32x4# -> float64x2#
  = "" "caml_sse2_cvt_int32x4_float64x2"
[@@noalloc] [@@builtin]

external cvt_i32_f32 : int32x4# -> float32x4#
  = "" "caml_sse2_cvt_int32x4_float32x4"
[@@noalloc] [@@builtin]

external cvt_f64_i32 : float64x2# -> int32x4#
  = "" "caml_sse2_cvt_float64x2_int32x2"
[@@noalloc] [@@builtin]

external cvtt_f64_i32 : float64x2# -> int32x4#
  = "" "caml_sse2_cvtt_float64x2_int32x2"
[@@noalloc] [@@builtin]

external cvt_f64_f32 : float64x2# -> float32x4#
  = "" "caml_sse2_cvt_float64x2_float32x2"
[@@noalloc] [@@builtin]

external cvt_f32_i32 : float32x4# -> int32x4#
  = "" "caml_sse2_cvt_float32x4_int32x4"
[@@noalloc] [@@builtin]

external cvtt_f32_i32 : float32x4# -> int32x4#
  = "" "caml_sse2_cvtt_float32x4_int32x4"
[@@noalloc] [@@builtin]

external cvt_f32_f64 : float32x4# -> float64x2#
  = "" "caml_sse2_cvt_float32x4_float64x2"
[@@noalloc] [@@builtin]

let[@inline never] test_cvt_i32_f64 x = cvt_i32_f64 x
let[@inline never] test_cvt_i32_f32 x = cvt_i32_f32 x
let[@inline never] test_cvt_f64_i32 x = cvt_f64_i32 x
let[@inline never] test_cvtt_f64_i32 x = cvtt_f64_i32 x
let[@inline never] test_cvt_f64_f32 x = cvt_f64_f32 x
let[@inline never] test_cvt_f32_i32 x = cvt_f32_i32 x
let[@inline never] test_cvtt_f32_i32 x = cvtt_f32_i32 x
let[@inline never] test_cvt_f32_f64 x = cvt_f32_f64 x
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'sitofp < 2 x i32 > .* to < 2 x double >' "$ir"
grep -q 'sitofp < 4 x i32 > .* to < 4 x float >' "$ir"
grep -q 'fpext < 2 x float > .* to < 2 x double >' "$ir"
grep -q '@llvm.x86.sse2.cvtpd2dq' "$ir"
grep -q '@llvm.x86.sse2.cvttpd2dq' "$ir"
grep -q '@llvm.x86.sse2.cvtpd2ps' "$ir"
grep -q '@llvm.x86.sse2.cvtps2dq' "$ir"
grep -q '@llvm.x86.sse2.cvttps2dq' "$ir"
