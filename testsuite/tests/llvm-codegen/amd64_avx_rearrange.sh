#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_avx_rearrange_generated.ml"
out="$build_dir/amd64_avx_rearrange_generated.o"
ir="$build_dir/amd64_avx_rearrange_generated.ll"

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
external broadcast64 : int64x2# -> int64x4#
  = "" "caml_avx_vec256_broadcast_64"
[@@noalloc] [@@builtin]

external broadcast32 : int32x4# -> int32x8#
  = "" "caml_avx_vec256_broadcast_32"
[@@noalloc] [@@builtin]

external broadcast32_128 : int32x4# -> int32x4#
  = "" "caml_avx_vec128_broadcast_32"
[@@noalloc] [@@builtin]

external permute64_128 : (int[@untagged]) -> int64x2# -> int64x2#
  = "" "caml_avx_vec128_permute_64"
[@@noalloc] [@@builtin]

external permute64 : (int[@untagged]) -> int64x4# -> int64x4#
  = "" "caml_avx_vec128x2_permute_64"
[@@noalloc] [@@builtin]

external permute32_128 : (int[@untagged]) -> int32x4# -> int32x4#
  = "" "caml_avx_vec128_permute_32"
[@@noalloc] [@@builtin]

external permute32 : (int[@untagged]) -> int32x8# -> int32x8#
  = "" "caml_avx_vec128x2_permute_32"
[@@noalloc] [@@builtin]

external permute2_128 : (int[@untagged]) -> int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec256_permute2_128"
[@@noalloc] [@@builtin]

external shuffle64 : (int[@untagged]) -> int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec128x2_shuffle_64"
[@@noalloc] [@@builtin]

external shuffle32 : (int[@untagged]) -> int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx_vec128x2_shuffle_32"
[@@noalloc] [@@builtin]

external interleave_high64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec128x2_interleave_high_64"
[@@noalloc] [@@builtin]

external interleave_low64 : int64x4# -> int64x4# -> int64x4#
  = "" "caml_avx_vec128x2_interleave_low_64"
[@@noalloc] [@@builtin]

external interleave_high32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx_vec128x2_interleave_high_32"
[@@noalloc] [@@builtin]

external interleave_low32 : int32x8# -> int32x8# -> int32x8#
  = "" "caml_avx_vec128x2_interleave_low_32"
[@@noalloc] [@@builtin]

let[@inline never] use_broadcast64 a = broadcast64 a
let[@inline never] use_broadcast32 a = broadcast32 a
let[@inline never] use_broadcast32_128 a = broadcast32_128 a
let[@inline never] use_permute64_128 a = permute64_128 1 a
let[@inline never] use_permute64 a = permute64 0x5 a
let[@inline never] use_permute32_128 a = permute32_128 0x1b a
let[@inline never] use_permute32 a = permute32 0x1b a
let[@inline never] use_permute2_128 a b = permute2_128 0x31 a b
let[@inline never] use_shuffle64 a b = shuffle64 0x5 a b
let[@inline never] use_shuffle32 a b = shuffle32 0x1b a b
let[@inline never] use_interleave_high64 a b = interleave_high64 a b
let[@inline never] use_interleave_low64 a b = interleave_low64 a b
let[@inline never] use_interleave_high32 a b = interleave_high32 a b
let[@inline never] use_interleave_low32 a b = interleave_low32 a b
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
grep -q 'extractelement < 4 x i64 >' "$ir"
grep -q 'extractelement < 4 x i32 >' "$ir"
grep -q 'extractelement < 8 x i32 >' "$ir"
grep -q 'insertelement < 4 x i64 >' "$ir"
grep -q 'insertelement < 8 x i32 >' "$ir"
grep -q 'insertelement < 4 x i32 >' "$ir"

if grep -q 'llvm\.x86\.avx' "$ir"; then
  echo "unexpected target-feature-specific AVX intrinsic in generated IR" >&2
  exit 1
fi
