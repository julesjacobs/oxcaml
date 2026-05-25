#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_gather_mem_generated.ml"
out="$build_dir/amd64_simd_gather_mem_generated.o"
ir="$build_dir/amd64_simd_gather_mem_generated.ll"

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
external gather32_i32x4 :
  scale:int64# -> onto:int32x4# -> nativeint# -> idx:int32x4# ->
  mask:int32x4# -> int32x4#
  = "" "caml_avx2_vec128_gather32_index32"
[@@noalloc] [@@builtin]

external gather32_i32x8 :
  scale:int64# -> onto:int32x8# -> nativeint# -> idx:int32x8# ->
  mask:int32x8# -> int32x8#
  = "" "caml_avx2_vec256_gather32_index32"
[@@noalloc] [@@builtin]

external gather32_i64x2 :
  scale:int64# -> onto:int32x4# -> nativeint# -> idx:int64x2# ->
  mask:int32x4# -> int32x4#
  = "" "caml_avx2_vec128_gather32_index64"
[@@noalloc] [@@builtin]

external gather32_i64x4 :
  scale:int64# -> onto:int32x4# -> nativeint# -> idx:int64x4# ->
  mask:int32x4# -> int32x4#
  = "" "caml_avx2_vec256_gather32_index64"
[@@noalloc] [@@builtin]

external gather64_i32x4_to_x2 :
  scale:int64# -> onto:int64x2# -> nativeint# -> idx:int32x4# ->
  mask:int64x2# -> int64x2#
  = "" "caml_avx2_vec128_gather64_index32"
[@@noalloc] [@@builtin]

external gather64_i32x4_to_x4 :
  scale:int64# -> onto:int64x4# -> nativeint# -> idx:int32x4# ->
  mask:int64x4# -> int64x4#
  = "" "caml_avx2_vec256_gather64_index32"
[@@noalloc] [@@builtin]

external gather64_i64x2 :
  scale:int64# -> onto:int64x2# -> nativeint# -> idx:int64x2# ->
  mask:int64x2# -> int64x2#
  = "" "caml_avx2_vec128_gather64_index64"
[@@noalloc] [@@builtin]

external gather64_i64x4 :
  scale:int64# -> onto:int64x4# -> nativeint# -> idx:int64x4# ->
  mask:int64x4# -> int64x4#
  = "" "caml_avx2_vec256_gather64_index64"
[@@noalloc] [@@builtin]

let[@inline never] test_gather32_i32x4 onto p idx mask =
  gather32_i32x4 ~scale:#4L ~onto p ~idx ~mask

let[@inline never] test_gather32_i32x8 onto p idx mask =
  gather32_i32x8 ~scale:#4L ~onto p ~idx ~mask

let[@inline never] test_gather32_i64x2 onto p idx mask =
  gather32_i64x2 ~scale:#4L ~onto p ~idx ~mask

let[@inline never] test_gather32_i64x4 onto p idx mask =
  gather32_i64x4 ~scale:#4L ~onto p ~idx ~mask

let[@inline never] test_gather64_i32x4_to_x2 onto p idx mask =
  gather64_i32x4_to_x2 ~scale:#8L ~onto p ~idx ~mask

let[@inline never] test_gather64_i32x4_to_x4 onto p idx mask =
  gather64_i32x4_to_x4 ~scale:#8L ~onto p ~idx ~mask

let[@inline never] test_gather64_i64x2 onto p idx mask =
  gather64_i64x2 ~scale:#8L ~onto p ~idx ~mask

let[@inline never] test_gather64_i64x4 onto p idx mask =
  gather64_i64x4 ~scale:#8L ~onto p ~idx ~mask
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'call  *< 4 x i32 > @llvm.masked.gather.v4i32.v4p0' "$ir"
grep -q 'call  *< 8 x i32 > @llvm.masked.gather.v8i32.v8p0' "$ir"
grep -q 'call  *< 2 x i32 > @llvm.masked.gather.v2i32.v2p0' "$ir"
grep -q 'call  *< 2 x i64 > @llvm.masked.gather.v2i64.v2p0' "$ir"
grep -q 'call  *< 4 x i64 > @llvm.masked.gather.v4i64.v4p0' "$ir"
grep -q 'icmp slt < 2 x i32 >' "$ir"
grep -q 'icmp slt < 4 x i32 >' "$ir"
grep -q 'icmp slt < 8 x i32 >' "$ir"
grep -q 'icmp slt < 2 x i64 >' "$ir"
grep -q 'icmp slt < 4 x i64 >' "$ir"
grep -q 'inttoptr i64' "$ir"
