#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_masked_mem_generated.ml"
out="$build_dir/amd64_simd_masked_mem_generated.o"
ir="$build_dir/amd64_simd_masked_mem_generated.ll"

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
type void : void

external load_mask64x2 : int64x2# -> nativeint# -> int64x2#
  = "" "caml_avx_vec128_load_mask64"
[@@noalloc] [@@builtin]

external load_mask64x4 : int64x4# -> nativeint# -> int64x4#
  = "" "caml_avx_vec256_load_mask64"
[@@noalloc] [@@builtin]

external load_mask32x4 : int32x4# -> nativeint# -> int32x4#
  = "" "caml_avx_vec128_load_mask32"
[@@noalloc] [@@builtin]

external load_mask32x8 : int32x8# -> nativeint# -> int32x8#
  = "" "caml_avx_vec256_load_mask32"
[@@noalloc] [@@builtin]

external store_mask64x2 : nativeint# -> int64x2# -> int64x2# -> void
  = "" "caml_avx_vec128_store_mask64"
[@@noalloc] [@@builtin]

external store_mask64x4 : nativeint# -> int64x4# -> int64x4# -> void
  = "" "caml_avx_vec256_store_mask64"
[@@noalloc] [@@builtin]

external store_mask32x4 : nativeint# -> int32x4# -> int32x4# -> void
  = "" "caml_avx_vec128_store_mask32"
[@@noalloc] [@@builtin]

external store_mask32x8 : nativeint# -> int32x8# -> int32x8# -> void
  = "" "caml_avx_vec256_store_mask32"
[@@noalloc] [@@builtin]

let[@inline never] test_load64x2 m p = load_mask64x2 m p
let[@inline never] test_load64x4 m p = load_mask64x4 m p
let[@inline never] test_load32x4 m p = load_mask32x4 m p
let[@inline never] test_load32x8 m p = load_mask32x8 m p

let[@inline never] test_store64x2 p v m =
  let _ = store_mask64x2 p v m in
  ()

let[@inline never] test_store64x4 p v m =
  let _ = store_mask64x4 p v m in
  ()

let[@inline never] test_store32x4 p v m =
  let _ = store_mask32x4 p v m in
  ()

let[@inline never] test_store32x8 p v m =
  let _ = store_mask32x8 p v m in
  ()
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -q 'call  *< 2 x i64 > @llvm.masked.load.v2i64.p0' "$ir"
grep -q 'call  *< 4 x i64 > @llvm.masked.load.v4i64.p0' "$ir"
grep -q 'call  *< 4 x i32 > @llvm.masked.load.v4i32.p0' "$ir"
grep -q 'call  *< 8 x i32 > @llvm.masked.load.v8i32.p0' "$ir"
grep -q 'call  *void @llvm.masked.store.v2i64.p0' "$ir"
grep -q 'call  *void @llvm.masked.store.v4i64.p0' "$ir"
grep -q 'call  *void @llvm.masked.store.v4i32.p0' "$ir"
grep -q 'call  *void @llvm.masked.store.v8i32.p0' "$ir"
grep -q 'icmp slt < 2 x i64 >' "$ir"
grep -q 'icmp slt < 4 x i64 >' "$ir"
grep -q 'icmp slt < 4 x i32 >' "$ir"
grep -q 'icmp slt < 8 x i32 >' "$ir"
