#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_insert_extract_generated.ml"
out="$build_dir/amd64_simd_sse41_insert_extract_generated.o"
ir="$build_dir/amd64_simd_sse41_insert_extract_generated.ll"

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
external extract_8 : (int[@untagged]) -> int8x16# -> (int[@untagged])
  = "" "caml_sse41_int8x16_extract"
[@@noalloc] [@@builtin]

external extract_16 : (int[@untagged]) -> int16x8# -> (int[@untagged])
  = "" "caml_sse41_int16x8_extract"
[@@noalloc] [@@builtin]

external extract_32 : (int[@untagged]) -> int32x4# -> int32#
  = "" "caml_sse41_int32x4_extract"
[@@noalloc] [@@builtin]

external extract_64 : (int[@untagged]) -> int64x2# -> int64#
  = "" "caml_sse41_int64x2_extract"
[@@noalloc] [@@builtin]

external insert_8 : (int[@untagged]) -> int8x16# -> (int[@untagged]) -> int8x16#
  = "" "caml_sse41_int8x16_insert"
[@@noalloc] [@@builtin]

external insert_16 : (int[@untagged]) -> int16x8# -> (int[@untagged]) -> int16x8#
  = "" "caml_sse41_int16x8_insert"
[@@noalloc] [@@builtin]

external insert_32 : (int[@untagged]) -> int32x4# -> int32# -> int32x4#
  = "" "caml_sse41_int32x4_insert"
[@@noalloc] [@@builtin]

external insert_64 : (int[@untagged]) -> int64x2# -> int64# -> int64x2#
  = "" "caml_sse41_int64x2_insert"
[@@noalloc] [@@builtin]

let[@inline never] test_extract_8 v = extract_8 7 v
let[@inline never] test_extract_16 v = extract_16 3 v
let[@inline never] test_extract_32 v = extract_32 2 v
let[@inline never] test_extract_64 v = extract_64 1 v

let[@inline never] test_insert_8 v x = insert_8 7 v x
let[@inline never] test_insert_16 v x = insert_16 3 v x
let[@inline never] test_insert_32 v x = insert_32 2 v x
let[@inline never] test_insert_64 v x = insert_64 1 v x
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'extractelement < 16 x i8 >' "$ir")" -ge 1
test "$(grep -c 'extractelement < 8 x i16 >' "$ir")" -ge 1
test "$(grep -c 'extractelement < 4 x i32 >' "$ir")" -ge 1
test "$(grep -c 'extractelement < 2 x i64 >' "$ir")" -ge 1
test "$(grep -c 'zext i8' "$ir")" -ge 1
test "$(grep -c 'zext i16' "$ir")" -ge 1
test "$(grep -c 'insertelement < 16 x i8 >' "$ir")" -ge 1
test "$(grep -c 'insertelement < 8 x i16 >' "$ir")" -ge 1
test "$(grep -c 'insertelement < 4 x i32 >' "$ir")" -ge 1
test "$(grep -c 'insertelement < 2 x i64 >' "$ir")" -ge 1
test "$(grep -c 'trunc i64' "$ir")" -ge 2
