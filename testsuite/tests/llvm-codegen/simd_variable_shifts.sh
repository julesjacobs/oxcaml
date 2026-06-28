#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/simd_variable_shifts_generated.ml"
obj="$build_dir/simd_variable_shifts_generated.o"
cmx="$build_dir/simd_variable_shifts_generated.cmx"
ir="$build_dir/simd_variable_shifts_generated.ll"
stub_src="$build_dir/simd_variable_shifts_stubs.c"
stub_obj="$build_dir/simd_variable_shifts_stubs.o"
out="$build_dir/simd_variable_shifts_generated.exe"
stdout_file="$build_dir/simd_variable_shifts_stdout.txt"

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
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

cat > "$src" <<'EOF'
external i64_low_of : int64 -> int64x2
  = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external i64_extract : (int[@untagged]) -> (int64x2[@unboxed]) -> (int64[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int64x2_extract"
[@@noalloc] [@@builtin]

external i64_insert :
  (int[@untagged]) -> (int64x2[@unboxed]) -> (int64[@unboxed]) -> (int64x2[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int64x2_insert"
[@@noalloc] [@@builtin]

external i64_ushl : int64x2 -> int64x2 -> int64x2
  = "caml_vec128_unreachable" "caml_neon_int64x2_ushl"
[@@noalloc] [@@unboxed] [@@builtin]

external i64_sshl : int64x2 -> int64x2 -> int64x2
  = "caml_vec128_unreachable" "caml_neon_int64x2_sshl"
[@@noalloc] [@@unboxed] [@@builtin]

external i32_low_of : int32 -> int32x4
  = "caml_vec128_unreachable" "caml_int32x4_low_of_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external i32_extract : (int[@untagged]) -> (int32x4[@unboxed]) -> (int32[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int32x4_extract"
[@@noalloc] [@@builtin]

external i32_insert :
  (int[@untagged]) -> (int32x4[@unboxed]) -> (int32[@unboxed]) -> (int32x4[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int32x4_insert"
[@@noalloc] [@@builtin]

external i32_ushl : int32x4 -> int32x4 -> int32x4
  = "caml_vec128_unreachable" "caml_neon_int32x4_ushl"
[@@noalloc] [@@unboxed] [@@builtin]

external i32_sshl : int32x4 -> int32x4 -> int32x4
  = "caml_vec128_unreachable" "caml_neon_int32x4_sshl"
[@@noalloc] [@@unboxed] [@@builtin]

external i16_low_of : (int[@untagged]) -> (int16x8[@unboxed])
  = "caml_vec128_unreachable" "caml_int16x8_low_of_int16"
[@@noalloc] [@@builtin]

external i16_extract : (int[@untagged]) -> (int16x8[@unboxed]) -> (int[@untagged])
  = "caml_vec128_unreachable" "caml_neon_int16x8_extract"
[@@noalloc] [@@builtin]

external i16_insert :
  (int[@untagged]) -> (int16x8[@unboxed]) -> (int[@untagged]) -> (int16x8[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int16x8_insert"
[@@noalloc] [@@builtin]

external i16_ushl : int16x8 -> int16x8 -> int16x8
  = "caml_vec128_unreachable" "caml_neon_int16x8_ushl"
[@@noalloc] [@@unboxed] [@@builtin]

external i16_sshl : int16x8 -> int16x8 -> int16x8
  = "caml_vec128_unreachable" "caml_neon_int16x8_sshl"
[@@noalloc] [@@unboxed] [@@builtin]

external i8_low_of : (int[@untagged]) -> (int8x16[@unboxed])
  = "caml_vec128_unreachable" "caml_int8x16_low_of_int8"
[@@noalloc] [@@builtin]

external i8_extract : (int[@untagged]) -> (int8x16[@unboxed]) -> (int[@untagged])
  = "caml_vec128_unreachable" "caml_neon_int8x16_extract"
[@@noalloc] [@@builtin]

external i8_insert :
  (int[@untagged]) -> (int8x16[@unboxed]) -> (int[@untagged]) -> (int8x16[@unboxed])
  = "caml_vec128_unreachable" "caml_neon_int8x16_insert"
[@@noalloc] [@@builtin]

external i8_ushl : int8x16 -> int8x16 -> int8x16
  = "caml_vec128_unreachable" "caml_neon_int8x16_ushl"
[@@noalloc] [@@unboxed] [@@builtin]

external i8_sshl : int8x16 -> int8x16 -> int8x16
  = "caml_vec128_unreachable" "caml_neon_int8x16_sshl"
[@@noalloc] [@@unboxed] [@@builtin]

let i64x2 a b = i64_insert 1 (i64_low_of a) b

let i32x4 a b c d =
  let v = i32_low_of a in
  let v = i32_insert 1 v b in
  let v = i32_insert 2 v c in
  i32_insert 3 v d

let i16x8 a b c d e f g h =
  let v = i16_low_of a in
  let v = i16_insert 1 v b in
  let v = i16_insert 2 v c in
  let v = i16_insert 3 v d in
  let v = i16_insert 4 v e in
  let v = i16_insert 5 v f in
  let v = i16_insert 6 v g in
  i16_insert 7 v h

let i8x16 a b c d e f g h i j k l m n o p =
  let v = i8_low_of a in
  let v = i8_insert 1 v b in
  let v = i8_insert 2 v c in
  let v = i8_insert 3 v d in
  let v = i8_insert 4 v e in
  let v = i8_insert 5 v f in
  let v = i8_insert 6 v g in
  let v = i8_insert 7 v h in
  let v = i8_insert 8 v i in
  let v = i8_insert 9 v j in
  let v = i8_insert 10 v k in
  let v = i8_insert 11 v l in
  let v = i8_insert 12 v m in
  let v = i8_insert 13 v n in
  let v = i8_insert 14 v o in
  i8_insert 15 v p

let fail label expected actual =
  Printf.ksprintf failwith "%s: expected %s, got %s" label expected actual

let check_i64 label actual expected0 expected1 =
  let actual0 = i64_extract 0 actual in
  let actual1 = i64_extract 1 actual in
  if not (Int64.equal actual0 expected0)
  then fail (label ^ "[0]") (Printf.sprintf "0x%Lx" expected0) (Printf.sprintf "0x%Lx" actual0);
  if not (Int64.equal actual1 expected1)
  then fail (label ^ "[1]") (Printf.sprintf "0x%Lx" expected1) (Printf.sprintf "0x%Lx" actual1)

let check_i32_lane label i expected actual =
  if not (Int32.equal actual expected)
  then
    fail (Printf.sprintf "%s[%d]" label i)
      (Printf.sprintf "0x%lx" expected)
      (Printf.sprintf "0x%lx" actual)

let check_i32 label actual expected0 expected1 expected2 expected3 =
  check_i32_lane label 0 expected0 (i32_extract 0 actual);
  check_i32_lane label 1 expected1 (i32_extract 1 actual);
  check_i32_lane label 2 expected2 (i32_extract 2 actual);
  check_i32_lane label 3 expected3 (i32_extract 3 actual)

let check_int_lane mask label i expected actual =
  let actual = actual land mask in
  if actual <> expected
  then
    fail (Printf.sprintf "%s[%d]" label i)
      (Printf.sprintf "0x%x" expected)
      (Printf.sprintf "0x%x" actual)

let check_i16 label actual expected0 expected1 expected2 expected3 expected4 expected5
    expected6 expected7 =
  check_int_lane 0xffff label 0 expected0 (i16_extract 0 actual);
  check_int_lane 0xffff label 1 expected1 (i16_extract 1 actual);
  check_int_lane 0xffff label 2 expected2 (i16_extract 2 actual);
  check_int_lane 0xffff label 3 expected3 (i16_extract 3 actual);
  check_int_lane 0xffff label 4 expected4 (i16_extract 4 actual);
  check_int_lane 0xffff label 5 expected5 (i16_extract 5 actual);
  check_int_lane 0xffff label 6 expected6 (i16_extract 6 actual);
  check_int_lane 0xffff label 7 expected7 (i16_extract 7 actual)

let check_i8 label actual expected0 expected1 expected2 expected3 expected4 expected5
    expected6 expected7 expected8 expected9 expected10 expected11 expected12
    expected13 expected14 expected15 =
  check_int_lane 0xff label 0 expected0 (i8_extract 0 actual);
  check_int_lane 0xff label 1 expected1 (i8_extract 1 actual);
  check_int_lane 0xff label 2 expected2 (i8_extract 2 actual);
  check_int_lane 0xff label 3 expected3 (i8_extract 3 actual);
  check_int_lane 0xff label 4 expected4 (i8_extract 4 actual);
  check_int_lane 0xff label 5 expected5 (i8_extract 5 actual);
  check_int_lane 0xff label 6 expected6 (i8_extract 6 actual);
  check_int_lane 0xff label 7 expected7 (i8_extract 7 actual);
  check_int_lane 0xff label 8 expected8 (i8_extract 8 actual);
  check_int_lane 0xff label 9 expected9 (i8_extract 9 actual);
  check_int_lane 0xff label 10 expected10 (i8_extract 10 actual);
  check_int_lane 0xff label 11 expected11 (i8_extract 11 actual);
  check_int_lane 0xff label 12 expected12 (i8_extract 12 actual);
  check_int_lane 0xff label 13 expected13 (i8_extract 13 actual);
  check_int_lane 0xff label 14 expected14 (i8_extract 14 actual);
  check_int_lane 0xff label 15 expected15 (i8_extract 15 actual)

let () =
  let v = i64x2 0x0123456789abcdefL 0x8000000000000000L in
  check_i64 "i64 ushl in-range" (i64_ushl v (i64x2 1L (-1L)))
    0x02468acf13579bdeL 0x4000000000000000L;
  check_i64 "i64 ushl out-of-range" (i64_ushl v (i64x2 64L (-64L))) 0L 0L;
  check_i64 "i64 sshl in-range" (i64_sshl v (i64x2 1L (-1L)))
    0x02468acf13579bdeL 0xc000000000000000L;
  check_i64 "i64 sshl out-of-range" (i64_sshl v (i64x2 64L (-64L))) 0L (-1L);

  let v = i32x4 0x01234567l 0x80000000l 0x7fffffffl 0xffffffffl in
  let counts = i32x4 1l (-1l) 32l (-32l) in
  check_i32 "i32 ushl" (i32_ushl v counts) 0x02468acel 0x40000000l 0l 0l;
  check_i32 "i32 sshl" (i32_sshl v counts) 0x02468acel 0xc0000000l 0l
    0xffffffffl;

  let v = i16x8 0x1234 0x8000 0x7fff 0xffff 0x0001 0xfffe 0x4000 0x8001 in
  let counts = i16x8 1 (-1) 16 (-16) 1 (-1) 16 (-16) in
  check_i16 "i16 ushl" (i16_ushl v counts) 0x2468 0x4000 0 0 0x0002
    0x7fff 0 0;
  check_i16 "i16 sshl" (i16_sshl v counts) 0x2468 0xc000 0 0xffff 0x0002
    0xffff 0 0xffff;

  let v =
    i8x16 0x12 0x80 0x7f 0xff 0x01 0xfe 0x40 0x81
      0x12 0x80 0x7f 0xff 0x01 0xfe 0x40 0x81
  in
  let counts = i8x16 1 (-1) 8 (-8) 1 (-1) 8 (-8) 1 (-1) 8 (-8) 1 (-1) 8 (-8) in
  check_i8 "i8 ushl" (i8_ushl v counts) 0x24 0x40 0 0 0x02 0x7f 0 0
    0x24 0x40 0 0 0x02 0x7f 0 0;
  check_i8 "i8 sshl" (i8_sshl v counts) 0x24 0xc0 0 0xff 0x02 0xff 0
    0xff 0x24 0xc0 0 0xff 0x02 0xff 0 0xff;
  print_endline "OK"
EOF

cat > "$stub_src" <<'EOF'
#include <stdlib.h>

#define STUB(name) void name(void) { abort(); }

STUB(caml_int8x16_low_of_int8)
STUB(caml_int16x8_low_of_int16)
STUB(caml_int32x4_low_of_int32)
STUB(caml_int64x2_low_of_int64)
STUB(caml_neon_int8x16_extract)
STUB(caml_neon_int8x16_insert)
STUB(caml_neon_int8x16_sshl)
STUB(caml_neon_int8x16_ushl)
STUB(caml_neon_int16x8_extract)
STUB(caml_neon_int16x8_insert)
STUB(caml_neon_int16x8_sshl)
STUB(caml_neon_int16x8_ushl)
STUB(caml_neon_int32x4_extract)
STUB(caml_neon_int32x4_insert)
STUB(caml_neon_int32x4_sshl)
STUB(caml_neon_int32x4_ushl)
STUB(caml_neon_int64x2_extract)
STUB(caml_neon_int64x2_insert)
STUB(caml_neon_int64x2_sshl)
STUB(caml_neon_int64x2_ushl)
EOF

"$ocamlopt" -c -o "$stub_obj" "$stub_src"

"$ocamlopt" -O3 -S -c -keep-llvmir -llvm-backend -extension simd_beta \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$obj" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated file missing: $ir" >&2
  exit 1
fi

for intrinsic in \
  llvm.aarch64.neon.ushl.v2i64 \
  llvm.aarch64.neon.ushl.v4i32 \
  llvm.aarch64.neon.ushl.v8i16 \
  llvm.aarch64.neon.ushl.v16i8 \
  llvm.aarch64.neon.sshl.v2i64 \
  llvm.aarch64.neon.sshl.v4i32 \
  llvm.aarch64.neon.sshl.v8i16 \
  llvm.aarch64.neon.sshl.v16i8
do
  if ! grep -q "$intrinsic" "$ir"; then
    echo "missing LLVM intrinsic: $intrinsic" >&2
    exit 1
  fi
done

"$ocamlopt" -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$stub_obj" "$cmx"

"$out" > "$stdout_file"
grep -q "^OK$" "$stdout_file"
