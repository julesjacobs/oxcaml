#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_bmi2_generated.ml"
stub="$build_dir/amd64_bmi2_stubs.c"
out="$build_dir/amd64_bmi2_generated.o"
exe="$build_dir/amd64_bmi2_generated.exe"
ir="$build_dir/amd64_bmi2_generated.ll"

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
external box_int64 : int64# -> int64 = "%box_int64"

external bzhi64 : int64 -> int64 -> int64 = "" "caml_bmi2_bzhi_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external pext64 : int64 -> int64 -> int64 = "" "caml_bmi2_pext_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external pdep64 : int64 -> int64 -> int64 = "" "caml_bmi2_pdep_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external rorx64 : (int[@untagged]) -> (int64[@unboxed]) -> (int64[@unboxed])
  = "" "caml_bmi2_rorx_int64"
[@@noalloc] [@@builtin]

external sarx64 : int64 -> int64 -> int64 = "" "caml_bmi2_sarx_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external shrx64 : int64 -> int64 -> int64 = "" "caml_bmi2_shrx_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external shlx64 : int64 -> int64 -> int64 = "" "caml_bmi2_shlx_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external mulx64 : int64# -> int64# -> #(int64# * int64#)
  = "" "caml_bmi2_mulx_int64"
[@@noalloc] [@@builtin]

external bzhi32 : int32 -> int32 -> int32 = "" "caml_bmi2_bzhi_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external pext32 : int32 -> int32 -> int32 = "" "caml_bmi2_pext_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external pdep32 : int32 -> int32 -> int32 = "" "caml_bmi2_pdep_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external rorx32 : (int[@untagged]) -> (int32[@unboxed]) -> (int32[@unboxed])
  = "" "caml_bmi2_rorx_int32"
[@@noalloc] [@@builtin]

external sarx32 : int32 -> int32 -> int32 = "" "caml_bmi2_sarx_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external shrx32 : int32 -> int32 -> int32 = "" "caml_bmi2_shrx_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external shlx32 : int32 -> int32 -> int32 = "" "caml_bmi2_shlx_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external mulx32 : int32# -> int32# -> #(int64# * int64#)
  = "" "caml_bmi2_mulx_int32"
[@@noalloc] [@@builtin]

let[@inline never] test_bzhi64 x y = bzhi64 x y
let[@inline never] test_pext64 x y = pext64 x y
let[@inline never] test_pdep64 x y = pdep64 x y
let[@inline never] test_rorx64 i x = rorx64 i x
let[@inline never] test_sarx64 x y = sarx64 x y
let[@inline never] test_shrx64 x y = shrx64 x y
let[@inline never] test_shlx64 x y = shlx64 x y

let[@inline never] test_mulx64 x y =
  let #(low, high) = mulx64 x y in
  box_int64 low, box_int64 high

let[@inline never] test_bzhi32 x y = bzhi32 x y
let[@inline never] test_pext32 x y = pext32 x y
let[@inline never] test_pdep32 x y = pdep32 x y
let[@inline never] test_rorx32 i x = rorx32 i x
let[@inline never] test_sarx32 x y = sarx32 x y
let[@inline never] test_shrx32 x y = shrx32 x y
let[@inline never] test_shlx32 x y = shlx32 x y

let[@inline never] test_mulx32 x y =
  let #(low, high) = mulx32 x y in
  box_int64 low, box_int64 high

let eq64 actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "0x%Lx <> 0x%Lx" actual expected)

let eq32 actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "0x%lx <> 0x%lx" actual expected)

let () =
  eq64 (test_bzhi64 0b1100L 3L) 0b100L;
  eq64 (test_bzhi64 0b1100L 64L) 0b1100L;
  eq64 (test_pdep64 3L 4L) 0x4L;
  eq64 (test_pdep64 235L 522L) 0xAL;
  eq64 (test_pext64 3L 4L) 0x0L;
  eq64 (test_pext64 235L 522L) 0x3L;
  eq64 (test_rorx64 1 1L) Int64.min_int;
  eq64 (test_rorx64 0 0x1234L) 0x1234L;
  eq64 (test_sarx64 (-8L) 65L) (-4L);
  eq64 (test_shrx64 (-1L) 64L) (-1L);
  eq64 (test_shlx64 1L 64L) 1L;
  let low, high = test_mulx64 #2L #3L in
  eq64 low 6L;
  eq64 high 0L;
  let low, high = test_mulx64 #0x8000000000000000L #2L in
  eq64 low 0L;
  eq64 high 1L;
  eq32 (test_bzhi32 0b1100l 3l) 0b100l;
  eq32 (test_bzhi32 0b1100l 32l) 0b1100l;
  eq32 (test_pdep32 3l 4l) 0x4l;
  eq32 (test_pdep32 235l 522l) 0xAl;
  eq32 (test_pext32 3l 4l) 0x0l;
  eq32 (test_pext32 235l 522l) 0x3l;
  eq32 (test_rorx32 1 1l) Int32.min_int;
  eq32 (test_rorx32 0 0x1234l) 0x1234l;
  eq32 (test_sarx32 (-8l) 33l) (-4l);
  eq32 (test_shrx32 (-1l) 32l) (-1l);
  eq32 (test_shlx32 1l 32l) 1l;
  let low, high = test_mulx32 #2l #3l in
  eq64 low 6L;
  eq64 high 0L;
  let low, high = test_mulx32 #0x80000000l #2l in
  eq64 low 0L;
  eq64 high 1L
EOF

cat > "$stub" <<'EOF'
#include <caml/mlvalues.h>

#define STUB(name) CAMLprim value name(value unit) { return Val_unit; }

STUB(caml_bmi2_bzhi_int32)
STUB(caml_bmi2_bzhi_int64)
STUB(caml_bmi2_mulx_int32)
STUB(caml_bmi2_mulx_int64)
STUB(caml_bmi2_pext_int32)
STUB(caml_bmi2_pext_int64)
STUB(caml_bmi2_pdep_int32)
STUB(caml_bmi2_pdep_int64)
STUB(caml_bmi2_rorx_int32)
STUB(caml_bmi2_rorx_int64)
STUB(caml_bmi2_sarx_int32)
STUB(caml_bmi2_sarx_int64)
STUB(caml_bmi2_shrx_int32)
STUB(caml_bmi2_shrx_int64)
STUB(caml_bmi2_shlx_int32)
STUB(caml_bmi2_shlx_int64)
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

if grep -Eq 'llvm\.x86\.(bmi|tbm)' "$ir"; then
  echo "unexpected target-feature-specific BMI intrinsic in generated IR" >&2
  exit 1
fi

grep -q 'mul i128' "$ir"
grep -q 'shl i64' "$ir"
grep -q 'lshr i64' "$ir"
grep -q 'ashr i64' "$ir"
grep -q 'select i1' "$ir"
grep -q 'zext i32 .* to i64' "$ir"

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$stub" "$src"
"$exe"
