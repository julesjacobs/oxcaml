#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_bmi1_generated.ml"
stub="$build_dir/amd64_bmi1_stubs.c"
out="$build_dir/amd64_bmi1_generated.o"
exe="$build_dir/amd64_bmi1_generated.exe"
ir="$build_dir/amd64_bmi1_generated.ll"

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
external andn64 : int64 -> int64 -> int64 = "" "caml_bmi_andn_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external bextr64 : int64 -> int64 -> int64 = "" "caml_bmi_bextr_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external blsi64 : int64 -> int64 = "" "caml_bmi_blsi_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external blsmsk64 : int64 -> int64 = "" "caml_bmi_blsmsk_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external blsr64 : int64 -> int64 = "" "caml_bmi_blsr_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external andn32 : int32 -> int32 -> int32 = "" "caml_bmi_andn_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external bextr32 : int32 -> int32 -> int32 = "" "caml_bmi_bextr_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external blsi32 : int32 -> int32 = "" "caml_bmi_blsi_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external blsmsk32 : int32 -> int32 = "" "caml_bmi_blsmsk_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external blsr32 : int32 -> int32 = "" "caml_bmi_blsr_int32"
[@@noalloc] [@@unboxed] [@@builtin]

let[@inline never] test_andn64 x y = andn64 x y
let[@inline never] test_bextr64 x y = bextr64 x y
let[@inline never] test_blsi64 x = blsi64 x
let[@inline never] test_blsmsk64 x = blsmsk64 x
let[@inline never] test_blsr64 x = blsr64 x

let[@inline never] test_andn32 x y = andn32 x y
let[@inline never] test_bextr32 x y = bextr32 x y
let[@inline never] test_blsi32 x = blsi32 x
let[@inline never] test_blsmsk32 x = blsmsk32 x
let[@inline never] test_blsr32 x = blsr32 x

let eq64 actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "0x%Lx <> 0x%Lx" actual expected)

let eq32 actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "0x%lx <> 0x%lx" actual expected)

let () =
  eq64 (test_andn64 0b1010L 0b1100L) 0b0100L;
  eq64 (test_bextr64 0b1100L 0x0202L) 0b11L;
  eq64 (test_bextr64 0xffffL 0x0000L) 0L;
  eq64 (test_bextr64 (-1L) 0x4000L) (-1L);
  eq64 (test_bextr64 0xffffL 0x0240L) 0L;
  eq64 (test_blsi64 0b1110L) 0b10L;
  eq64 (test_blsmsk64 0b1100L) 0b111L;
  eq64 (test_blsmsk64 0L) (-1L);
  eq64 (test_blsr64 0b1100L) 0b1000L;
  eq64 (test_blsr64 0L) 0L;
  eq32 (test_andn32 0b1010l 0b1100l) 0b0100l;
  eq32 (test_bextr32 0b1100l 0x0202l) 0b11l;
  eq32 (test_bextr32 0xffffl 0x0000l) 0l;
  eq32 (test_bextr32 (-1l) 0x2000l) (-1l);
  eq32 (test_bextr32 0xffffl 0x0220l) 0l;
  eq32 (test_blsi32 0b1110l) 0b10l;
  eq32 (test_blsmsk32 0b1100l) 0b111l;
  eq32 (test_blsmsk32 0l) (-1l);
  eq32 (test_blsr32 0b1100l) 0b1000l;
  eq32 (test_blsr32 0l) 0l
EOF

cat > "$stub" <<'EOF'
#include <caml/mlvalues.h>

#define STUB(name) CAMLprim value name(value unit) { return Val_unit; }

STUB(caml_bmi_andn_int32)
STUB(caml_bmi_andn_int64)
STUB(caml_bmi_bextr_int32)
STUB(caml_bmi_bextr_int64)
STUB(caml_bmi_blsi_int32)
STUB(caml_bmi_blsi_int64)
STUB(caml_bmi_blsmsk_int32)
STUB(caml_bmi_blsmsk_int64)
STUB(caml_bmi_blsr_int32)
STUB(caml_bmi_blsr_int64)
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

grep -q 'xor i64' "$ir"
grep -q 'and i64' "$ir"
grep -q 'sub i64' "$ir"
grep -q 'lshr i64' "$ir"
grep -q 'select i1' "$ir"
grep -q 'zext i32 .* to i64' "$ir"

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$stub" "$src"
"$exe"
