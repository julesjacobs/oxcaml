#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_ssse3_hadd_hsub_saturating_generated.ml"
out="$build_dir/amd64_simd_ssse3_hadd_hsub_saturating_generated.o"
ir="$build_dir/amd64_simd_ssse3_hadd_hsub_saturating_generated.ll"

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
external hadd_sat_i16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_ssse3_int16x8_hadd_saturating"
[@@noalloc] [@@builtin]

external hsub_sat_i16 : int16x8# -> int16x8# -> int16x8#
  = "" "caml_ssse3_int16x8_hsub_saturating"
[@@noalloc] [@@builtin]

let[@inline never] test_hadd_sat_i16 a b = hadd_sat_i16 a b
let[@inline never] test_hsub_sat_i16 a b = hsub_sat_i16 a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'sext i16' "$ir")" -ge 32
test "$(grep -c 'add i32' "$ir")" -ge 8
test "$(grep -c 'sub i32' "$ir")" -ge 8
test "$(grep -c 'icmp slt i32' "$ir")" -ge 16
test "$(grep -c 'icmp sgt i32' "$ir")" -ge 16
test "$(grep -c 'select i1' "$ir")" -ge 32
test "$(grep -c 'trunc i32' "$ir")" -ge 16
grep -q 'i32 -32768' "$ir"
grep -q 'i32 32767' "$ir"
grep -q 'insertelement < 8 x i16 > .* i64 7' "$ir"
