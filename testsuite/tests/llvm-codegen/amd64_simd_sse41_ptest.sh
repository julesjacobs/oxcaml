#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_ptest_generated.ml"
out="$build_dir/amd64_simd_sse41_ptest_generated.o"
ir="$build_dir/amd64_simd_sse41_ptest_generated.ll"

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
external testz : int64x2# -> int64x2# -> (int[@untagged])
  = "" "caml_sse41_vec128_testz"
[@@noalloc] [@@builtin]

external testc : int64x2# -> int64x2# -> (int[@untagged])
  = "" "caml_sse41_vec128_testc"
[@@noalloc] [@@builtin]

external testnzc : int64x2# -> int64x2# -> (int[@untagged])
  = "" "caml_sse41_vec128_testnzc"
[@@noalloc] [@@builtin]

let[@inline never] test_testz a b = testz a b
let[@inline never] test_testc a b = testc a b
let[@inline never] test_testnzc a b = testnzc a b
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'bitcast < 2 x i64 >' "$ir")" -ge 3
test "$(grep -c 'and i128' "$ir")" -ge 3
test "$(grep -c 'xor i128' "$ir")" -ge 3
test "$(grep -c 'icmp eq i128' "$ir")" -ge 2
test "$(grep -c 'icmp ne i128' "$ir")" -ge 2
test "$(grep -c 'zext i1' "$ir")" -ge 3
