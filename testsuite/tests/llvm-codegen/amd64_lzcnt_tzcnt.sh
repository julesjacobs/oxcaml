#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_lzcnt_tzcnt_generated.ml"
out="$build_dir/amd64_lzcnt_tzcnt_generated.o"
ir="$build_dir/amd64_lzcnt_tzcnt_generated.ll"

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
external lzcnt16 : int16# -> int16# = "" "caml_lzcnt_int16"
[@@noalloc] [@@builtin]

external lzcnt32 : int32# -> int32# = "" "caml_lzcnt_int32"
[@@noalloc] [@@builtin]

external lzcnt64 : int64# -> int64# = "" "caml_lzcnt_int64"
[@@noalloc] [@@builtin]

external tzcnt16 : int16# -> int16# = "" "caml_bmi_tzcnt_int16"
[@@noalloc] [@@builtin]

external tzcnt32 : int32# -> int32# = "" "caml_bmi_tzcnt_int32"
[@@noalloc] [@@builtin]

external tzcnt64 : int64# -> int64# = "" "caml_bmi_tzcnt_int64"
[@@noalloc] [@@builtin]

let[@inline never] test_lzcnt16 x = lzcnt16 x
let[@inline never] test_lzcnt32 x = lzcnt32 x
let[@inline never] test_lzcnt64 x = lzcnt64 x
let[@inline never] test_tzcnt16 x = tzcnt16 x
let[@inline never] test_tzcnt32 x = tzcnt32 x
let[@inline never] test_tzcnt64 x = tzcnt64 x
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -Eq 'call +i16 @llvm\.ctlz\.i16\(i16 .*, i1 0\)' "$ir"
grep -Eq 'call +i32 @llvm\.ctlz\.i32\(i32 .*, i1 0\)' "$ir"
grep -Eq 'call +i64 @llvm\.ctlz\.i64\(i64 .*, i1 0\)' "$ir"
grep -Eq 'call +i16 @llvm\.cttz\.i16\(i16 .*, i1 0\)' "$ir"
grep -Eq 'call +i32 @llvm\.cttz\.i32\(i32 .*, i1 0\)' "$ir"
grep -Eq 'call +i64 @llvm\.cttz\.i64\(i64 .*, i1 0\)' "$ir"
test "$(grep -c 'zext i16 .* to i64' "$ir")" -ge 2
test "$(grep -c 'zext i32 .* to i64' "$ir")" -ge 2
