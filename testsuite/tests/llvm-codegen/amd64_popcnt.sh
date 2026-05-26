#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_popcnt_generated.ml"
out="$build_dir/amd64_popcnt_generated.o"
ir="$build_dir/amd64_popcnt_generated.ll"

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
external popcnt16 : int16# -> int16# = "" "caml_popcnt_int16"
[@@noalloc] [@@builtin]

external popcnt32 : int32# -> int32# = "" "caml_popcnt_int32"
[@@noalloc] [@@builtin]

external popcnt64 : int64# -> int64# = "" "caml_popcnt_int64"
[@@noalloc] [@@builtin]

let[@inline never] test_popcnt16 x = popcnt16 x
let[@inline never] test_popcnt32 x = popcnt32 x
let[@inline never] test_popcnt64 x = popcnt64 x
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

grep -Eq 'call +i16 @llvm\.ctpop\.i16' "$ir"
grep -Eq 'call +i32 @llvm\.ctpop\.i32' "$ir"
grep -Eq 'call +i64 @llvm\.ctpop\.i64' "$ir"
grep -q 'zext i16 .* to i64' "$ir"
grep -q 'zext i32 .* to i64' "$ir"
