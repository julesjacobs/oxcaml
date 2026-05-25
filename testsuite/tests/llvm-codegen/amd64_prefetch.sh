#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_prefetch_generated.ml"
out="$build_dir/amd64_prefetch_generated.o"
ir="$build_dir/amd64_prefetch_generated.ll"

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
type ext_pointer = private int

external prefetch_read_high : ext_pointer -> unit
  = "" "caml_prefetch_read_high_ext_pointer"
[@@noalloc] [@@builtin]

external prefetch_write_low : ext_pointer -> unit
  = "" "caml_prefetch_write_low_ext_pointer"
[@@noalloc] [@@builtin]

let[@inline never] test p =
  prefetch_read_high p;
  prefetch_write_low p
EOF

"$ocamlopt" -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c 'call  *void @llvm\.prefetch\.p0' "$ir")" = 2
grep -q 'declare void @llvm\.prefetch\.p0(ptr, i32, i32, i32)' "$ir"
grep -q 'call  *void @llvm\.prefetch\.p0(ptr %.* i32 0, i32 3, i32 1)' "$ir"
grep -q 'call  *void @llvm\.prefetch\.p0(ptr %.* i32 [01], i32 1, i32 1)' "$ir"
