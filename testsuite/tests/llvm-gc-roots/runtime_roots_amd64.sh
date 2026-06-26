#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}

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

stdlib_flags=""
ocamlopt_dir=$(dirname "$ocamlopt")
for stdlib_dir in \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_flags="-I $stdlib_dir"
    break
  fi
done

for test in \
  allocation_slow_path_roots \
  closure_call_roots \
  live_values_roots \
  trap_roots
do
  exe="$build_dir/$test.exe"
  out="$build_dir/$test.out"
  "$ocamlopt" $stdlib_flags -O3 -llvm-backend -ccopt -no-pie \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$exe" "$source_dir/$test.ml"
  "$exe" > "$out"
  diff -u "$source_dir/$test.reference" "$out"
done
