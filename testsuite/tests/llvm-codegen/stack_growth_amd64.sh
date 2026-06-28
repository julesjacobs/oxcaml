#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/stack_growth_amd64_generated.ml"
exe="$build_dir/stack_growth_amd64_generated.exe"
out="$build_dir/stack_growth_amd64.out"
extra_link_flags=""

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64) extra_link_flags="-ccopt -no-pie" ;;
esac

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

awk '
  in_header && /^\*\)/ { in_header = 0; next }
  NR == 1 && /^\(\* TEST$/ { in_header = 1; next }
  !in_header { print }
' "$source_dir/stack_growth.ml" > "$src"

"$ocamlopt" $stdlib_flags -O3 -llvm-backend $extra_link_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$src"

runparam="${OCAMLRUNPARAM:-}"
if [ -z "$runparam" ]; then
  runparam="l=200000"
else
  runparam="$runparam,l=200000"
fi
OCAMLRUNPARAM="$runparam" "$exe" > "$out"

diff -u "$source_dir/stack_growth.reference" "$out"
