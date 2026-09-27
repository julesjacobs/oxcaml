#!/bin/sh
# Build testsuite/tests/vox/hmc_compilation_examples.ml, write the modules it
# compiles and their final memories from the WebAssembly model, and run them
# in Node with run-examples-node.js.
#
#   verification/catalogue/compiler-example/run-examples.sh [DIR]
#
# Run from the repository root after `make install`. The build passes
# -smt-assume-verified: `./dev test vox/hmc_compilation_examples.ml` checks
# the same files with verification. DIR (default _build/compiler-examples)
# receives the build and the modules; it must not exist, except for the
# default, which is replaced.
set -eu

root=$(pwd)
test=testsuite/tests/vox/hmc_compilation_examples.ml
default=_build/compiler-examples
dir=${1:-$default}
compiler=$root/_install/bin/ocamlopt.opt

[ -f "$test" ] || { echo "run from the repository root" >&2; exit 2; }
[ -x "$compiler" ] || { echo "no $compiler: run make install" >&2; exit 2; }

modules=$(sed -n 's/^ *all_modules = "\([^"]*\)";$/\1/p' "$test")
if [ "$dir" = "$default" ]; then
  rm -rf "$dir"
elif [ -e "$dir" ]; then
  echo "$dir exists" >&2; exit 2
fi
mkdir -p "$dir/modules"
for file in $modules; do
  if [ -f "testsuite/tests/vox/$file" ]; then
    cp "testsuite/tests/vox/$file" "$dir"
  else
    cp "verification/library/$file" "$dir"
  fi
done
(cd "$dir" && "$compiler" -extension refinement_types -smt-assume-verified $modules -o examples.exe)
(cd "$dir" && HMC_EXAMPLES_DIR=modules ./examples.exe)
node verification/catalogue/compiler-example/run-examples-node.js "$dir/modules"
