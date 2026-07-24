#!/bin/sh

set -eu

ocamlrun=$1
ocamlc=$2
source=$3
: "${TMPDIR:?TMPDIR must name a private test scratch directory}"
cache=$TMPDIR/solver-cache-process-$$
compiler_bin=$cache/compiler-bin
first=$PWD/solver-cache-first.log
second=$PWD/solver-cache-second.log
changed_build=$PWD/solver-cache-changed-build.log

mkdir -m 700 "$cache"
mkdir -m 700 "$compiler_bin"
ln -s "$ocamlc" "$compiler_bin/ocamlc"
trap 'rm -rf "$cache"' EXIT HUP INT TERM
solver="sh -c 'cat >/dev/null; printf \"unsat\\n()\\n\"'"

env VOX_SOLVER_CACHE=1 VOX_SOLVER_CACHE_DIR="$cache" \
  VOX_SOLVER_CACHE_DEBUG=1 VOX_Z3_SOLVER_VERSION=test-solver-v1 \
  VOX_SMT_SOLVER="$solver" PATH="$compiler_bin:$PATH" \
  ocamlc -use-runtime "$ocamlrun" -extension-universe alpha -vox-backend z3 \
  -i "$source" >/dev/null 2>"$first"

env VOX_SOLVER_CACHE=1 VOX_SOLVER_CACHE_DIR="$cache" \
  VOX_SOLVER_CACHE_DEBUG=1 VOX_Z3_SOLVER_VERSION=test-solver-v1 \
  VOX_SMT_SOLVER="$solver" PATH="$compiler_bin:$PATH" \
  ocamlc -use-runtime "$ocamlrun" -extension-universe alpha -vox-backend z3 \
  -i "$source" >/dev/null 2>"$second"

test "$(grep -c '^vox solver cache: z3 miss$' "$first")" -gt 0
test "$(grep -c '^vox solver cache: z3 hit$' "$second")" \
  -eq "$(grep -c '^vox solver cache: z3 miss$' "$first")"
test "$(grep -c '^vox solver cache: z3 miss$' "$second" || true)" -eq 0

env VOX_SOLVER_CACHE=1 VOX_SOLVER_CACHE_DIR="$cache" \
  VOX_SOLVER_CACHE_DEBUG=1 VOX_Z3_SOLVER_VERSION=test-solver-v1 \
  VOX_SOLVER_CACHE_COMPILER_IDENTITY=changed-build \
  VOX_SMT_SOLVER="$solver" PATH="$compiler_bin:$PATH" \
  ocamlc -use-runtime "$ocamlrun" -extension-universe alpha -vox-backend z3 \
  -i "$source" >/dev/null 2>"$changed_build"

test "$(grep -c '^vox solver cache: z3 miss$' "$changed_build")" \
  -eq "$(grep -c '^vox solver cache: z3 miss$' "$first")"

echo "solver-cache-cross-process-ok"
