#!/bin/sh

set -eu

ocamlrun=$1
ocamlc=$2
source=$3
: "${TMPDIR:?TMPDIR must name a private test scratch directory}"
scratch_root=$(cd "$TMPDIR" && pwd -P)
root=$scratch_root/default-backend-selection-$$
solver="sh -c 'cat >/dev/null; printf \"unsat\\n()\\n\"'"

mkdir -m 700 "$root"
trap 'rm -rf "$root"' EXIT HUP INT TERM

run () {
  label=$1
  expected=$2
  shift 2
  cache=$root/$label-cache
  log=$root/$label.log
  mkdir -m 700 "$cache"
  env VOX_SOLVER_CACHE=1 VOX_SOLVER_CACHE_DIR="$cache" \
    VOX_SOLVER_CACHE_DEBUG=1 VOX_Z3_SOLVER_VERSION=test-solver-v1 \
    VOX_SMT_SOLVER="$solver" \
    "$ocamlrun" "$ocamlc" -extension-universe alpha "$@" -i "$source" \
    >/dev/null 2>"$log"
  grep -Fq "vox solver cache: $expected miss" "$log"
  if test "$label" != cross; then
    for backend in lean z3 oxsmt; do
      if test "$backend" != "$expected" \
         && grep -Fq "vox solver cache: $backend " "$log"; then
        exit 1
      fi
    done
  fi
}

run default z3
run lean lean -vox-backend lean
run z3 z3 -vox-backend z3
run oxsmt oxsmt -vox-backend oxsmt
run cross lean -vox-backend cross
grep -Fq 'vox solver cache: z3 miss' "$root/cross.log"
grep -Fq 'vox solver cache: oxsmt miss' "$root/cross.log"

echo "default-backend-selection-ok"
