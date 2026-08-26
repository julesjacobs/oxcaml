#!/bin/sh
set -eu

planner=${ocamlsrcdir}/ocamltest/ocamltest
fixtures=${test_source_directory}
temp_tsl=$(mktemp incremental_XXXXXX.ml)
trap 'rm -f "$temp_tsl"' EXIT HUP INT TERM

check_plan () {
  fixture=$1
  expected=$2
  sed 's/CHILDTEST/TEST/' "$fixtures/$fixture.tsl" > "$temp_tsl"
  actual=$("$planner" -plan-incremental "$temp_tsl")
  if [ "$actual" != "$expected" ]; then
    echo "$fixture: expected [$expected], got [$actual]" >&2
    exit 1
  fi
}

reject_plan () {
  sed 's/CHILDTEST/TEST/' "$fixtures/$1.tsl" > "$temp_tsl"
  if "$planner" -plan-incremental "$temp_tsl" > /dev/null 2>&1; then
    echo "$1: unexpectedly accepted" >&2
    exit 1
  else
    [ "$?" = 2 ]
  fi
}

if "$planner" -plan-incremental \
  "$fixtures/../tool-ocamltest-var-expansion/actions.ml" >/dev/null 2>&1; then
  exit 1
else
  [ "$?" = 2 ]
fi
check_plan incremental-split 'compilerlibs.ocamlcommon
ocamlc.byte
ocamlopt.byte'
reject_plan incremental-unsupported
reject_plan incremental-dynamic
reject_plan incremental-generator
reject_plan incremental-stale
reject_plan incremental-transitive
reject_plan incremental-initializer

sed 's/CHILDTEST/TEST/' "$fixtures/incremental-skipping.tsl" > "$temp_tsl"
actual=$("$planner" "$temp_tsl")
case "$actual" in *"=> passed") ;; *) echo "$actual"; exit 1 ;; esac

sed 's/CHILDTEST/TEST/' "$fixtures/incremental-failing.tsl" > "$temp_tsl"
actual=$("$planner" "$temp_tsl" 2>/dev/null)
case "$actual" in *"=> failed"*) ;; *) echo "$actual"; exit 1 ;; esac
[ ! -e should-not-exist ]
