#!/bin/sh
# The offline sweep: the tables the ordinary suite is too small to carry, on
# every backend.  Run this when the translation of an operation changes, or
# before a release.
#
#   differential_sweep.sh <install-prefix> [backend ...]
#
# <install-prefix> is a directory holding bin/ocamlrun, bin/ocamlc.byte,
# bin/ocamlc.opt and bin/ocamlopt.opt, such as the _install a build produces.
#
# The ordinary suite runs the small [routine] table against oxsmt and the
# division table against z3.  Everything else is here: the rest of the core
# table on both SMT backends, the whole cross product, and Lean, which costs
# several times what an SMT obligation costs and is held to one process.

set -eu

prefix=$1
shift
backends=${*:-z3 oxsmt lean}

: "${TMPDIR:?TMPDIR must name a private scratch directory}"
here=$(cd "$(dirname "$0")" && pwd -P)

OCAMLLIB=$prefix/lib/ocaml
export OCAMLLIB

# Every arm runs even when an earlier one disagrees, and the failures are
# summarised at the end: a tool whose job is diagnosis should say which arms
# are affected rather than stop at the first.
failures=

for backend in $backends; do
  jobs=2
  profiles="core full"
  if test "$backend" = lean; then
    jobs=1
    # Lean at about 0.8s an obligation: the whole cross product would take
    # over an hour, so it runs the table that keeps every operator.
    profiles=lean
  fi
  for profile in $profiles; do
    if python3 "$here/differential_gate.py" \
      --ocamlrun "$prefix/bin/ocamlrun" \
      --ocamlc "$prefix/bin/ocamlc.byte" \
      --ocamlc-opt "$prefix/bin/ocamlc.opt" \
      --ocamlopt-opt "$prefix/bin/ocamlopt.opt" \
      --backend "$backend" --profile "$profile" --jobs "$jobs"
    then :
    else failures="$failures $backend/$profile"
    fi
  done
done

if test -n "$failures"; then
  echo "sweep: arms that did not come back clean:$failures"
  exit 1
fi
echo "sweep: every arm clean"
