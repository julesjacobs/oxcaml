#!/bin/sh
# The offline sweep: the whole cross product, on every backend, with an
# answer the machine did not give probed for every case rather than for a
# sample.  This is not part of the ordinary gate, which keeps a few hundred
# obligations so it can run with the suite; run this when the translation of
# an operation changes, or before a release.
#
#   differential_sweep.sh <install-prefix> [backend ...]
#
# <install-prefix> is a directory holding bin/ocamlrun, bin/ocamlc.byte,
# bin/ocamlc.opt and bin/ocamlopt.opt, such as the _install a build produces.

set -eu

prefix=$1
shift
backends=${*:-z3 oxsmt lean}

: "${TMPDIR:?TMPDIR must name a private scratch directory}"
here=$(cd "$(dirname "$0")" && pwd -P)

OCAMLLIB=$prefix/lib/ocaml
export OCAMLLIB

for backend in $backends; do
  jobs=2
  if test "$backend" = lean; then jobs=1; fi
  python3 "$here/differential_gate.py" \
    --ocamlrun "$prefix/bin/ocamlrun" \
    --ocamlc "$prefix/bin/ocamlc.byte" \
    --ocamlc-opt "$prefix/bin/ocamlc.opt" \
    --ocamlopt-opt "$prefix/bin/ocamlopt.opt" \
    --backend "$backend" --profile full --jobs "$jobs" \
    --sentinels-everywhere
done
