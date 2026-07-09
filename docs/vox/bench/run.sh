#!/bin/sh
# Build and run the bounds-check benchmark: the verified kernel is the
# ACTUAL suite file (demo/lean_kernel.ml), compiled natively under
# verification -- the build fails if any obligation does.
set -e
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
OPT=${OCAMLOPT:-$ROOT/_build/_bootinstall/bin/ocamlopt}
LEAN=${VOX_LEAN:-/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
cp "$ROOT/testsuite/tests/vox/lib/ia_lib.mli" \
   "$ROOT/testsuite/tests/vox/lib/ia_lib.ml" \
   "$ROOT/testsuite/tests/vox/demo/lean_kernel.ml" \
   "$ROOT/docs/vox/bench/bench.ml" "$D"
cd "$D"
"$OPT" -O3 -vox-solver-path "$LEAN" \
  -o bench ia_lib.mli ia_lib.ml lean_kernel.ml bench.ml 2>/dev/null \
  || "$OPT" -vox-solver-path "$LEAN" \
       -o bench ia_lib.mli ia_lib.ml lean_kernel.ml bench.ml
./bench
