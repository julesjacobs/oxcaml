#!/bin/sh

# Locate a Lean 4 binary for the vox solver tests: $VOX_LEAN, then
# PATH, then a known pinned copy.  Skips the test when none is found;
# otherwise appends the location to the compiler flags.

PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean

if [ -n "$VOX_LEAN" ] && [ -x "$VOX_LEAN" ]; then
  LEAN="$VOX_LEAN"
elif command -v lean > /dev/null 2>&1; then
  LEAN="$(command -v lean)"
elif [ -x "$PINNED" ]; then
  LEAN="$PINNED"
else
  echo "lean not available" > "${ocamltest_response}"
  exit "${TEST_SKIP}"
fi

echo "flags+=\" -vox-solver-path $LEAN\"" > "${ocamltest_response}"
exit "${TEST_PASS}"
