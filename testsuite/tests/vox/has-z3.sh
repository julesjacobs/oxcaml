#!/bin/sh

# Locate a Z3 binary for the vox solver tests: $VOX_Z3, then PATH.
# Skips the test when none is found; otherwise appends the location
# to the compiler flags.

if [ -n "$VOX_Z3" ] && [ -x "$VOX_Z3" ]; then
  Z3="$VOX_Z3"
elif command -v z3 > /dev/null 2>&1; then
  Z3="$(command -v z3)"
else
  echo "z3 not available" > "${ocamltest_response}"
  exit "${TEST_SKIP}"
fi

echo "flags+=\" -vox-solver-path $Z3\"" > "${ocamltest_response}"
exit "${TEST_PASS}"
