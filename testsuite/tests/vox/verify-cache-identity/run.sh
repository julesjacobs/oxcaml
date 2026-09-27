#!/bin/sh
# Usage: run.sh OCAMLC STDLIB_DIR, from the test's build directory
# Compiles checked.ml with the verification caches enabled, through a solver
# wrapper at a fixed path whose version and behaviour change between steps.
# A solver that fails every query can only succeed from the cache.
OCAMLC="$1"; STDLIB="$2"
Z3=$(command -v z3) || exit 1
VOX_VERIFY_CACHE="$PWD/verify-cache"; export VOX_VERIFY_CACHE
rm -rf "$VOX_VERIFY_CACHE"; mkdir "$VOX_VERIFY_CACHE"
# solver VERSION BEHAVIOUR: install the wrapper at ./solver.
solver() {
  rm -f solver
  {
    echo '#!/bin/sh'
    echo "if [ \"\$1\" = -version ]; then echo 'Z3 version $1 (wrapper)'; exit 0; fi"
    case "$2" in
      works) echo "exec '$Z3' \"\$@\"" ;;
      fails) echo 'exit 1' ;;
    esac
  } > solver
  chmod +x solver
}
# step LABEL FLAGS...: a new flag changes the unit key but not the query key.
step() {
  label=$1; shift
  if "$OCAMLC" -nostdlib -I "$STDLIB" -extension refinement_types \
       -smt-solver ./solver "$@" -c checked.ml > /dev/null 2>&1
  then echo "$label: accepted"
  else echo "$label: rejected"
  fi
}
solver 1.0 works
step "1 (version 1.0, working)"
solver 2.0 fails
step "2 (version 2.0, failing, query cache)" -w +a
step "3 (version 2.0, failing, unit cache)"
solver 1.0 fails
step "4 (version 1.0, failing, unit cache)"
step "5 (version 1.0, failing, query cache)" -w +a
