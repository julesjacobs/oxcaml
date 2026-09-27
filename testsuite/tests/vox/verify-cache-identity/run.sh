#!/bin/sh
# Usage: run.sh OCAMLC STDLIB_DIR, from the test's build directory
# Compiles checked.ml with the verification caches enabled, through a solver
# wrapper at a fixed path whose version and behaviour change between steps.
# A solver that fails every query can only succeed from a cache.
OCAMLC="$1"; STDLIB="$2"
Z3=$(command -v z3) || exit 1
VOX_VERIFY_CACHE="$PWD/verify-cache"; export VOX_VERIFY_CACHE
rm -rf "$VOX_VERIFY_CACHE" queries; mkdir "$VOX_VERIFY_CACHE"
# solver VERSION BEHAVIOUR: install the wrapper at ./solver.
solver() {
  rm -f solver
  {
    echo '#!/bin/sh'
    echo "if [ \"\$1\" = -version ]; then echo 'Z3 version $1'; exit 0; fi"
    case "$2" in
      works) echo "exec '$Z3' \"\$@\"" ;;
      fails) echo 'exit 1' ;;
    esac
  } > solver
  chmod +x solver
}
# A rebuilt compiler: the same bytecode behind an equivalent #! line, so its
# digest differs.
header=$(head -n 1 "$OCAMLC")
case "$header" in
  '#!'*) ;;
  *) echo "$OCAMLC does not start with #!"; exit 1 ;;
esac
runtime=${header#'#!'}
{ printf '#!%s/./%s\n' "$(dirname "$runtime")" "$(basename "$runtime")"
  tail -n +2 "$OCAMLC"; } > rebuilt
chmod +x rebuilt
# step LABEL COMPILER FLAGS...: a new flag changes the unit key but not the
# query key.
step() {
  label=$1; compiler=$2; shift 2
  if "$compiler" -nostdlib -I "$STDLIB" -extension refinement_types \
       -smt-solver ./solver "$@" -c checked.ml > /dev/null 2>&1
  then echo "$label: accepted"
  else echo "$label: rejected"
  fi
}
solver 1.0 works
step "1 (version 1.0, working)" "$OCAMLC"
solver 2.0 fails
step "2 (version 2.0, failing, query cache)" "$OCAMLC" -w +a
step "3 (version 2.0, failing, unit cache)" "$OCAMLC"
solver 1.0 fails
step "4 (version 1.0, failing, unit cache)" "$OCAMLC"
step "5 (version 1.0, failing, query cache)" "$OCAMLC" -w +a
# The unit cache is keyed by the compiler; hide the query entries to see it.
mkdir queries; mv "$VOX_VERIFY_CACHE"/query-* queries/
step "6 (rebuilt compiler, failing, no query cache)" ./rebuilt
# The query cache is not: it answers after a compiler rebuild.
mv queries/* "$VOX_VERIFY_CACHE"/
step "7 (rebuilt compiler, failing, query cache)" ./rebuilt
