#!/bin/sh
# Usage: run.sh BUILD_DIR OCAMLRUN OCAMLC_BYTE STDLIB_DIR
# Compiles cached.ml three times against different interfaces, with the unit
# verification cache enabled.  The implementation never changes, so a cache
# key that ignored the unit's own interface would replay step 1's success in
# step 2.
cd "$1" || exit 1
compile() {
  # The compiler is a bytecode executable or a native one.
  case $(head -c 2 "$OCAMLC") in
    '#!') runner=$OCAMLRUN ;;
    *) runner= ;;
  esac
  $runner "$OCAMLC" -nostdlib -I "$STDLIB" -extension refinement_types \
    -c "$1" > /dev/null 2>&1
}
OCAMLRUN="$2"; OCAMLC="$3"; STDLIB="$4"
VOX_VERIFY_CACHE="$1/verify-cache"; export VOX_VERIFY_CACHE
rm -rf "$VOX_VERIFY_CACHE"; mkdir "$VOX_VERIFY_CACHE"
cp impl.ml cached.ml
step() {
  cp "$2" cached.mli
  if compile cached.mli && compile cached.ml
  then echo "$1: accepted"
  else echo "$1: rejected"
  fi
}
step 1 weak.mli    # verified; the unit's success is cached
step 2 wrong.mli   # same .ml, stronger claim: must be verified again and fail
step 3 weak.mli    # back to the verified interface
