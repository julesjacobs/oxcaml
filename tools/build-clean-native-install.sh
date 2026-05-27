#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

unset OCAMLPARAM
unset BUILD_OCAMLPARAM

rm -rf \
  "$repo/_build" \
  "$repo/_install" \
  "$repo/duneconf/boot.ws" \
  "$repo/duneconf/runtime_stdlib.ws" \
  "$repo/duneconf/main.ws"

make -C "$repo" install \
  LLVM_BOOT_BACKEND=0 \
  LLVM_BACKEND=0 \
  OCAMLPARAM= \
  BUILD_OCAMLPARAM= \
  "$@"

log="$repo/_build/log"
if ! grep -Eq '^# OCAMLPARAM: (""|unset)$' "$log"; then
  echo "native install build did not leave a clean OCAMLPARAM in $log" >&2
  sed -n '1,5p' "$log" >&2
  exit 1
fi

if grep -q 'llvm-backend=1' "$log"; then
  echo "native install build log contains llvm-backend=1: $log" >&2
  exit 1
fi

cat <<EOF
Clean native install ready:
  $repo/_install

Compiler:
  $repo/_install/bin/ocamlopt.opt
EOF
