#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
native_build=${NATIVE_BUILD:-$repo/_native_build}
native_install=${NATIVE_INSTALL:-$repo/_native_install}
force=0

make_args=()
for arg in "$@"; do
  case "$arg" in
    --force)
      force=1
      ;;
    *)
      make_args+=("$arg")
      ;;
  esac
done

if [ "${#make_args[@]}" -ne 0 ]; then
  force=1
fi

log_is_clean () {
  local log=$1
  [ -f "$log" ] || return 1
  grep -Eq '^# OCAMLPARAM: (""|unset)$' "$log" || return 1
  ! grep -q 'llvm-backend=1' "$log"
}

install_is_present () {
  local install=$1
  [ -x "$install/bin/ocamlopt.opt" ] \
    && [ -f "$install/lib/ocaml/stdlib.cmxa" ]
}

save_current_native () {
  rsync -a --delete "$repo/_build/" "$native_build/"
  rsync -a --delete "$repo/_install/" "$native_install/"
}

print_ready () {
  cat <<EOF
Clean native comparison install ready:
  $native_install

Build tree:
  $native_build

Compiler:
  $native_install/bin/ocamlopt.opt
EOF
}

if [ "$force" = 0 ] \
  && install_is_present "$native_install" \
  && log_is_clean "$native_build/log"; then
  print_ready
  exit 0
fi

if [ "$force" = 0 ] \
  && install_is_present "$repo/_install" \
  && log_is_clean "$repo/_build/log"; then
  save_current_native
  print_ready
  exit 0
fi

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
  ${make_args[@]+"${make_args[@]}"}

log="$repo/_build/log"
if ! log_is_clean "$log"; then
  echo "native install build did not leave a clean OCAMLPARAM in $log" >&2
  sed -n '1,5p' "$log" >&2
  exit 1
fi

save_current_native
print_ready
