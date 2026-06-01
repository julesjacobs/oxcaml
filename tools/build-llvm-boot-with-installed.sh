#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
. "$repo/tools/llvm-backend-defaults.sh"

stage0_install=${STAGE0_INSTALL:-$repo/_install}
stage0_install=$(cd "$stage0_install" && pwd)
boot_build=${BOOT_BUILD:-$repo/_llvm_boot_context_build}
wrapper=${LLVM_WRAPPER:?set LLVM_WRAPPER to the clang wrapper or LLVM tool path}
wrapper_log=${LLVM_WRAPPER_LOG:-$wrapper.log}
opam_switch_bin=${OPAM_SWITCH_BIN:-}
arch=${ARCH:-}
run_smoke=${RUN_SMOKE:-1}
dune_build_flags=()
if [ -n "${DUNE_BUILD_FLAGS:-}" ]; then
  read -r -a dune_build_flags <<< "$DUNE_BUILD_FLAGS"
fi

if [ -z "$arch" ]; then
  case "$(uname -m)" in
    arm64|aarch64) arch=arm64 ;;
    x86_64|amd64) arch=amd64 ;;
    *) echo "set ARCH for target $(uname -m)" >&2; exit 1 ;;
  esac
fi

require_path () {
  if [ ! -e "$1" ]; then
    echo "missing required path: $1" >&2
    exit 1
  fi
}

make_var () {
  awk -F= -v name="$1" '$1 == name { sub(/^[ \t]*/, "", $2); sub(/[ \t]*$/, "", $2); print $2; exit }' "$2"
}

print_wrapper_counts () {
  fresh_ir=$(rg -c -- '-x ir' "$wrapper_log" || true)
  if [ -z "$fresh_ir" ]; then fresh_ir=0; fi
  printf '%s wrapper lines: %s\n' "$1" "$(wc -l < "$wrapper_log")"
  printf '%s fresh ir: %s\n' "$1" "$fresh_ir"
}

require_path "$stage0_install/bin/ocamlopt.opt"
require_path "$stage0_install/bin/ocamlc.opt"
require_path "$stage0_install/lib/ocaml/stdlib.cmxa"
require_path "$wrapper"
if [ -n "$opam_switch_bin" ]; then
  require_path "$opam_switch_bin/dune"
fi

boot_ws=$(mktemp /tmp/oxcaml-llvm-boot.XXXXXX)
saved_boot_ws=$(mktemp /tmp/oxcaml-llvm-saved-boot.XXXXXX)
tmpdir=
had_boot_ws=0
if [ -f "$repo/duneconf/boot.ws" ]; then
  cp "$repo/duneconf/boot.ws" "$saved_boot_ws"
  had_boot_ws=1
else
  rm -f "$saved_boot_ws"
fi

cleanup () {
  rm -f "$boot_ws"
  if [ "$had_boot_ws" = 1 ]; then
    cp "$saved_boot_ws" "$repo/duneconf/boot.ws"
    rm -f "$saved_boot_ws"
  else
    rm -f "$repo/duneconf/boot.ws"
  fi
  if [ -n "$tmpdir" ]; then
    rm -rf "$tmpdir"
  fi
}
trap cleanup EXIT

system=$(make_var SYSTEM "$repo/Makefile.config")
model=$(make_var MODEL "$repo/Makefile.config")
aspp=$(make_var ASPP "$repo/Makefile.config")
oc_cppflags=$(make_var OC_CPPFLAGS "$repo/Makefile.build_config")
oc_native_cppflags=$(make_var OC_NATIVE_CPPFLAGS "$repo/Makefile.build_config")
oc_cppflags=${oc_cppflags//\$\(ROOTDIR\)/$repo}
oc_cppflags=${oc_cppflags//\$\(RUNTIME_DIR\)/runtime}
asppflags=${ASPPFLAGS:-$oc_cppflags $oc_native_cppflags}

if [ -z "$system" ] || [ -z "$model" ] || [ -z "$aspp" ]; then
  echo "could not read SYSTEM, MODEL, or ASPP from Makefile.config" >&2
  exit 1
fi

make -C "$repo" LLVM_BOOT_BACKEND=1 LLVM_BOOT_INSTALL="$stage0_install" \
  LLVM_PATH="$wrapper" duneconf/boot.ws >/dev/null

cp "$repo/duneconf/boot.ws" "$boot_ws"

targets=(
  main_native.exe
  boot_ocamlopt.exe
  boot_ocamlj.exe
  tools/ocamlmklib.exe
  tools/ocamldep.exe
  tools/objinfo.exe
  ocamltest/ocamltest.native
)

rm -rf "$boot_build"
: > "$wrapper_log"

dune_command=(
  dune build --root="$repo" --build-dir="$boot_build"
  --workspace="$boot_ws"
)
if [ "${#dune_build_flags[@]}" -ne 0 ]; then
  dune_command+=("${dune_build_flags[@]}")
fi
if [ -n "$opam_switch_bin" ]; then
  export PATH="$opam_switch_bin:$PATH"
fi

PATH="$stage0_install/bin:$PATH" \
RUNTIME_DIR=runtime ARCH="$arch" SYSTEM="$system" MODEL="$model" \
ASPP="$aspp" ASPPFLAGS="$asppflags" \
  "${dune_command[@]}" "${targets[@]}"

print_wrapper_counts boot

if [ "$run_smoke" = 1 ]; then
  tmpdir=$(mktemp -d /tmp/oxcaml-llvm-boot-smoke.XXXXXX)
  printf 'let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)\nlet () = Printf.printf "%%d\\n" (sum 10 0)\n' \
    > "$tmpdir/main.ml"
  : > "$wrapper_log"
  OCAMLLIB="$stage0_install/lib/ocaml" \
  OCAMLPARAM="$(llvm_backend_ocamlparam "$wrapper")" \
    "$boot_build/default/boot_ocamlopt.exe" -o "$tmpdir/main.exe" "$tmpdir/main.ml"
  "$tmpdir/main.exe"
  print_wrapper_counts smoke
fi
