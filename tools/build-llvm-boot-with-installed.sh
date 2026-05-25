#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

stage0_install=${STAGE0_INSTALL:-$repo/_install}
stage0_install=$(cd "$stage0_install" && pwd)
boot_build=${BOOT_BUILD:-$repo/_llvm_boot_context_build}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}
wrapper_log=${LLVM_WRAPPER_LOG:-$wrapper.log}
if [ -n "${OPAM_SWITCH_BIN:-}" ]; then
  opam_switch_bin=$OPAM_SWITCH_BIN
else
  dune_path=$(command -v dune || true)
  if [ -z "$dune_path" ]; then
    echo "could not find dune; set OPAM_SWITCH_BIN" >&2
    exit 1
  fi
  opam_switch_bin=$(dirname "$dune_path")
fi
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

make_stage0_boot_install () {
  local wrapped=$1
  mkdir -p "$wrapped/bin" "$wrapped/lib"
  ln -s "$stage0_install/lib/ocaml" "$wrapped/lib/ocaml"
  for tool in "$stage0_install"/bin/*; do
    ln -s "$tool" "$wrapped/bin/$(basename "$tool")"
  done

  # Dune appends some per-library optimization flags after workspace flags.
  # Keep the LLVM-built stage0 compiler in classic mode for the bootstrap by
  # appending -Oclassic from a wrapper, so it wins over those local flags.
  rm -f "$wrapped/bin/ocamlopt" "$wrapped/bin/ocamlopt.opt"
  cat > "$wrapped/bin/ocamlopt.opt" <<EOF
#!/usr/bin/env bash
exec "$stage0_install/bin/ocamlopt.opt" "\$@" -Oclassic
EOF
  chmod +x "$wrapped/bin/ocamlopt.opt"
  ln -s ocamlopt.opt "$wrapped/bin/ocamlopt"
}

require_path "$stage0_install/bin/ocamlopt.opt"
require_path "$stage0_install/bin/ocamlc.opt"
require_path "$stage0_install/lib/ocaml/stdlib.cmxa"
require_path "$wrapper"

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

boot_ws=
stage0_boot_install=$(mktemp -d /tmp/oxcaml-llvm-stage0-boot.XXXXXX)
trap 'if [ -n "${boot_ws:-}" ]; then rm -f "$boot_ws"; fi; rm -rf "$stage0_boot_install"' EXIT
make_stage0_boot_install "$stage0_boot_install"

make -C "$repo" LLVM_BOOT_BACKEND=1 LLVM_BOOT_INSTALL="$stage0_boot_install" \
  LLVM_PATH="$wrapper" duneconf/boot.ws >/dev/null

boot_ws=$(mktemp /tmp/oxcaml-llvm-boot.XXXXXX)
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

PATH="$stage0_boot_install/bin:$opam_switch_bin:$PATH" \
RUNTIME_DIR=runtime ARCH="$arch" SYSTEM="$system" MODEL="$model" \
ASPP="$aspp" ASPPFLAGS="$asppflags" \
  "$opam_switch_bin/dune" build --root="$repo" --build-dir="$boot_build" \
    --workspace="$boot_ws" "${dune_build_flags[@]}" "${targets[@]}"

print_wrapper_counts boot

if [ "$run_smoke" = 1 ]; then
  tmpdir=$(mktemp -d /tmp/oxcaml-llvm-boot-smoke.XXXXXX)
  trap 'rm -f "$boot_ws"; rm -rf "$stage0_boot_install"; rm -rf "$tmpdir"' EXIT
  printf 'let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)\nlet () = Printf.printf "%%d\\n" (sum 10 0)\n' \
    > "$tmpdir/main.ml"
  : > "$wrapper_log"
  OCAMLLIB="$stage0_install/lib/ocaml" \
  OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
    "$boot_build/default/boot_ocamlopt.exe" -Oclassic -o "$tmpdir/main.exe" \
      "$tmpdir/main.ml"
  "$tmpdir/main.exe"
  print_wrapper_counts smoke
fi
