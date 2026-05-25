#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

boot_install=${BOOT_INSTALL:-$repo/_install}
boot_install=$(cd "$boot_install" && pwd)
runtime_build=${RUNTIME_BUILD:-$repo/_llvm_stage5_bootstrap_build}
main_build=${MAIN_BUILD:-$repo/_llvm_stage5_main_build}
stage_install=${STAGE_INSTALL:-$repo/_llvm_stage5_install}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}
wrapper_log=${LLVM_WRAPPER_LOG:-$wrapper.log}
cleanup_paths=()
cleanup () {
  if [ "${#cleanup_paths[@]}" -gt 0 ]; then
    rm -rf "${cleanup_paths[@]}"
  fi
}
trap cleanup EXIT

if [ -n "${RUNTIME_WS:-}" ]; then
  runtime_ws=$RUNTIME_WS
else
  runtime_ws=$(mktemp /tmp/oxcaml-stage5-runtime.XXXXXX)
  cleanup_paths+=("$runtime_ws")
fi
if [ -n "${MAIN_WS:-}" ]; then
  main_ws=$MAIN_WS
else
  main_ws=$(mktemp /tmp/oxcaml-stage5-main.XXXXXX)
  cleanup_paths+=("$main_ws")
fi
arch=${ARCH:-}

build_runtime=${BUILD_RUNTIME:-1}
build_main=${BUILD_MAIN:-1}
refresh_install=${REFRESH_INSTALL:-1}
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

require_path "$boot_install/bin/ocamlopt.opt"
require_path "$boot_install/bin/ocamlc.opt"
require_path "$boot_install/lib/ocaml/stdlib.cmxa"
require_path "$wrapper"

print_wrapper_counts () {
  fresh_ir=$(rg -c -- '-x ir' "$wrapper_log" || true)
  if [ -z "$fresh_ir" ]; then fresh_ir=0; fi
  printf '%s wrapper lines: %s\n' "$1" "$(wc -l < "$wrapper_log")"
  printf '%s fresh ir: %s\n' "$1" "$fresh_ir"
}

make_boot_build_install () {
  local wrapped=$1
  local force_classic=${2:-0}
  mkdir -p "$wrapped/bin" "$wrapped/lib"
  ln -s "$boot_install/lib/ocaml" "$wrapped/lib/ocaml"
  for tool in "$boot_install"/bin/*; do
    ln -s "$tool" "$wrapped/bin/$(basename "$tool")"
  done

  if [ "$force_classic" = 1 ]; then
    # Dune appends some per-library optimization flags after workspace flags.
    # Keep the LLVM-built boot compiler in classic mode while it builds the
    # next compiler by appending -Oclassic from a wrapper.
    rm -f "$wrapped/bin/ocamlopt" "$wrapped/bin/ocamlopt.opt"
    cat > "$wrapped/bin/ocamlopt.opt" <<EOF
#!/usr/bin/env bash
exec "$boot_install/bin/ocamlopt.opt" "\$@" -Oclassic
EOF
    chmod +x "$wrapped/bin/ocamlopt.opt"
    ln -s ocamlopt.opt "$wrapped/bin/ocamlopt"
  fi
}

make_var () {
  awk -F= -v name="$1" '$1 == name { sub(/^[ \t]*/, "", $2); sub(/[ \t]*$/, "", $2); print $2; exit }' "$2"
}

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

runtime_boot_build_install=$(mktemp -d /tmp/oxcaml-llvm-runtime-boot-build.XXXXXX)
main_boot_build_install=$(mktemp -d /tmp/oxcaml-llvm-main-boot-build.XXXXXX)
cleanup_paths+=("$runtime_boot_build_install" "$main_boot_build_install")
make_boot_build_install "$runtime_boot_build_install" 0
make_boot_build_install "$main_boot_build_install" 1

cat > "$runtime_ws" <<EOF
(lang dune 2.8)
(context (default
  (name runtime_stdlib)
  (profile main)
  (paths
    (PATH ("$runtime_boot_build_install/bin" :standard))
    (OCAMLLIB ("$runtime_boot_build_install/lib/ocaml")))
  (env (_
    (flags (:standard -directory stdlib -warn-error +A -alert -unsafe_multidomain))
    (env-vars ("OCAMLPARAM" "_,llvm-backend=1,llvm-path=$wrapper"))))))
EOF

cat > "$main_ws" <<EOF
(lang dune 2.8)
(context (default
  (name main)
  (profile main)
  (paths
    (PATH ("$main_boot_build_install/bin" :standard))
    (OCAMLLIB ("$runtime_build/install/runtime_stdlib/lib/ocaml_runtime_stdlib")))
  (env (_
    (flags (:standard -directory compiler-distro -warn-error +A -alert -unsafe_multidomain))
    (ocamlopt_flags (:standard -fno-asan))
    (env-vars ("OCAMLPARAM" "_,llvm-backend=1,llvm-path=$wrapper"))))))
EOF

runtime_targets=(
  --only-package=ocaml_runtime_stdlib
  @install
)

main_targets=(
  --only-package=ocaml
  @install
  tools/regalloc/regalloc.exe
  oxcaml/testsuite/tools/expect.exe
  oxcaml/testsuite/tools/expectnat.exe
  oxcaml/testsuite/tools/codegen_main.exe
  "oxcaml/testsuite/tools/asmgen_${arch}.o"
  testsuite/lib/lib.cma
  testsuite/lib/lib.cmxa
  testsuite/lib/testing.cma
  testsuite/lib/testing.cmxa
  @middle_end/flambda2/tests/tools/all
  ./tools/dumpobj.bc
)

export PATH="$opam_switch_bin:$PATH"

if [ "$build_runtime" = 1 ]; then
  : > "$wrapper_log"
  RUNTIME_DIR=runtime ARCH="$arch" \
    dune build --root="$repo" --build-dir "$runtime_build" \
      --workspace="$runtime_ws" "${dune_build_flags[@]}" \
      "${runtime_targets[@]}"

  runtime_lib="$runtime_build/install/runtime_stdlib/lib/ocaml_runtime_stdlib"
  require_path "$runtime_lib/stdlib.cmxa"
  mkdir -p "$runtime_lib/dynlink"
  touch "$runtime_lib/dynlink.cmxa" "$runtime_lib/dynlink/dynlink.cmxa"

  print_wrapper_counts runtime
fi

if [ "$build_main" = 1 ]; then
  require_path "$runtime_build/install/runtime_stdlib/lib/ocaml_runtime_stdlib/stdlib.cmxa"
  : > "$wrapper_log"
  RUNTIME_DIR=runtime ARCH="$arch" SYSTEM="$system" MODEL="$model" \
  ASPP="$aspp" ASPPFLAGS="$asppflags" \
    dune build --root="$repo" --build-dir "$main_build" \
      --workspace="$main_ws" "${dune_build_flags[@]}" "${main_targets[@]}"

  print_wrapper_counts main
fi

if [ "$refresh_install" = 1 ]; then
  RUNTIME_INSTALL="$runtime_build/install/runtime_stdlib" \
  MAIN_INSTALL="$main_build/install/main" \
  STAGE_INSTALL="$stage_install" \
    "$repo/tools/setup-llvm-stage-install.sh"
fi
