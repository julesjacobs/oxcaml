#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

stage0_install=${STAGE0_INSTALL:-$repo/_install}
boot_build=${BOOT_BUILD:-$repo/_llvm_boot_context_build}
boot_install=${BOOT_INSTALL:-$repo/_llvm_boot_install}
self_runtime_build=${SELF_RUNTIME_BUILD:-$repo/_llvm_self_stage_runtime_build}
self_main_build=${SELF_MAIN_BUILD:-$repo/_llvm_self_stage_main_build}
self_stage_install=${SELF_STAGE_INSTALL:-$repo/_llvm_self_stage_install}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}
wrapper_log=${LLVM_WRAPPER_LOG:-$wrapper.log}

require_path () {
  if [ ! -e "$1" ]; then
    echo "missing required path: $1" >&2
    exit 1
  fi
}

copy_file () {
  local src=$1
  local dst=$2
  require_path "$src"
  cp -L "$src" "$dst"
}

copy_tool_file () {
  local src=$1
  local dst=$2
  if [ -e "$src.real" ]; then
    src="$src.real"
  fi
  copy_file "$src" "$dst"
}

print_wrapper_counts () {
  fresh_ir=$(rg -c -- '-x ir' "$wrapper_log" || true)
  if [ -z "$fresh_ir" ]; then fresh_ir=0; fi
  printf '%s wrapper lines: %s\n' "$1" "$(wc -l < "$wrapper_log")"
  printf '%s fresh ir: %s\n' "$1" "$fresh_ir"
}

STAGE0_INSTALL="$stage0_install" \
BOOT_BUILD="$boot_build" \
LLVM_WRAPPER="$wrapper" \
RUN_SMOKE=1 \
  "$repo/tools/build-llvm-boot-with-installed.sh"

rm -rf "$boot_install"
mkdir -p "$boot_install/bin" "$boot_install/lib"
cp -L -R "$stage0_install/lib/ocaml" "$boot_install/lib/ocaml"

copy_file "$boot_build/default/boot_ocamlopt.exe" "$boot_install/bin/ocamlopt.opt"
copy_file "$boot_build/default/main_native.exe" "$boot_install/bin/ocamlc.opt"
copy_file "$boot_build/default/boot_ocamlj.exe" "$boot_install/bin/ocamlj.opt"
copy_file "$boot_build/default/tools/ocamlmklib.exe" "$boot_install/bin/ocamlmklib.opt"
copy_file "$boot_build/default/tools/ocamldep.exe" "$boot_install/bin/ocamldep.opt"
copy_file "$boot_build/default/tools/objinfo.exe" "$boot_install/bin/ocamlobjinfo.opt"
copy_tool_file "$stage0_install/bin/ocamllex.opt" "$boot_install/bin/ocamllex.opt"

(
  cd "$boot_install/bin"
  for prog in ocamlopt ocamlc ocamlj ocamlmklib ocamldep ocamlobjinfo ocamllex; do
    ln -sf "$prog.opt" "$prog"
  done
)

OCAMLLIB="$boot_install/lib/ocaml" \
  "$boot_install/bin/ocamlopt.opt" -config >/tmp/oxcaml-llvm-boot-install-config
grep -q "^standard_library: $boot_install/lib/ocaml$" \
  /tmp/oxcaml-llvm-boot-install-config

BOOT_INSTALL="$boot_install" \
RUNTIME_BUILD="$self_runtime_build" \
MAIN_BUILD="$self_main_build" \
STAGE_INSTALL="$self_stage_install" \
LLVM_WRAPPER="$wrapper" \
  "$repo/tools/build-llvm-stage5-install.sh"

tmpdir=$(mktemp -d /tmp/oxcaml-llvm-self-stage-smoke.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT
cat > "$tmpdir/main.ml" <<'EOF'
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
let () = Printf.printf "%d\n" (fib 10)
EOF

: > "$wrapper_log"
OCAMLLIB="$self_stage_install/lib/ocaml" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
  "$self_stage_install/bin/ocamlopt.opt" -o "$tmpdir/main.exe" "$tmpdir/main.ml"
"$tmpdir/main.exe"
print_wrapper_counts self-stage-smoke

cat <<EOF
Self-stage LLVM install ready:
  $self_stage_install

Compiler:
  $self_stage_install/bin/ocamlopt.opt
EOF
