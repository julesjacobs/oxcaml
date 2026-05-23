#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

if [ "${SELF_STAGE:-0}" = 1 ]; then
  default_stage_install=$repo/_llvm_self_stage_install
  default_stage_build=$repo/_llvm_self_stage_main_build
  default_fake_root=/tmp/oxcaml-self-stage-ocamltest-src
  default_list=/tmp/oxcaml-self-stage-all-minus-asm-list.txt
else
  default_stage_install=$repo/_llvm_stage5_install
  default_stage_build=$repo/_llvm_stage5_main_build
  default_fake_root=/tmp/oxcaml-stage5-ocamltest-src
  default_list=/tmp/oxcaml-stage5-all-minus-asm-list.txt
fi

stage_install=${STAGE_INSTALL:-$default_stage_install}
stage_build=${STAGE_BUILD:-$default_stage_build}
normal_build=${NORMAL_BUILD:-$repo/_build}
normal_runtime_dir=${NORMAL_RUNTIME_DIR:-$normal_build/runtime_stdlib/runtime}
fake_root=${FAKE_ROOT:-$default_fake_root}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}
list=${LIST:-$default_list}
generate_list=${GENERATE_LIST:-1}

exclude_regex=${EXCLUDE_REGEX:-'^tests/(asmgen|asmcomp)$'}

require_path () {
  if [ ! -e "$1" ]; then
    echo "missing required path: $1" >&2
    exit 1
  fi
}

stage_install=$(cd "$stage_install" && pwd)
stage_build=$(cd "$stage_build" && pwd)
normal_build=$(cd "$normal_build" && pwd)
normal_runtime_dir=$(cd "$normal_runtime_dir" && pwd)

require_path "$stage_install/bin/ocamlopt.opt"
require_path "$stage_install/bin/ocamlc.byte"
require_path "$stage_install/lib/ocaml/stdlib.cmxa"
require_path "$stage_build/main/oxcaml_main_native.exe"
require_path "$normal_runtime_dir/ocamlrun"
require_path "$wrapper"

find "$repo/testsuite/tests" -name '*.corrected' -delete

if [ "$generate_list" = 1 ]; then
  find "$repo/testsuite/tests" -type d | while IFS= read -r dir; do
    if find "$dir" -maxdepth 1 -type f \
        \( -name '*.ml' -o -name '*.mli' -o -name '*.mll' -o -name '*.mly' \) \
        | grep -q .; then
      printf 'tests/%s\n' "${dir#"$repo/testsuite/tests/"}"
    fi
  done | sort -u | rg -v "$exclude_regex" > "$list"
fi

require_path "$list"

STAGE_BUILD="$stage_build" \
NORMAL_BUILD="$normal_build" \
FAKE_ROOT="$fake_root" \
STDLIB_DIR="$stage_install/lib/ocaml" \
INSTALL_BIN="$stage_install/bin" \
INSTALL_LIB="$stage_install/lib/ocaml" \
STDLIB_STABLE_DIR="$stage_install/lib/ocaml/stdlib_stable" \
RUNTIME_DIR_PATH="$normal_runtime_dir" \
DEBUGGER_EXE="$stage_install/bin/ocamldebug" \
LLVM_WRAPPER="$wrapper" \
  "$repo/tools/setup-llvm-stage4-ocamltest.sh"

: > /tmp/oxcaml-clang-wrapper.log

OCAMLSRCDIR="$fake_root" \
CAML_LD_LIBRARY_PATH="$fake_root/stublibs" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
OCAMLLIB="$stage_install/lib/ocaml" \
PATH="/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH" \
  make -C "$repo/testsuite" one LIST="$list" \
    ocamltest_directory=../_runtest/ocamltest

printf 'wrapper lines: '
wc -l < /tmp/oxcaml-clang-wrapper.log
printf 'fresh ir: '
rg -c -- '-x ir' /tmp/oxcaml-clang-wrapper.log || true
