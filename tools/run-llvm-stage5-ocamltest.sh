#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

self_stage=${SELF_STAGE:-0}
case "$self_stage" in
0)
  default_stage_install=$repo/_llvm_stage5_install
  default_stage_build=$repo/_llvm_stage5_main_build
  default_fake_root=/tmp/oxcaml-stage5-ocamltest-src
  default_list=/tmp/oxcaml-stage5-all-minus-asm-list.txt
  ;;
1)
  default_stage_install=$repo/_llvm_self_stage_install
  default_stage_build=$repo/_llvm_self_stage_main_build
  default_fake_root=/tmp/oxcaml-self-stage-ocamltest-src
  default_list=/tmp/oxcaml-self-stage-all-minus-asm-list.txt
  ;;
2)
  default_stage_install=$repo/_llvm_self_stage2_install
  default_stage_build=$repo/_llvm_self_stage2_main_build
  default_fake_root=/tmp/oxcaml-self-stage2-ocamltest-src
  default_list=/tmp/oxcaml-self-stage2-all-minus-asm-list.txt
  ;;
*)
  echo "SELF_STAGE must be 0, 1, or 2" >&2
  exit 2
  ;;
esac

stage_install=${STAGE_INSTALL:-$default_stage_install}
stage_build=${STAGE_BUILD:-$default_stage_build}
normal_build=${NORMAL_BUILD:-$repo/_build}
normal_runtime_dir=${NORMAL_RUNTIME_DIR:-$normal_build/runtime_stdlib/runtime}
fake_root=${FAKE_ROOT:-$default_fake_root}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}
wrapper_log=${LLVM_WRAPPER_LOG:-$wrapper.log}
list=${LIST:-$default_list}
generate_list=${GENERATE_LIST:-1}
parallel_tests=${LLVM_TESTSUITE_PARALLEL:-auto}
testsuite_jobs=${LLVM_TESTSUITE_JOBS:-}
opam_switch_bin=${OPAM_SWITCH_BIN:-}

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
if [ -n "$opam_switch_bin" ]; then
  require_path "$opam_switch_bin"
  export PATH="$opam_switch_bin:$PATH"
fi

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

: > "$wrapper_log"

set +e
make_args=(-C "$repo/testsuite")
if [ -n "$testsuite_jobs" ]; then
  make_args+=("-j$testsuite_jobs")
fi

parallel_available () {
  echo | parallel >/dev/null 2>/dev/null \
    && echo | parallel --gnu --no-notice >/dev/null 2>/dev/null
}

select_testsuite_target () {
  case "$parallel_tests" in
  auto)
    if parallel_available; then
      parallel_tests=1
    else
      parallel_tests=0
      echo "GNU parallel not available; falling back to serial testsuite run"
    fi
    ;;
  0|1) ;;
  *)
    echo "LLVM_TESTSUITE_PARALLEL must be 0, 1, or auto" >&2
    exit 2
    ;;
  esac

  if [ "$parallel_tests" = 1 ]; then
    testsuite_target=list-parallel
    testsuite_list_var=FILE
  else
    testsuite_target=one
    testsuite_list_var=LIST
  fi
}

select_testsuite_target

echo "Running testsuite target '$testsuite_target' with $testsuite_list_var=$list"

OCAMLSRCDIR="$fake_root" \
CAML_LD_LIBRARY_PATH="$fake_root/stublibs" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
OCAMLLIB="$stage_install/lib/ocaml" \
  make "${make_args[@]}" "$testsuite_target" "$testsuite_list_var=$list" \
    ocamltest_directory=../_runtest/ocamltest
test_status=$?
set -e

printf 'wrapper lines: '
wc -l < "$wrapper_log"
printf 'fresh ir: '
rg -c -- '-x ir' "$wrapper_log" || true

exit "$test_status"
