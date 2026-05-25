#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
tmpdir=$(mktemp -d /tmp/oxcaml-run-llvm-stage5-negative.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT

stage_install=$tmpdir/stage-install
stage_build=$tmpdir/stage-build
normal_build=$tmpdir/normal-build
normal_runtime_dir=$normal_build/runtime_stdlib/runtime
wrapper=$tmpdir/clang-wrapper

mkdir -p "$stage_install/bin" "$stage_install/lib/ocaml" \
  "$stage_build/main" "$normal_runtime_dir"
touch "$stage_install/bin/ocamlopt.opt" \
  "$stage_install/bin/ocamlc.byte" \
  "$stage_install/lib/ocaml/stdlib.cmxa" \
  "$stage_build/main/oxcaml_main_native.exe" \
  "$normal_runtime_dir/ocamlrun" \
  "$wrapper"
chmod +x "$stage_install/bin/ocamlopt.opt" \
  "$stage_install/bin/ocamlc.byte" \
  "$stage_build/main/oxcaml_main_native.exe" \
  "$normal_runtime_dir/ocamlrun" \
  "$wrapper"

expect_reject () {
  local entry=$1
  local pattern=$2
  local list=$tmpdir/list
  local stderr=$tmpdir/stderr
  printf '%s\n' "$entry" > "$list"
  if STAGE_INSTALL=$stage_install STAGE_BUILD=$stage_build \
      NORMAL_BUILD=$normal_build NORMAL_RUNTIME_DIR=$normal_runtime_dir \
      LLVM_WRAPPER=$wrapper LIST=$list GENERATE_LIST=0 \
      "$repo/tools/run-llvm-stage5-ocamltest.sh" 2>"$stderr"; then
    echo "accepted invalid list entry: $entry" >&2
    exit 1
  fi
  grep -q "$pattern" "$stderr"
}

expect_reject "tests/llvm-codegen/does-not-exist" "missing testsuite list entry"
expect_reject "../tests/llvm-codegen" "invalid testsuite list entry"
expect_reject "/tmp/llvm-codegen" "invalid testsuite list entry"

capture_default_paths () {
  local stdout=$1
  STAGE_INSTALL=$stage_install STAGE_BUILD=$stage_build \
    NORMAL_BUILD=$normal_build NORMAL_RUNTIME_DIR=$normal_runtime_dir \
    LLVM_WRAPPER=$wrapper LLVM_TESTSUITE_VALIDATE_ONLY=1 \
    "$repo/tools/run-llvm-stage5-ocamltest.sh" > "$stdout"
}

capture_default_paths "$tmpdir/first.out"
capture_default_paths "$tmpdir/second.out"

first_fake=$(awk -F= '$1 == "FAKE_ROOT" { print $2 }' "$tmpdir/first.out")
second_fake=$(awk -F= '$1 == "FAKE_ROOT" { print $2 }' "$tmpdir/second.out")
first_list=$(awk -F= '$1 == "LIST" { print $2 }' "$tmpdir/first.out")
second_list=$(awk -F= '$1 == "LIST" { print $2 }' "$tmpdir/second.out")

if [ -z "$first_fake" ] || [ -z "$second_fake" ] \
    || [ -z "$first_list" ] || [ -z "$second_list" ]; then
  echo "validate-only mode did not print default paths" >&2
  exit 1
fi
if [ "$first_fake" = "$second_fake" ] || [ "$first_list" = "$second_list" ]; then
  echo "default temp paths were reused" >&2
  exit 1
fi

case "$first_fake:$second_fake:$first_list:$second_list" in
  *XXXXXX*)
    echo "mktemp template leaked into runtime paths" >&2
    exit 1
    ;;
esac
