#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

fake_root=${FAKE_ROOT:-/tmp/oxcaml-stage4-ocamltest-src}
normal_build=${NORMAL_BUILD:-$repo/_normal_stage1_fpfix_build}
stage_build=${STAGE_BUILD:-$repo/_llvm_stage4_probe_build}
stdlib_dir=${STDLIB_DIR:-$repo/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}

install_bin=${INSTALL_BIN:-$normal_build/install/main/bin}
install_lib=${INSTALL_LIB:-$normal_build/install/main/lib/ocaml}
stdlib_stable_dir=${STDLIB_STABLE_DIR:-$install_lib/stdlib_stable}
runtime_dir=${RUNTIME_DIR_PATH:-$normal_build/main/runtime}
stage_ocamlopt=$stage_build/main/oxcaml_main_native.exe
toplevel_dir=${TOPLEVEL_DIR:-$stage_build/main/toplevel/byte/.ocamltoplevel.objs/byte}

require_path () {
  if [ ! -e "$1" ]; then
    echo "missing required path: $1" >&2
    exit 1
  fi
}

require_path "$install_bin/ocamlc.byte"
require_path "$install_bin/ocamlopt.byte"
require_path "$stage_ocamlopt"
require_path "$stdlib_dir/stdlib.cmxa"
require_path "$stdlib_stable_dir/stdlib_stable.cma"
require_path "$runtime_dir/ocamlrun"
require_path "$repo/_runtest/testsuite/tools/expect"
require_path "$repo/_runtest/testsuite/tools/expectnat"
require_path "$wrapper"
require_path "$toplevel_dir/toploop.cmi"

shopt -s nullglob

mkdir -p \
  "$fake_root" \
  "$fake_root/otherlibs" \
  "$fake_root/testsuite/tools" \
  "$fake_root/testsuite/lib"

for tool in "$install_bin"/*; do
  name=$(basename "$tool")
  ln -sfn "$tool" "$fake_root/$name"
done

ln -sfn "$install_bin/ocamlc.byte" "$fake_root/ocamlc"
ln -sfn "$install_bin/ocamlopt.byte" "$fake_root/ocamlopt"
ln -sfn "$stage_ocamlopt" "$fake_root/ocamlopt.opt"

ln -sfn "$runtime_dir" "$fake_root/runtime"
ln -sfn "$stdlib_dir" "$fake_root/stdlib"
ln -sfn "$toplevel_dir" "$fake_root/toplevel"
ln -sfn "$repo/_install" "$fake_root/_install"
ln -sfn "$repo/runtime5" "$fake_root/runtime5"
ln -sfn "$repo/tools" "$fake_root/tools"
ln -sfn "$repo/_runtest/testsuite/tools/expect" "$fake_root/testsuite/tools/expect"
ln -sfn "$repo/_runtest/testsuite/tools/expectnat" "$fake_root/testsuite/tools/expectnat"

for lib in unix threads str; do
  ln -sfn "$install_lib/$lib" "$fake_root/otherlibs/$lib"
done
ln -sfn "$install_lib/threads" "$fake_root/otherlibs/systhreads"
ln -sfn "$install_lib/stublibs" "$fake_root/stublibs"

for universe in upstream_compatible stable beta alpha; do
  mkdir -p "$fake_root/otherlibs/$universe"
  ln -sfn "$install_lib/stdlib_$universe" \
    "$fake_root/otherlibs/stdlib_$universe"
done

mkdir -p "$fake_root/otherlibs/eval"
for file in "$install_lib"/eval* "$install_lib"/libeval*; do
  ln -sfn "$file" "$fake_root/otherlibs/eval/$(basename "$file")"
done

mkdir -p "$fake_root/otherlibs/runtime_events"
for file in "$install_lib"/runtime_events/{lib,}runtime_events*; do
  ln -sfn "$file" "$fake_root/otherlibs/runtime_events/$(basename "$file")"
done

ln -sfn "$install_lib/libthreadsnat_stubs.a" "$install_lib/threads/libthreadsnat_stubs.a"
ln -sfn "$install_lib/libthreadsnat_stubs_native.a" \
  "$install_lib/threads/libthreadsnat_stubs_native.a"
ln -sfn "$install_lib/libthreads_stubs.a" "$install_lib/threads/libthreads_stubs.a"
ln -sfn "$install_lib/caml/threads.h" "$runtime_dir/caml/threads.h"
ln -sfn "$normal_build/main/otherlibs/systhreads/threads.h" "$runtime_dir/threads.h"

testing_src=$repo/testsuite/lib/testing
ocamlc=$install_bin/ocamlc.byte
ocamlopt=$stage_ocamlopt
testing_dir=$fake_root/testsuite/lib

"$ocamlc" -nostdlib -I "$stdlib_dir" -c -o "$testing_dir/testing.cmi" \
  "$testing_src.mli"
"$ocamlc" -nostdlib -I "$stdlib_dir" -I "$testing_dir" -c -o "$testing_dir/testing.cmo" \
  "$testing_src.ml"
"$ocamlc" -nostdlib -I "$stdlib_dir" -a -linkall -o "$testing_dir/testing.cma" \
  "$testing_dir/testing.cmo"
OCAMLLIB="$stdlib_dir" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
  "$ocamlopt" -nostdlib -I "$stdlib_dir" -I "$testing_dir" -c -o "$testing_dir/testing.cmx" \
  "$testing_src.ml"
OCAMLLIB="$stdlib_dir" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
  "$ocamlopt" -nostdlib -I "$stdlib_dir" -a -linkall -o "$testing_dir/testing.cmxa" \
  "$testing_dir/testing.cmx"

cat <<EOF
Fake OCAMLSRCDIR ready:
  $fake_root

Use:
  OCAMLSRCDIR=$fake_root \\
  CAML_LD_LIBRARY_PATH=$fake_root/stublibs \\
  OCAMLPARAM=_,llvm-backend=1,llvm-path=$wrapper \\
  OCAMLLIB=$stdlib_dir \\
  make -C testsuite one LIST=/path/to/list ocamltest_directory=../_runtest/ocamltest
EOF
