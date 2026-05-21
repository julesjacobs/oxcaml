#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

fake_root=${FAKE_ROOT:-/tmp/oxcaml-stage4-ocamltest-src}
normal_build=${NORMAL_BUILD:-$repo/_normal_stage1_fpfix_build}
stage_build=${STAGE_BUILD:-$repo/_llvm_stage4_probe_build}
stdlib_dir=${STDLIB_DIR:-$repo/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib}
wrapper=${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}

install_bin=$normal_build/install/main/bin
install_lib=$normal_build/install/main/lib/ocaml
runtime_dir=$normal_build/main/runtime
stage_ocamlopt=$stage_build/main/oxcaml_main_native.exe

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
require_path "$runtime_dir/ocamlrun"
require_path "$repo/_runtest/testsuite/tools/expect"
require_path "$repo/_runtest/testsuite/tools/expectnat"
require_path "$wrapper"

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
ln -sfn "$repo/_install" "$fake_root/_install"
ln -sfn "$repo/runtime5" "$fake_root/runtime5"
ln -sfn "$repo/tools" "$fake_root/tools"
ln -sfn "$repo/_runtest/testsuite/tools/expect" "$fake_root/testsuite/tools/expect"
ln -sfn "$repo/_runtest/testsuite/tools/expectnat" "$fake_root/testsuite/tools/expectnat"

for lib in unix threads str stdlib_stable; do
  ln -sfn "$install_lib/$lib" "$fake_root/otherlibs/$lib"
done
ln -sfn "$install_lib/threads" "$fake_root/otherlibs/systhreads"
ln -sfn "$install_lib/stublibs" "$fake_root/stublibs"

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
