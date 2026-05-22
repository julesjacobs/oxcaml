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
expect_exe=${EXPECT_EXE:-$stage_build/main/oxcaml/testsuite/tools/expect.exe}
expectnat_exe=${EXPECTNAT_EXE:-$stage_build/main/oxcaml/testsuite/tools/expectnat.exe}
codegen_exe=${CODEGEN_EXE:-$stage_build/main/oxcaml/testsuite/tools/codegen_main.exe}
ocamltest_exe=${OCAMLTEST_EXE:-$repo/_runtest/ocamltest/ocamltest}
toplevel_dir=${TOPLEVEL_DIR:-$stage_build/main/toplevel/byte/.ocamltoplevel.objs/byte}
opttoplevel_dir=${OPTTOPLEVEL_DIR:-$stage_build/main/toplevel/native/.ocamlopttoplevel.objs/byte}
debugger_dir=${DEBUGGER_DIR:-$stage_build/main/debugger/.ocamldebug.objs/byte}
debugger_exe=${DEBUGGER_EXE:-$install_bin/ocamldebug}
fexprc_exe=${FEXPRC_EXE:-$stage_build/main/middle_end/flambda2/tests/tools/fexprc.exe}
config_obj=${CONFIG_OBJ:-$stage_build/main/.ocamlcommon.objs/native/config.o}

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
require_path "$expect_exe"
require_path "$expectnat_exe"
require_path "$codegen_exe"
require_path "$ocamltest_exe"
require_path "$wrapper"
require_path "$toplevel_dir/toploop.cmi"
require_path "$opttoplevel_dir/opttopdirs.cmi"
require_path "$debugger_dir/ocamldebug.cmi"
require_path "$debugger_exe"
require_path "$fexprc_exe"
require_path "$install_bin/ocamlmklib.byte"
require_path "$install_bin/dumpobj.byte"
require_path "$install_bin/ocamlobjinfo.byte"
require_path "$install_bin/ocamllex.byte"
require_path "$install_bin/ocamlyacc"
require_path "$install_lib/compiler-libs/ocamlcommon.cma"
require_path "$install_lib/compiler-libs/ocamlcommon.cmxa"
require_path "$config_obj"

shopt -s nullglob

mirror_ml_sources () {
  local src=$1
  local dst=$2
  [ -d "$src" ] || return 0
  mkdir -p "$dst"
  find "$dst" -type f \( -name '*.ml' -o -name '*.mli' -o -name '*.corrected' \) -delete
  (
    cd "$src"
    find . -type d \
      ! -path './_build*' \
      ! -path './_runtest*' \
      ! -path './.*' \
      -exec mkdir -p "$dst/{}" \;
    find . -type f \( -name '*.ml' -o -name '*.mli' \) \
      ! -path './_build*' \
      ! -path './_runtest*' \
      ! -path './.*' \
      -print
  ) | while IFS= read -r file; do
    mkdir -p "$dst/$(dirname "$file")"
    ln -f "$src/$file" "$dst/$file" 2>/dev/null \
      || cp -p "$src/$file" "$dst/$file"
  done
}

install_tool () {
  local tool=$1
  if [ -e "$tool.real" ]; then
    printf '%s.real' "$tool"
  else
    printf '%s' "$tool"
  fi
}

mkdir -p \
  "$fake_root" \
  "$fake_root/otherlibs" \
  "$fake_root/testsuite/tools" \
  "$fake_root/testsuite/lib"

ln -f "$repo/VERSION" "$fake_root/VERSION" 2>/dev/null \
  || cp -p "$repo/VERSION" "$fake_root/VERSION"
for file in Makefile.config Makefile.build_config; do
  if [ -e "$repo/$file" ]; then
    ln -f "$repo/$file" "$fake_root/$file" 2>/dev/null \
      || cp -p "$repo/$file" "$fake_root/$file"
  fi
done

for tool in "$install_bin"/*; do
  name=$(basename "$tool")
  ln -sfn "$(install_tool "$tool")" "$fake_root/$name"
done

ln -sfn "$(install_tool "$install_bin/ocamlc.byte")" "$fake_root/ocamlc"
ln -sfn "$(install_tool "$install_bin/ocamlopt.byte")" "$fake_root/ocamlopt"
ln -sfn "$(install_tool "$install_bin/ocamllex.byte")" "$fake_root/ocamllex"
ln -sfn "$stage_ocamlopt" "$fake_root/ocamlopt.opt"
ln -sfn "$(install_tool "$install_bin/ocamlyacc")" "$fake_root/ocamlyacc"
ln -sfn . "$fake_root/lex"
ln -sfn . "$fake_root/yacc"

ln -sfn "$runtime_dir" "$fake_root/runtime"
ln -sfn "$stdlib_dir" "$fake_root/stdlib"
ln -sfn "$install_lib/compiler-libs" "$fake_root/compilerlibs"
ln -sfn "$repo/_install" "$fake_root/_install"
ln -sfn "$repo/runtime5" "$fake_root/runtime5"
ln -sfn "$expect_exe" "$fake_root/testsuite/tools/expect"
ln -sfn "$expectnat_exe" "$fake_root/testsuite/tools/expectnat"
ln -sfn "$fexprc_exe" "$fake_root/testsuite/tools/fexprc"
ln -sfn "$codegen_exe" "$fake_root/testsuite/tools/codegen"
for file in "$stage_build"/main/oxcaml/testsuite/tools/asmgen_*.o; do
  ln -sfn "$file" "$fake_root/testsuite/tools/$(basename "$file")"
done

mkdir -p "$fake_root/ocamltest"
ln -sfn "$ocamltest_exe" "$fake_root/ocamltest/ocamltest"

rm -rf "$fake_root/toplevel"
mkdir -p "$fake_root/toplevel"
for file in "$toplevel_dir"/* "$opttoplevel_dir"/*; do
  ln -sfn "$file" "$fake_root/toplevel/$(basename "$file")"
done

rm -rf "$fake_root/utils"
mkdir -p "$fake_root/utils"
for file in "$install_lib"/compiler-libs/*; do
  ln -sfn "$file" "$fake_root/utils/$(basename "$file")"
done
ln -sfn "$config_obj" "$fake_root/utils/config.o"

for dir in asmcomp bytecomp driver file_formats lambda middle_end parsing typing; do
  mkdir -p "$fake_root/$dir"
done

for dir in asmcomp bytecomp driver file_formats lambda middle_end \
    ocamltest parsing testsuite typing utils; do
  mirror_ml_sources "$repo/$dir" "$fake_root/$dir"
done

rm -rf "$fake_root/debugger"
mkdir -p "$fake_root/debugger"
for file in "$debugger_dir"/*; do
  ln -sfn "$file" "$fake_root/debugger/$(basename "$file")"
done
ln -sfn "$(install_tool "$debugger_exe")" "$fake_root/debugger/ocamldebug"

rm -rf "$fake_root/tools"
mkdir -p "$fake_root/tools"
for file in "$repo"/tools/*; do
  ln -sfn "$file" "$fake_root/tools/$(basename "$file")"
done
ln -sfn "$(install_tool "$install_bin/ocamlmklib.byte")" "$fake_root/tools/ocamlmklib"
ln -sfn "$(install_tool "$install_bin/dumpobj.byte")" "$fake_root/tools/dumpobj"
ln -sfn "$(install_tool "$install_bin/ocamlobjinfo.byte")" "$fake_root/tools/ocamlobjinfo"

rm -rf \
  "$fake_root/otherlibs/unix" \
  "$fake_root/otherlibs/threads" \
  "$fake_root/otherlibs/systhreads" \
  "$fake_root/otherlibs/str"
for lib in unix threads str; do
  ln -sfn "$install_lib/$lib" "$fake_root/otherlibs/$lib"
done
ln -sfn "$install_lib/threads" "$fake_root/otherlibs/systhreads"
ln -sfn "$install_lib/stublibs" "$fake_root/stublibs"

rm -rf "$fake_root/otherlibs/dynlink"
mkdir -p "$fake_root/otherlibs/dynlink/native"
for file in "$install_lib"/dynlink/dynlink*; do
  ln -sfn "$file" "$fake_root/otherlibs/dynlink/$(basename "$file")"
done

for universe in upstream_compatible stable beta alpha; do
  rm -rf "$fake_root/otherlibs/$universe" "$fake_root/otherlibs/stdlib_$universe"
  mkdir -p "$fake_root/otherlibs/$universe"
  ln -sfn "$install_lib/stdlib_$universe" \
    "$fake_root/otherlibs/stdlib_$universe"
done

rm -rf "$fake_root/otherlibs/eval"
mkdir -p "$fake_root/otherlibs/eval"
for file in "$install_lib"/eval* "$install_lib"/libeval*; do
  ln -sfn "$file" "$fake_root/otherlibs/eval/$(basename "$file")"
done

rm -rf "$fake_root/otherlibs/runtime_events"
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
lib_src=$repo/testsuite/lib/lib
ocamlc=$install_bin/ocamlc.byte
ocamlopt=$stage_ocamlopt
testing_dir=$fake_root/testsuite/lib

"$ocamlc" -nostdlib -I "$stdlib_dir" -c -o "$testing_dir/lib.cmi" \
  "$lib_src.mli"
"$ocamlc" -nostdlib -I "$stdlib_dir" -I "$testing_dir" -c -o "$testing_dir/lib.cmo" \
  "$lib_src.ml"
"$ocamlc" -nostdlib -I "$stdlib_dir" -a -o "$testing_dir/lib.cma" \
  "$testing_dir/lib.cmo"
OCAMLLIB="$stdlib_dir" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
  "$ocamlopt" -nostdlib -I "$stdlib_dir" -I "$testing_dir" -c -o "$testing_dir/lib.cmx" \
  "$lib_src.ml"
OCAMLLIB="$stdlib_dir" \
OCAMLPARAM="_,llvm-backend=1,llvm-path=$wrapper" \
  "$ocamlopt" -nostdlib -I "$stdlib_dir" -a -o "$testing_dir/lib.cmxa" \
  "$testing_dir/lib.cmx"

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
