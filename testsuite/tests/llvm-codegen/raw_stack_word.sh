#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/raw_stack_word_generated.ml"
out="$build_dir/raw_stack_word_generated.exe"
stdout_file="$build_dir/raw_stack_word_stdout.txt"
stderr_file="$build_dir/raw_stack_word_stderr.txt"
stub_obj="$build_dir/raw_stack_word_stubs.o"
extra_link_ccopt=""

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64) extra_link_ccopt="-no-pie" ;;
esac

search_dir=$build_dir
ocamlopt=""
while [ "$search_dir" != "/" ]; do
  if [ -f "$search_dir/ocamlopt.opt" ] && [ -x "$search_dir/ocamlopt.opt" ]; then
    ocamlopt="$search_dir/ocamlopt.opt"
    break
  fi
  search_dir=$(dirname "$search_dir")
done

if [ -z "$ocamlopt" ]; then
  if [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
    ocamlopt="$OCAMLSRCDIR/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

ocamlopt_dir=$(dirname "$ocamlopt")
stdlib_dir_arg=""
for stdlib_dir in \
  "$ocamlopt_dir/../_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib" \
  "$ocamlopt_dir/../runtime_stdlib_install/lib/ocaml_runtime_stdlib" \
  "$source_dir/../../../_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib" \
  "$source_dir/../../../../_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib" \
  "$ocamlopt_dir/utils" \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml"
do
  if [ -f "$stdlib_dir/std_exit.cmx" ] || [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_dir_arg="$stdlib_dir"
    break
  fi
done

stdlib_upstream_dir_arg=""
for stdlib_upstream_dir in \
  "$ocamlopt_dir/otherlibs/stdlib_upstream_compatible" \
  "$ocamlopt_dir/lib/ocaml/stdlib_upstream_compatible" \
  "$ocamlopt_dir/_install/lib/ocaml/stdlib_upstream_compatible" \
  "$ocamlopt_dir/../lib/ocaml/stdlib_upstream_compatible"
do
  if [ -f "$stdlib_upstream_dir/stdlib_upstream_compatible.cmxa" ]; then
    stdlib_upstream_dir_arg="$stdlib_upstream_dir"
    break
  fi
done

runtime_include_dir_arg=""
for runtime_dir in \
  "$source_dir/../../../runtime" \
  "$ocamlopt_dir/runtime" \
  "$ocamlopt_dir/../runtime" \
  "$ocamlopt_dir/../../runtime"
do
  if [ -f "$runtime_dir/caml/alloc.h" ]; then
    runtime_include_dir_arg="$runtime_dir"
    break
  fi
done

run_ocamlopt_with_test_paths() {
  if [ -n "$stdlib_upstream_dir_arg" ]; then
    set -- -I "$stdlib_upstream_dir_arg" "$@"
  else
    set -- -I +stdlib_upstream_compatible "$@"
  fi
  if [ -n "$stdlib_dir_arg" ]; then
    set -- -I "$stdlib_dir_arg" "$@"
  fi
  "$ocamlopt" "$@"
}

{
  echo "open Stdlib_upstream_compatible"
  echo
  echo "external raw_stack_base : unit -> nativeint# ="
  echo "  \"raw_stack_base_bytecode\" \"raw_stack_base_native\" [@@noalloc]"
  echo
  echo "external raw_stack_high : unit -> nativeint# ="
  echo "  \"raw_stack_high_bytecode\" \"raw_stack_high_native\" [@@noalloc]"
  echo
  echo "let[@inline never] id x = Sys.opaque_identity x"
  echo
  echo "let[@inline never] rec grow n (keep : nativeint#) ="
  echo "  let keep = id keep in"
  echo "  if n = 0 then keep"
  echo "  else"
  echo "    let r = grow (n - 1) keep in"
  echo "    if not (Nativeint_u.equal keep r) then failwith \"raw value changed\";"
  echo "    r"
  echo
  echo "let () ="
  echo "  let keep = raw_stack_base () in"
  echo "  let before = Nativeint_u.to_nativeint keep in"
  echo "  let before_s = Nativeint.to_string before in"
  echo "  let before_hex = Printf.sprintf \"%nx\" before in"
  echo "  let high_before = Nativeint_u.to_nativeint (raw_stack_high ()) in"
  echo "  let after = Nativeint_u.to_nativeint (grow 20000 keep) in"
  echo "  let after_s = Nativeint.to_string after in"
  echo "  let after_hex = Printf.sprintf \"%nx\" after in"
  echo "  let base_after = Nativeint_u.to_nativeint (raw_stack_base ()) in"
  echo "  let high_after = Nativeint_u.to_nativeint (raw_stack_high ()) in"
  echo "  if not (String.equal before_s after_s) then begin"
  echo "    Printf.eprintf"
  echo "      \"before=%s/%s after=%s/%s high_before=%nx base_after=%nx high_after=%nx\\n\""
  echo "      before_s before_hex after_s after_hex high_before base_after high_after;"
  echo "    failwith \"raw stack-looking nativeint# was rewritten\""
  echo "  end;"
  echo "  if Nativeint.equal before base_after && Nativeint.equal high_before high_after"
  echo "  then failwith \"stack did not grow\""
} > "$src"

if [ -n "$runtime_include_dir_arg" ]; then
  run_ocamlopt_with_test_paths -ccopt "-I$runtime_include_dir_arg" \
    -O3 -llvm-backend \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -c -o "$stub_obj" "$build_dir/raw_stack_word_stubs.c"
else
  run_ocamlopt_with_test_paths -O3 -llvm-backend \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -c -o "$stub_obj" "$build_dir/raw_stack_word_stubs.c"
fi

if [ -n "$extra_link_ccopt" ]; then
  run_ocamlopt_with_test_paths -O3 -llvm-backend \
    -ccopt "$extra_link_ccopt" \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$out" stdlib_upstream_compatible.cmxa "$stub_obj" "$src"
else
  run_ocamlopt_with_test_paths -O3 -llvm-backend \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$out" stdlib_upstream_compatible.cmxa "$stub_obj" "$src"
fi

set +e
OCAMLRUNPARAM="l=1M" "$out" > "$stdout_file" 2> "$stderr_file"
run_status=$?
set -e

if [ "$run_status" -eq 0 ]; then
  exit 0
fi

cat "$stderr_file"
exit "$run_status"
