#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/amd64_c_stack_args_roots_generated.ml"
stub_src="$build_dir/amd64_c_stack_args_roots_stubs.c"
stub_obj="$build_dir/amd64_c_stack_args_roots_stubs.o"
exe="$build_dir/amd64_c_stack_args_roots_generated.exe"
ir="$build_dir/amd64_c_stack_args_roots_generated.ll"
asm="$build_dir/amd64_c_stack_args_roots_generated.s"
stdout_file="$build_dir/amd64_c_stack_args_roots.out"
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
  src_root=$(cd "$source_dir/../../.." && pwd)
  if [ -x "$src_root/_install/bin/ocamlopt.opt" ]; then
    ocamlopt="$src_root/_install/bin/ocamlopt.opt"
  elif [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
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

runtime_include_dir_arg=""
for runtime_dir in \
  "$source_dir/../../../runtime" \
  "$ocamlopt_dir/runtime" \
  "$ocamlopt_dir/../runtime" \
  "$ocamlopt_dir/../../runtime"
do
  if [ -f "$runtime_dir/caml/mlvalues.h" ]; then
    runtime_include_dir_arg="$runtime_dir"
    break
  fi
done

run_ocamlopt_with_test_stdlib() {
  if [ -n "$stdlib_dir_arg" ]; then
    set -- -I "$stdlib_dir_arg" "$@"
  fi
  "$ocamlopt" "$@"
}

cat > "$src" <<'EOF'
external stack_args_gc :
  int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> bytes -> int =
  "amd64_stack_args_gc_bytecode" "amd64_stack_args_gc_native"

let[@inline never] call keep =
  let r = stack_args_gc 1 2 3 4 5 6 7 8 9 10 11 12 keep in
  if Bytes.get keep 0 <> 'z' then failwith "root changed";
  r + Bytes.length keep

let () =
  let keep = Bytes.make 17 'z' in
  let r = call keep in
  if r <> 667 then Printf.ksprintf failwith "expected 667, got %d" r;
  print_endline "ok"
EOF

cat > "$stub_src" <<'EOF'
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/minor_gc.h>
#include <caml/mlvalues.h>

CAMLprim value amd64_stack_args_gc_native(value a1, value a2, value a3,
                                          value a4, value a5, value a6,
                                          value a7, value a8, value a9,
                                          value a10, value a11, value a12,
                                          value keep)
{
  CAMLparam5(a1, a2, a3, a4, a5);
  CAMLxparam5(a6, a7, a8, a9, a10);
  CAMLxparam3(a11, a12, keep);
  CAMLlocal1(block);
  for (int i = 0; i < 10000; i++) {
    block = caml_alloc(2, 0);
    Field(block, 0) = Val_long(i);
    Field(block, 1) = Val_long(i + 1);
  }
  caml_minor_collection();
  for (int i = 0; i < 10000; i++) {
    block = caml_alloc(2, 0);
    Field(block, 0) = Val_long(10000 + i);
    Field(block, 1) = Val_long(10001 + i);
  }
  if (Byte(keep, 0) != 'z') {
    CAMLreturn(Val_long(-1));
  }
  intnat sum = 1 * Long_val(a1) + 2 * Long_val(a2) + 3 * Long_val(a3)
             + 4 * Long_val(a4) + 5 * Long_val(a5) + 6 * Long_val(a6)
             + 7 * Long_val(a7) + 8 * Long_val(a8) + 9 * Long_val(a9)
             + 10 * Long_val(a10) + 11 * Long_val(a11)
             + 12 * Long_val(a12);
  CAMLreturn(Val_long(sum));
}

CAMLprim value amd64_stack_args_gc_bytecode(value *argv, int argn)
{
  (void) argn;
  return amd64_stack_args_gc_native(argv[0], argv[1], argv[2], argv[3],
                                    argv[4], argv[5], argv[6], argv[7],
                                    argv[8], argv[9], argv[10], argv[11],
                                    argv[12]);
}
EOF

if [ -n "$runtime_include_dir_arg" ]; then
  run_ocamlopt_with_test_stdlib -ccopt "-I$runtime_include_dir_arg" \
    -c -o "$stub_obj" "$stub_src"
else
  run_ocamlopt_with_test_stdlib -c -o "$stub_obj" "$stub_src"
fi

if [ -n "$extra_link_ccopt" ]; then
  run_ocamlopt_with_test_stdlib -O3 -S -keep-llvmir -llvm-backend \
    -ccopt "$extra_link_ccopt" \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$exe" "$stub_obj" "$src"
else
  run_ocamlopt_with_test_stdlib -O3 -S -keep-llvmir -llvm-backend \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$exe" "$stub_obj" "$src"
fi

"$exe" > "$stdout_file"
grep -q '^ok$' "$stdout_file"

grep -Eq '@"\\01_?caml_c_call_stack_args".*"statepoint-id"=' "$ir"
grep -q '@llvm.stacksave' "$ir"
grep -q '@llvm.stackrestore' "$ir"
grep -Eq '@"\\01_?amd64_stack_args_gc_native"' "$ir"
grep -Eq 'callq?[[:space:]]+_?caml_c_call_stack_args' "$asm"

awk '
  function without_comma(value) {
    sub(/,$/, "", value)
    return value
  }
  /^_?caml.*__call_.*_code:/ {
    in_call = 1
    found = 1
    checked = 0
    state = "entry"
    saw_stack_save = 0
    saw_stack_save_spill = 0
    saw_stack_save_reload = 0
    next
  }
  in_call && !saw_stack_save && /^[[:space:]]*movq[[:space:]]+%rsp,[[:space:]]+%r[a-z0-9]+/ {
    stack_save_reg = $0
    sub(/^.*%rsp,[[:space:]]*/, "", stack_save_reg)
    sub(/[[:space:]].*$/, "", stack_save_reg)
    if (stack_save_reg == "%rbp") {
      next
    }
    saw_stack_save = 1
    next
  }
  in_call && saw_stack_save && !saw_stack_save_spill && $0 ~ "^[[:space:]]*movq[[:space:]]+" stack_save_reg ",[[:space:]]+-?[0-9]+\\(%rbp\\)" {
    stack_save_slot = $0
    sub(/^[[:space:]]*movq[[:space:]]+%r[a-z0-9]+,[[:space:]]*/, "", stack_save_slot)
    sub(/[[:space:]].*$/, "", stack_save_slot)
    saw_stack_save_spill = 1
    next
  }
  in_call && saw_stack_save && /^[[:space:]]*callq?[[:space:]]+_?caml_c_call_stack_args/ {
    state = "called_helper"
    next
  }
  in_call && state == "called_helper" && $1 == "movq" && without_comma($2) == stack_save_reg && $3 == "%rsp" {
    checked = 1
    in_call = 0
    next
  }
  in_call && state == "called_helper" && saw_stack_save_spill && $1 == "movq" && without_comma($2) == stack_save_slot && $3 == "%rsp" {
    checked = 1
    in_call = 0
    next
  }
  in_call && state == "called_helper" && saw_stack_save_spill && !saw_stack_save_reload && $1 == "movq" && without_comma($2) == stack_save_slot && $3 ~ /^%r[a-z0-9]+$/ {
    stack_save_reload_reg = $3
    saw_stack_save_reload = 1
    next
  }
  in_call && state == "called_helper" && saw_stack_save_reload && $1 == "movq" && without_comma($2) == stack_save_reload_reg && $3 == "%rsp" {
    checked = 1
    in_call = 0
    next
  }
  in_call && /^[[:space:]]*\.cfi_endproc/ {
    in_call = 0
  }
  END {
    exit found && checked ? 0 : 1
  }
' "$asm"
