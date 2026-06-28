#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/amd64_noalloc_direct_call_generated.ml"
stub_src="$build_dir/amd64_noalloc_direct_call_stubs.c"
stub_obj="$build_dir/amd64_noalloc_direct_call_stubs.o"
exe="$build_dir/amd64_noalloc_direct_call_generated.exe"
ir="$build_dir/amd64_noalloc_direct_call_generated.ll"
asm="$build_dir/amd64_noalloc_direct_call_generated.s"
stdout_file="$build_dir/amd64_noalloc_direct_call.out"
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
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
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
external noalloc_add : int -> int =
  "amd64_noalloc_add_bytecode" "amd64_noalloc_add_native" [@@noalloc]

let[@inline never] f x = noalloc_add (Sys.opaque_identity x)

let () =
  let n = f 39 in
  if n <> 42 then Printf.ksprintf failwith "expected 42, got %d" n;
  print_endline "ok"
EOF

cat > "$stub_src" <<'EOF'
#include <caml/mlvalues.h>

CAMLprim value amd64_noalloc_add_native(value x)
{
  return Val_long(Long_val(x) + 3);
}

CAMLprim value amd64_noalloc_add_bytecode(value x)
{
  return amd64_noalloc_add_native(x);
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

awk '
  /define .*__f_.*_code/ {
    in_f = 1
    found = 1
    state = 0
  }
  in_f && /^}/ {
    checked = state == 1
    in_f = 0
  }
  in_f && state == 0 && /call[[:space:]]+oxcaml_c_directcc .*@"\\01_?amd64_noalloc_add_native"/ {
    state = 1
  }
  END {
    exit found && checked ? 0 : 1
  }
' "$ir"

if grep -q 'c_call_wrapper' "$ir"; then
  echo "noalloc direct C call should not use a generated C-call wrapper" >&2
  exit 1
fi

grep -Eq 'callq?[[:space:]]+_?amd64_noalloc_add_native' "$asm"

awk '
  /^_?caml.*__f_.*_code:/ {
    in_f = 1
    found = 1
    state = "entry"
    saw_cfi_start = 0
    saw_cfi_remember = 0
    saw_cfi_c_stack = 0
    saw_cfi_restore = 0
    saw_cfi_return = 0
    saw_c_stack_load = 0
    saw_ret = 0
    next
  }
  in_f && /^[[:space:]]*\.cfi_startproc/ {
    saw_cfi_start = 1
    next
  }
  in_f && state == "entry" && /^[[:space:]]*movq[[:space:]]+%rsp,[[:space:]]+%r[a-z0-9]+/ {
    saved_rsp_reg = $3
    sub(/,$/, "", saved_rsp_reg)
    if (saved_rsp_reg == "%rbp") {
      next
    }
    if (saved_rsp_reg != "%r13") {
      bad = 1
      next
    }
    state = "saved_ocaml_rsp"
    next
  }
  in_f && state == "saved_ocaml_rsp" && /^[[:space:]]*\.cfi_remember_state/ {
    saw_cfi_remember = 1
    next
  }
  in_f && state == "saved_ocaml_rsp" && /^[[:space:]]*\.cfi_def_cfa_register[[:space:]]+%r13/ {
    saw_cfi_c_stack = 1
    next
  }
  in_f && state == "saved_ocaml_rsp" && /^[[:space:]]*movq[[:space:]]+104\(%r14\),[[:space:]]+%rsp/ {
    saw_c_stack_load = 1
    state = "entered_c_stack"
    next
  }
  in_f && state == "entered_c_stack" && /^[[:space:]]*callq?[[:space:]]+_?amd64_noalloc_add_native/ {
    state = "called_c"
    next
  }
  in_f && state == "called_c" && /^[[:space:]]*movq[[:space:]]+%r[a-z0-9]+,[[:space:]]+%rsp/ {
    restored_rsp_reg = $2
    sub(/,$/, "", restored_rsp_reg)
    if (restored_rsp_reg != saved_rsp_reg) {
      bad = 1
      next
    }
    state = "restored_ocaml_rsp"
    next
  }
  in_f && state == "restored_ocaml_rsp" && /^[[:space:]]*\.cfi_restore_state/ {
    saw_cfi_restore = 1
    next
  }
  in_f && /^[[:space:]]*\.cfi_def_cfa[[:space:]]+%rsp,[[:space:]]+8/ {
    saw_cfi_return = 1
    next
  }
  in_f && /^[[:space:]]*retq/ {
    saw_ret = 1
    next
  }
  in_f && /^[[:space:]]*\.cfi_endproc/ {
    if (saw_cfi_start && saw_cfi_remember && saw_cfi_c_stack && saw_cfi_restore && saw_cfi_return && saw_ret && saw_c_stack_load && state == "restored_ocaml_rsp") {
      checked = 1
      in_f = 0
      next
    }
    bad = 1
    in_f = 0
  }
  END {
    exit found && checked && !bad ? 0 : 1
  }
' "$asm"
