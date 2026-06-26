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
  /define .*__f_.*_code/ { in_f = 1 }
  in_f && /^}/ {
    exit state == 4 ? 0 : 1
  }
  in_f && state == 0 && /@llvm\.read_register\.i64\(metadata !\{!"rsp\\00"\}\)/ {
    state = 1
  }
  in_f && state == 1 && /@llvm\.write_register\.i64\(metadata !\{!"rsp\\00"\}/ {
    state = 2
  }
  in_f && state == 2 && /call[[:space:]]+oxcaml_c_directcc .*@"\\01_?amd64_noalloc_add_native"/ {
    state = 3
  }
  in_f && state == 3 && /@llvm\.write_register\.i64\(metadata !\{!"rsp\\00"\}/ {
    state = 4
  }
' "$ir"

if grep -q 'c_call_wrapper' "$ir"; then
  echo "noalloc direct C call should not use a generated C-call wrapper" >&2
  exit 1
fi

grep -Eq 'callq?[[:space:]]+_?amd64_noalloc_add_native' "$asm"
