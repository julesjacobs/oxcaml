#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
alloc_src="$build_dir/async_allocation_try.ml"
stack_src="$build_dir/async_stack_overflow_try.ml"
llvm_path="${LLVM_PATH:-${LLVM_WRAPPER:-/tmp/oxcaml-clang-wrapper}}"
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

run_ocamlopt_with_test_stdlib() {
  if [ -n "$stdlib_dir_arg" ]; then
    set -- -I "$stdlib_dir_arg" "$@"
  fi
  if [ -n "$extra_link_ccopt" ]; then
    set -- -ccopt "$extra_link_ccopt" "$@"
  fi
  "$ocamlopt" "$@"
}

cat > "$alloc_src" <<'EOF'
let () = Sys.catch_break true

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () -> finished := true; raise Sys.Break) b;
  ref (Some b)

let () =
  let finished = ref false in
  let r = allocate_bytes finished in
  let result =
    try
      Sys.with_async_exns (fun () ->
        try
          r := None;
          let count = ref 0 in
          while true do
            incr count;
            if !count > 5_000_000 then failwith "finalizer did not run";
            let _ @ global = Sys.opaque_identity (42, !count) in
            ()
          done;
          "no-exn"
        with Sys.Break -> "inner")
    with Sys.Break ->
      if !finished then "outer" else "outer-before-finalizer"
  in
  print_endline result
EOF

cat > "$stack_src" <<'EOF'
let[@inline never] use x = Sys.opaque_identity x

let rec consume n =
  let a0 = use n in
  let a1 = use (n + 1) in
  let a2 = use (n + 2) in
  let a3 = use (n + 3) in
  1 + consume (a0 + a1 + a2 + a3)

let () =
  let result =
    try
      Sys.with_async_exns (fun () ->
        try ignore (consume 0); "no-exn"
        with Stack_overflow -> "inner")
    with Stack_overflow -> "outer"
  in
  print_endline result
EOF

run_ocamlopt_with_test_stdlib -O3 \
  -o "$build_dir/async_allocation_native.exe" "$alloc_src"
"$build_dir/async_allocation_native.exe" > "$build_dir/async_allocation_native.out"
grep -q '^outer$' "$build_dir/async_allocation_native.out"

run_ocamlopt_with_test_stdlib -O3 -llvm-backend \
  -llvm-path "$llvm_path" \
  -o "$build_dir/async_allocation_llvm.exe" "$alloc_src"
"$build_dir/async_allocation_llvm.exe" > "$build_dir/async_allocation_llvm.out"
grep -q '^outer$' "$build_dir/async_allocation_llvm.out"

run_ocamlopt_with_test_stdlib -O3 \
  -o "$build_dir/async_stack_native.exe" "$stack_src"
set +e
OCAMLRUNPARAM=l=100000 "$build_dir/async_stack_native.exe" \
  > "$build_dir/async_stack_native.out" 2> "$build_dir/async_stack_native.err"
native_stack_status=$?
set -e
grep -q '^outer$' "$build_dir/async_stack_native.out"

run_ocamlopt_with_test_stdlib -O3 -S -keep-llvmir -llvm-backend \
  -llvm-path "$llvm_path" \
  -o "$build_dir/async_stack_llvm.exe" "$stack_src"
set +e
OCAMLRUNPARAM=l=100000 "$build_dir/async_stack_llvm.exe" \
  > "$build_dir/async_stack_llvm.out" 2> "$build_dir/async_stack_llvm.err"
llvm_stack_status=$?
set -e
grep -q '^outer$' "$build_dir/async_stack_llvm.out"

if [ "$llvm_stack_status" != "$native_stack_status" ]; then
  echo "LLVM stack overflow status $llvm_stack_status differs from native $native_stack_status" >&2
  echo "native stderr:" >&2
  cat "$build_dir/async_stack_native.err" >&2
  echo "LLVM stderr:" >&2
  cat "$build_dir/async_stack_llvm.err" >&2
  exit 1
fi

grep -Eq '@"\\01_?caml_llvm_call_realloc_stack"' \
  "$build_dir/async_stack_overflow_try.ll"
case "$host_arch" in
  arm64 | aarch64)
    grep -q 'bl[[:space:]]*_caml_llvm_call_realloc_stack' \
      "$build_dir/async_stack_overflow_try.s"
    ;;
  x86_64 | amd64)
    grep -Eq 'callq?[[:space:]]+_?caml_llvm_call_realloc_stack' \
      "$build_dir/async_stack_overflow_try.s"
    ;;
  *)
    echo "unsupported architecture for async_control_transfer: $host_arch" >&2
    exit 1
    ;;
esac
