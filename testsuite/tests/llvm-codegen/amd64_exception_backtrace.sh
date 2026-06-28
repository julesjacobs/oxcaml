#!/bin/sh

set -eu

build_dir=$(pwd)
source_dir=${test_source_directory:-$(dirname "$0")}
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/amd64_exception_backtrace_generated.ml"
exe="$build_dir/amd64_exception_backtrace_generated.exe"
asm="$build_dir/amd64_exception_backtrace_generated.s"
stdout_file="$build_dir/amd64_exception_backtrace.out"
extra_link_ccopt=""
linux_debug_checks=false

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64)
    extra_link_ccopt="-no-pie"
    linux_debug_checks=true
    ;;
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
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
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

cat > "$src" <<'EOF'
let contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec search i =
    if i + needle_len > haystack_len then false
    else
      let rec matches j =
        j = needle_len
        || (haystack.[i + j] = needle.[j] && matches (j + 1))
      in
      matches 0 || search (i + 1)
  in
  search 0

let[@inline never] make_payload n =
  Array.init 7 (fun i -> string_of_int (n + i))

let[@inline never] rec raise_from_deep_stack n acc =
  let payload = make_payload (n + acc) in
  if n = 0 then raise (Failure payload.(3));
  String.length payload.(0) + raise_from_deep_stack (n - 1) (acc + 1)

let () =
  Printexc.record_backtrace true;
  let result =
    try raise_from_deep_stack 256 0 with
    | Failure s ->
      let bt = Printexc.get_backtrace () in
      if s <> "259" then Printf.ksprintf failwith "bad payload: %s" s;
      if not (contains bt "raise_from_deep_stack")
      then failwith "missing recursive frame in backtrace";
      if not (contains bt "amd64_exception_backtrace_generated.ml")
      then failwith "missing source location in backtrace";
      let after = Array.init 64 (fun i -> i + String.length s) in
      Array.fold_left ( + ) 0 after
  in
  Printf.printf "exception:%d\n" result
EOF

run_ocamlopt_with_test_stdlib -g -O3 -S -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$src"

OCAMLRUNPARAM="b,l=1M" "$exe" > "$stdout_file"
grep -q "^exception:2208$" "$stdout_file"

if [ "$linux_debug_checks" = true ]; then
  grep -Eq '^[[:space:]]*\.file[[:space:]]+0[[:space:]].*"amd64_exception_backtrace_generated\.ml"' "$asm"
  grep -q '\.debug_info' "$asm"
fi
