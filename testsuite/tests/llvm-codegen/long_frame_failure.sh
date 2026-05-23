#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/long_frame_generated.ml"
out="$build_dir/long_frame_generated.exe"
stdout_file="$build_dir/long_frame_stdout.txt"
stderr_file="$build_dir/long_frame_stderr.txt"

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
  ocamlopt="_build/install/main/bin/ocamlopt.opt"
fi

count=4300

{
  echo "external opaque : 'a -> 'a = \"%opaque\""
  echo "let[@inline never] f () ="
  i=1
  while [ "$i" -le "$count" ]; do
    echo "  let a$i = opaque (ref $i) in"
    i=$((i + 1))
  done
  echo "  Gc.minor ();"
  echo "  let s = ref 0 in"
  i=1
  while [ "$i" -le "$count" ]; do
    echo "  s := !s + !(opaque a$i);"
    i=$((i + 1))
  done
  echo "  !s"
  echo "let () = Printf.printf \"%d\\n\" (f ())"
} > "$src"

set +e
"$ocamlopt" -O3 -g -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src" > "$stdout_file" 2> "$stderr_file"
compile_status=$?
set -e

if [ "$compile_status" -eq 0 ]; then
  "$out" > "$stdout_file"
  exit 0
fi

if grep -q "\\[OxCamlGCPrinter\\] frame size requires long frames" "$stderr_file"; then
  # CR: This exhibits the current problem.  The LLVM frametable printer only
  # emits the short frame-descriptor format, so a function with a large static
  # frame aborts in LLVM codegen instead of compiling like the native backend's
  # long-frame path.
  exit 0
fi

cat "$stderr_file"
exit "$compile_status"
