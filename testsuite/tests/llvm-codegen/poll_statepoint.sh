#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/poll_statepoint_generated.ml"
out="$build_dir/poll_statepoint_generated.exe"
ir="$build_dir/poll_statepoint_generated.ll"

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

cat > "$src" <<'EOF'
external poll : unit -> unit = "%poll"

let[@inline never] run n =
  poll ();
  n + 1

let () = Printf.printf "%d\n" (run 41)
EOF

"$ocamlopt" -g -O3 -S -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$out" "$src"
"$out" > "$build_dir/poll_statepoint_stdout.txt"
grep -q "^42$" "$build_dir/poll_statepoint_stdout.txt"
grep -q 'caml_call_gc.*"statepoint-id"="1"' "$ir"
