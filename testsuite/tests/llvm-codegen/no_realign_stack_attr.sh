#!/bin/sh

set -eu

build_dir=$(pwd)
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/no_realign_stack_attr_generated.ml"
out="$build_dir/no_realign_stack_attr_generated.exe"
ir="$build_dir/no_realign_stack_attr_generated.ll"
extra_link_flags=""

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64) extra_link_flags="-ccopt -no-pie" ;;
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

stdlib_flags=""
ocamlopt_dir=$(dirname "$ocamlopt")
for stdlib_dir in \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_flags="-I $stdlib_dir"
    break
  fi
done

cat > "$src" <<'EOF'
let[@inline never] f x = Sys.opaque_identity x + 1

let () = Printf.printf "%d\n" (f 41)
EOF

"$ocamlopt" $stdlib_flags -O3 -S -keep-llvmir -llvm-backend \
  $extra_link_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$out" "$src"
"$out" > "$build_dir/no_realign_stack_attr_stdout.txt"
grep -q "^42$" "$build_dir/no_realign_stack_attr_stdout.txt"
grep -q '"no-realign-stack"' "$ir"
