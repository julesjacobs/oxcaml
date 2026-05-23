#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/dwarf_debug_info_generated.ml"
out="$build_dir/dwarf_debug_info_generated.exe"
asm="$build_dir/dwarf_debug_info_generated.s"

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
let[@inline never] f x = x + 1
let () = Printf.printf "%d\n" (f 41)
EOF

"$ocamlopt" -g -O3 -S -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"
"$out" > "$build_dir/dwarf_debug_info_stdout.txt"
grep -q "^42$" "$build_dir/dwarf_debug_info_stdout.txt"

if grep -Eq "\\.debug_|\\.loc|DW_TAG|DW_AT" "$asm"; then
  exit 1
fi

# CR: This records the current gap.  The LLVM backend preserves OCaml backtrace
# metadata in frame tables, but it does not yet emit standard DWARF debug info
# for source-level debugger support.
