#!/bin/sh

set -eu

build_dir=$(pwd)
native_dir="$build_dir/native_dwarf_debug_info"
llvm_dir="$build_dir/llvm_dwarf_debug_info"
mkdir -p "$native_dir" "$llvm_dir"
native_src="$native_dir/dwarf_debug_info_generated.ml"
llvm_src="$llvm_dir/dwarf_debug_info_generated.ml"
native_out="$native_dir/dwarf_debug_info_generated.exe"
llvm_out="$llvm_dir/dwarf_debug_info_generated.exe"
native_asm="$native_dir/dwarf_debug_info_generated.s"
llvm_asm="$llvm_dir/dwarf_debug_info_generated.s"

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

cat > "$native_src" <<'EOF'
let[@inline never] f x = x + 1
let () = Printf.printf "%d\n" (f 41)
EOF
cp "$native_src" "$llvm_src"

(
  cd "$native_dir"
  unset OCAMLPARAM
  "$ocamlopt" -g -O3 -S -o "$native_out" "$native_src"
)
"$native_out" > "$build_dir/dwarf_debug_info_native_stdout.txt"
grep -q "^42$" "$build_dir/dwarf_debug_info_native_stdout.txt"
grep -Eq "\\.loc|\\.file" "$native_asm"

(
  cd "$llvm_dir"
  "$ocamlopt" -g -O3 -S -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$llvm_out" "$llvm_src"
)
"$llvm_out" > "$build_dir/dwarf_debug_info_llvm_stdout.txt"
grep -q "^42$" "$build_dir/dwarf_debug_info_llvm_stdout.txt"

grep -Eq "\\.loc|\\.file" "$llvm_asm"
grep -Eq "\\.debug_|DW_TAG|DW_AT" "$llvm_asm"

# CR: This checks that -g -llvm-backend emits standard DWARF debug info for
# source-level debugger support, in addition to OCaml frame-table metadata.
