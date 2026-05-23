#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/allocation_frametable_generated.ml"
out="$build_dir/allocation_frametable_generated.exe"
asm="$build_dir/allocation_frametable_generated.s"
stdout_file="$build_dir/allocation_frametable_stdout.txt"

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

cat > "$src" <<'EOF'
external opaque : 'a -> 'a = "%opaque"

let[@inline never] f a b c =
  let x = opaque (a, b) in
  let y = opaque (c, a, b) in
  opaque (x, y)

let () =
  ignore (f 1 2 3);
  print_endline "ok"
EOF

"$ocamlopt" -O3 -g -S -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

"$out" > "$stdout_file"
grep -q "^ok$" "$stdout_file"

# The allocation slow path for [f] combines three allocation items.  In the
# frametable this is encoded as allocation count 3, then one byte per
# allocation size.  The bytes are alloc_words - 2, matching the native frame
# table format.
awk '
  /__frametable:/ { in_frametable = 1; next }
  in_frametable && /\.byte[[:space:]]+3[[:space:]]*$/ { state = 1; next }
  state == 1 && /\.byte[[:space:]]+1[[:space:]]*$/ { state = 2; next }
  state == 2 && /\.byte[[:space:]]+2[[:space:]]*$/ { state = 3; next }
  state == 3 && /\.byte[[:space:]]+1[[:space:]]*$/ { found = 1; exit }
  END { exit found ? 0 : 1 }
' "$asm"

# With [-g], the same frametable record should also carry source-location
# strings for allocation debug metadata.
grep -q "allocation_frametable_generated.ml" "$asm"
