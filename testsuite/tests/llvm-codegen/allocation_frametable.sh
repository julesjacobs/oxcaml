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

# The allocation slow path for [f] records the three allocation sizes as
# alloc_words - 2, matching the native frame table format.  Comballoc-capable
# compilers may encode them as one count-3 record; LLVM backend builds that
# deliberately disable comballoc encode the same sizes as separate count-1
# records.
awk '
  BEGIN {
    combined_len = 4
    combined[1] = 3; combined[2] = 1; combined[3] = 2; combined[4] = 1
    separate_len = 6
    separate[1] = 1; separate[2] = 1
    separate[3] = 1; separate[4] = 2
    separate[5] = 1; separate[6] = 1
  }

  function advance(value, pattern, idx) {
    if (value == pattern[idx + 1]) return idx + 1
    if (value == pattern[1]) return 1
    return 0
  }

  /__frametable:/ { in_frametable = 1; next }
  in_frametable && /^[[:space:]]*\.section[[:space:]]/ { in_frametable = 0 }

  in_frametable && /^[[:space:]]*\.byte[[:space:]]/ {
    value = $2
    sub(/[^0-9-].*/, "", value)
    combined_i = advance(value, combined, combined_i)
    separate_i = advance(value, separate, separate_i)
    if (combined_i == combined_len || separate_i == separate_len) {
      found = 1
      exit
    }
  }

  END { exit found ? 0 : 1 }
' "$asm"

# With [-g], the same frametable record should also carry source-location
# strings for allocation debug metadata.
grep -q "allocation_frametable_generated.ml" "$asm"
