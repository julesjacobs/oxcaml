#!/bin/sh

set -eu

build_dir=$(pwd)
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/allocation_frametable_generated.ml"
out="$build_dir/allocation_frametable_generated.exe"
asm="$build_dir/allocation_frametable_generated.s"
stdout_file="$build_dir/allocation_frametable_stdout.txt"
debug_flags="-g"
extra_link_flags=""
check_debug_locations=true

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64)
    debug_flags=""
    extra_link_flags="-ccopt -no-pie"
    check_debug_locations=false
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
external opaque : 'a -> 'a = "%opaque"

let[@inline never] f a b c =
  let x = opaque (a, b) in
  let y = opaque (c, a, b) in
  opaque (x, y)

let () =
  ignore (f 1 2 3);
  print_endline "ok"
EOF

"$ocamlopt" $stdlib_flags -O3 $debug_flags -S -llvm-backend \
  $extra_link_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

"$out" > "$stdout_file"
grep -q "^ok$" "$stdout_file"

# The allocation slow path for [f] records the three allocation sizes as
# alloc_words - 2, matching the native frame table format.  The LLVM backend
# runs comballoc, so these adjacent allocations should be encoded as one
# count-3 record.
awk '
  BEGIN {
    combined_len = 4
    combined[1] = 3; combined[2] = 1; combined[3] = 2; combined[4] = 1
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
    if (combined_i == combined_len) {
      found = 1
      exit
    }
  }

  END { exit found ? 0 : 1 }
' "$asm"

# With [-g], the same frametable record should also carry source-location
# strings for allocation debug metadata.
if [ "$check_debug_locations" = true ]; then
  grep -q "allocation_frametable_generated.ml" "$asm"
fi
