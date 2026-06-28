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
# count-3 record. With [-g], the frame-size word must also carry flags = 3,
# meaning the allocation record has source-location debug metadata.
awk '
  BEGIN {
    combined_len = 4
    combined[1] = 3; combined[2] = 1; combined[3] = 2; combined[4] = 1
  }

  function parse_value() {
    value = $2
    sub(/[^0-9-].*/, "", value)
    return value + 0
  }

  function reset_match() {
    state = ""
    combined_i = 0
    debug_longs = 0
    saw_payload_align = 0
  }

  /__frametable:/ { in_frametable = 1; next }
  in_frametable && /^[[:space:]]*\.section[[:space:]]/ { in_frametable = 0 }

  in_frametable && state != "alloc_debug_labels" && /^[[:space:]]*\.long[[:space:]]/ {
    state = "frame_size"
    frame_flags = -1
    live_remaining = 0
    combined_i = 0
    debug_longs = 0
    saw_payload_align = 0
    next
  }

  in_frametable && state == "frame_size" && /^[[:space:]]*\.short[[:space:]]/ {
    frame_flags = parse_value() % 4
    state = "live_count"
    next
  }

  in_frametable && state == "live_count" && /^[[:space:]]*\.short[[:space:]]/ {
    live_remaining = parse_value()
    state = live_remaining == 0 ? "alloc_count" : "live_offsets"
    next
  }

  in_frametable && state == "live_offsets" && /^[[:space:]]*\.short[[:space:]]/ {
    live_remaining--
    if (live_remaining == 0) state = "alloc_count"
    next
  }

  in_frametable && state == "alloc_count" && /^[[:space:]]*\.byte[[:space:]]/ {
    combined_i = parse_value() == combined[1] ? 1 : 0
    state = combined_i == 1 ? "alloc_sizes" : ""
    next
  }

  in_frametable && state == "alloc_sizes" && /^[[:space:]]*\.byte[[:space:]]/ {
    if (parse_value() == combined[combined_i + 1]) {
      combined_i++
    } else {
      reset_match()
    }
    if (combined_i == combined_len && frame_flags == 3) {
      state = "alloc_debug_labels"
      debug_longs = 0
      saw_payload_align = 0
      next
    } else if (combined_i == combined_len) {
      reset_match()
    }
    next
  }

  in_frametable && state ~ /^(frame_size|live_count|live_offsets|alloc_count|alloc_sizes)$/ && /^[[:space:]]*$/ { next }

  in_frametable && state ~ /^(frame_size|live_count|live_offsets|alloc_count|alloc_sizes)$/ {
    reset_match()
    next
  }

  in_frametable && state == "alloc_debug_labels" && /^[[:space:]]*$/ { next }

  in_frametable && state == "alloc_debug_labels" && /^[[:space:]]*\.?L[^:]*:/ { next }

  in_frametable && state == "alloc_debug_labels" && /^[[:space:]]*\.p2align[[:space:]]+2([,[:space:]]|$)/ {
    saw_payload_align = 1
    next
  }

  in_frametable && state == "alloc_debug_labels" && /^[[:space:]]*\.long[[:space:]]/ {
    if (!saw_payload_align || $2 !~ /^\.?L[^[:space:],]*-\.?L/) {
      reset_match()
      next
    }
    debug_longs++
    saw_payload_align = 0
    if (debug_longs == combined[1]) {
      found = 1
      exit
    }
    next
  }

  in_frametable && state == "alloc_debug_labels" {
    reset_match()
    next
  }

  END { exit found ? 0 : 1 }
' "$asm"
