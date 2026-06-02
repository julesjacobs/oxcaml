#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/basic_safepoint_ordinary_trap_roots_generated.ml"
out="$build_dir/basic_safepoint_ordinary_trap_roots_generated.o"
ir="$build_dir/basic_safepoint_ordinary_trap_roots_generated.ll"
cfg_dump="$build_dir/basic_safepoint_ordinary_trap_roots_generated.cmx.dump"

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
  src_root=$(cd "${test_source_directory}/../../.." && pwd)
  if [ -x "$src_root/_install/bin/ocamlopt.opt" ]; then
    ocamlopt="$src_root/_install/bin/ocamlopt.opt"
  elif [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
    ocamlopt="$OCAMLSRCDIR/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

cat > "$src" <<'EOF'
exception E
external poll : unit -> unit = "%poll"

let[@inline never] may_raise b =
  if b then raise_notrace E else 0

let[@inline never] poll_branch payload choose b =
  try
    if choose then (poll (); 0) else may_raise b
  with E -> String.length payload

let[@inline never] alloc_branch payload choose b x =
  try
    if choose then (
      let _ @ global = Sys.opaque_identity (x, x) in
      String.length x)
    else may_raise b
  with E -> String.length payload

let[@inline never] ordinary_call payload b =
  try may_raise b with E -> String.length payload
EOF

"$ocamlopt" -O3 -g -S -c -keep-llvmir -llvm-backend \
  -llvm-frontend-gc-roots -dcfg -dump-into-file \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

for generated_file in "$ir" "$cfg_dump"; do
  if [ ! -f "$generated_file" ]; then
    echo "expected generated file missing: $generated_file" >&2
    exit 1
  fi
done

function_ir() {
  name="$1"
  awk -v name="camlBasic_safepoint_ordinary_trap_roots_generated__${name}_" '
    /^define / { in_function = index($0, name) != 0 }
    in_function { print }
    in_function && /^}/ { in_function = 0 }
  ' "$ir"
}

function_cfg() {
  name="$1"
  awk -v name="camlBasic_safepoint_ordinary_trap_roots_generated__${name}_" '
    /^\*\*\*/ {
      in_stack_check_dump = $0 ~ /^\*\*\* After cfg_stack_checks/
      in_function = 0
      next
    }
    in_stack_check_dump && /^cfg for / {
      in_function = index($0, name) != 0
      next
    }
    in_function { print }
  ' "$cfg_dump"
}

slot_for_arg() {
  name="$1"
  arg="$2"
  function_ir "$name" | awk -v arg="$arg" '
    $0 ~ "store ptr addrspace\\(1\\) %" arg ", ptr %[0-9]+" {
      slot = $NF
      gsub(/,/, "", slot)
      print slot
      exit
    }
  '
}

call_gc_line() {
  name="$1"
  function_ir "$name" | grep '@"\\01_caml_call_gc"'
}

may_raise_line() {
  name="$1"
  function_ir "$name" | grep 'camlBasic_safepoint_ordinary_trap_roots_generated__may_raise_'
}

assert_cfg_shape() {
  name="$1"
  safepoint_pattern="$2"
  if ! function_cfg "$name" | grep -q 'pushtrap handler='; then
    echo "$name: expected an active trap region in CFG" >&2
    exit 1
  fi
  if ! function_cfg "$name" | grep -q "$safepoint_pattern"; then
    echo "$name: expected CFG basic safepoint matching: $safepoint_pattern" >&2
    exit 1
  fi
}

assert_call_gc_is_not_ordinary_invoke() {
  name="$1"
  num_call_gc_lines=$(call_gc_line "$name" | wc -l | tr -d ' ')
  if [ "$num_call_gc_lines" != "1" ]; then
    echo "$name: expected exactly one caml_call_gc in LLVM IR, found $num_call_gc_lines" >&2
    call_gc_line "$name" >&2
    exit 1
  fi

  line=$(call_gc_line "$name")
  case "$line" in
    *"unwind label"*|*"invoke "*)
      echo "$name: caml_call_gc should not unwind to the ordinary trap" >&2
      echo "$line" >&2
      exit 1
      ;;
  esac
}

assert_slot_absent_at_call_gc() {
  name="$1"
  slot="$2"
  description="$3"
  line=$(call_gc_line "$name")
  case "$line" in
    *"$slot"*)
      echo "$name: unexpected $description root $slot at caml_call_gc" >&2
      echo "$line" >&2
      exit 1
      ;;
  esac
}

assert_gc_live_present_at_call_gc() {
  name="$1"
  line=$(call_gc_line "$name")
  case "$line" in
    *'"gc-live"'*) ;;
    *)
      echo "$name: missing normal gc-live roots at caml_call_gc" >&2
      echo "$line" >&2
      exit 1
      ;;
  esac
}

assert_ordinary_call_uses_trap() {
  line=$(may_raise_line ordinary_call)
  case "$line" in
    *"invoke "*"unwind label"*) ;;
    *)
      echo "ordinary_call: may_raise should unwind to the ordinary trap" >&2
      echo "$line" >&2
      exit 1
      ;;
  esac
}

assert_cfg_shape poll_branch '^      [0-9][0-9]* poll$'
assert_call_gc_is_not_ordinary_invoke poll_branch
payload_slot=$(slot_for_arg poll_branch 2)
if [ -z "$payload_slot" ]; then
  echo "poll_branch: could not find payload alloca slot" >&2
  exit 1
fi
assert_slot_absent_at_call_gc poll_branch "$payload_slot" "handler-only payload"

assert_cfg_shape alloc_branch 'alloc 24'
assert_call_gc_is_not_ordinary_invoke alloc_branch
payload_slot=$(slot_for_arg alloc_branch 2)
if [ -z "$payload_slot" ]; then
  echo "alloc_branch: could not find expected alloca slots" >&2
  exit 1
fi
assert_slot_absent_at_call_gc alloc_branch "$payload_slot" "handler-only payload"
assert_gc_live_present_at_call_gc alloc_branch

assert_ordinary_call_uses_trap
