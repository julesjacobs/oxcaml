#!/bin/sh

set -eu

mode="${1:-stack-checks}"
build_dir=$(pwd)
src="$build_dir/stack_check_size_contract_generated.ml"
out="$build_dir/stack_check_size_contract_generated.o"
ir="$build_dir/stack_check_size_contract_generated.ll"
asm="$build_dir/stack_check_size_contract_generated.s"
cfg_dump="$build_dir/stack_check_size_contract_generated.cmx.dump"
extra_ocamlopt_flags=""

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

if [ "$mode" = "no-cfg-stack-checks" ]; then
  extra_ocamlopt_flags="-no-cfg-stack-checks"
fi

arg_count=48

{
cat <<'EOF'
external opaque : 'a -> 'a = "%opaque"

let[@inline never] leaf_alloc x = opaque (x, x)

let[@inline never] callee x = opaque (x + 1)

let[@inline never] non_tail_call x = callee x + 1

external noalloc_too_many :
EOF

i=1
while [ "$i" -le "$arg_count" ]; do
  echo "  int ->"
  i=$((i + 1))
done

cat <<'EOF'
  int = "" "noalloc_too_many" [@@noalloc]

let[@inline never] noalloc_outgoing_stack_args x =
EOF
printf "  noalloc_too_many x"
i=1
while [ "$i" -lt "$arg_count" ]; do
  printf " %d" "$i"
  i=$((i + 1))
done
printf "\n"
} > "$src"

"$ocamlopt" -O3 -g -S -c -keep-llvmir -llvm-backend \
  -dcfg -dump-into-file \
  $extra_ocamlopt_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

for generated_file in "$ir" "$asm" "$cfg_dump"; do
  if [ ! -f "$generated_file" ]; then
    echo "expected generated file missing: $generated_file" >&2
    exit 1
  fi
done

function_attr_line() {
  name="$1"
  grep "define .*camlStack_check_size_contract_generated__${name}_.*_code" "$ir"
}

stack_check_bytes() {
  name="$1"
  function_attr_line "$name" \
    | sed -n 's/.*"oxcaml-stack-check-bytes"="\([0-9][0-9]*\)".*/\1/p'
}

has_stack_check_request() {
  name="$1"
  function_attr_line "$name" | grep -q '"oxcaml-stack-check"="true"'
}

cfg_stack_check_bytes() {
  name="$1"
  awk -v name="camlStack_check_size_contract_generated__${name}_" '
    /^\*\*\*/ {
      in_stack_check_dump = $0 ~ /^\*\*\* After cfg_stack_checks/
      in_function = 0
      next
    }
    in_stack_check_dump && /^cfg for / {
      in_function = index($0, name) != 0
      next
    }
    in_function && /stack_check size=[0-9]+/ {
      bytes = $0
      sub(/.*stack_check size=/, "", bytes)
      sub(/[^0-9].*/, "", bytes)
      if (bytes + 0 > max) max = bytes + 0
    }
    END { print max + 0 }
  ' "$cfg_dump"
}

cfg_stack_check_count() {
  name="$1"
  awk -v name="camlStack_check_size_contract_generated__${name}_" '
    /^\*\*\*/ {
      in_stack_check_dump = $0 ~ /^\*\*\* After cfg_stack_checks/
      in_function = 0
      next
    }
    in_stack_check_dump && /^cfg for / {
      in_function = index($0, name) != 0
      next
    }
    in_function && /stack_check size=[0-9]+/ {
      count++
    }
    END { print count + 0 }
  ' "$cfg_dump"
}

check_contract_matches_cfg() {
  name="$1"
  cfg_bytes=$(cfg_stack_check_bytes "$name")
  ir_bytes=$(stack_check_bytes "$name")
  if [ "$ir_bytes" != "$cfg_bytes" ]; then
    echo "$name stack-check bytes: CFG has $cfg_bytes, LLVM IR has ${ir_bytes:-absent}" >&2
    exit 1
  fi
  if ! has_stack_check_request "$name"; then
    echo "$name missing legacy OxCaml stack-check request attribute" >&2
    exit 1
  fi
}

if [ "$mode" = "no-stack-checks" ]; then
  if grep -q '"oxcaml-stack-check' "$ir"; then
    echo "unexpected OxCaml stack-check attribute in no-stack-checks build" >&2
    exit 1
  fi
  if grep -q '_caml_llvm_prologue_realloc_stack' "$asm"; then
    echo "unexpected OxCaml stack-check prologue in no-stack-checks build" >&2
    exit 1
  fi
  exit 0
fi

if [ "$mode" = "no-cfg-stack-checks" ]; then
  if grep -q '"oxcaml-stack-check-bytes"' "$ir"; then
    echo "unexpected OxCaml stack-check byte attribute without CFG stack checks" >&2
    exit 1
  fi
  for name in leaf_alloc non_tail_call noalloc_outgoing_stack_args; do
    if ! has_stack_check_request "$name"; then
      echo "$name missing legacy OxCaml stack-check request attribute" >&2
      exit 1
    fi
  done
  if [ "$(cfg_stack_check_count non_tail_call)" != "0" ]; then
    echo "non_tail_call should have no CFG stack_check instructions" >&2
    exit 1
  fi
  exit 0
fi

check_contract_matches_cfg leaf_alloc
if [ "$(cfg_stack_check_bytes leaf_alloc)" != "0" ]; then
  echo "leaf_alloc should have CFG stack-check bytes 0" >&2
  exit 1
fi
if [ "$(cfg_stack_check_count leaf_alloc)" != "0" ]; then
  echo "leaf_alloc should have no CFG stack_check instructions" >&2
  exit 1
fi

check_contract_matches_cfg non_tail_call
if [ "$(cfg_stack_check_bytes non_tail_call)" = "0" ]; then
  echo "non_tail_call should have nonzero CFG stack-check bytes" >&2
  exit 1
fi

check_contract_matches_cfg noalloc_outgoing_stack_args
if [ "$(cfg_stack_check_bytes noalloc_outgoing_stack_args)" = "0" ]; then
  echo "noalloc_outgoing_stack_args should have nonzero CFG stack-check bytes" >&2
  exit 1
fi
