#!/bin/sh

set -eu

mode="${1:-stack-checks}"
build_dir=$(pwd)
src="$build_dir/stack_check_size_contract_generated.ml"
out="$build_dir/stack_check_size_contract_generated.o"
ir="$build_dir/stack_check_size_contract_generated.ll"
asm="$build_dir/stack_check_size_contract_generated.s"
cfg_dump="$build_dir/stack_check_size_contract_generated.cmx.dump"
boundary_ir="$build_dir/stack_check_size_contract_boundary.ll"
boundary_asm="$build_dir/stack_check_size_contract_boundary.s"
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

let[@inline never] small_leaf x = opaque (x + 1)

let[@inline never] callee x = opaque (x + 1)

let[@inline never] small_non_tail_call x = callee x + 1

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

stack_check_before_bytes() {
  name="$1"
  function_attr_line "$name" \
    | sed -n 's/.*"oxcaml-stack-check-before-bytes"="\([0-9][0-9]*\)".*/\1/p'
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

cfg_stack_check_before_bytes() {
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
      sub(/.*before=/, "", bytes)
      sub(/[^0-9].*/, "", bytes)
      if (bytes + 0 > max) max = bytes + 0
    }
    END { print max + 0 }
  ' "$cfg_dump"
}

function_asm() {
  name="$1"
  awk -v name="camlStack_check_size_contract_generated__${name}_" '
    index($0, name) != 0 { in_function = 1 }
    in_function { print }
    in_function && /^[[:space:]]*\.cfi_endproc/ { in_function = 0 }
  ' "$asm"
}

has_prologue_realloc() {
  function_asm "$1" | grep -q '_caml_call_realloc_stack'
}

has_ordinary_realloc() {
  function_asm "$1" | grep -q '_caml_llvm_call_realloc_stack'
}

assert_has_prologue_realloc() {
  if ! has_prologue_realloc "$1"; then
    echo "$1: expected LLVM prologue stack check" >&2
    exit 1
  fi
}

assert_no_prologue_realloc() {
  if has_prologue_realloc "$1"; then
    echo "$1: unexpected LLVM prologue stack check" >&2
    exit 1
  fi
}

assert_has_ordinary_realloc() {
  if ! has_ordinary_realloc "$1"; then
    echo "$1: expected ordinary LLVM stack check" >&2
    exit 1
  fi
}

assert_no_ordinary_realloc() {
  if has_ordinary_realloc "$1"; then
    echo "$1: unexpected ordinary LLVM stack check" >&2
    exit 1
  fi
}

boundary_function_asm() {
  name="$1"
  awk -v name="_${name}:" '
    index($0, name) != 0 { in_function = 1 }
    in_function { print }
    in_function && /^[[:space:]]*\.cfi_endproc/ { in_function = 0 }
  ' "$boundary_asm"
}

boundary_has_prologue_realloc() {
  boundary_function_asm "$1" | grep -q '_caml_call_realloc_stack'
}

check_zero_byte_prologue_boundary() {
  # Isolate the LLVM prologue heuristic around the 256-byte stack threshold.
  # These oxcaml_fpcc functions also save FP/LR, so the alloca sizes below
  # produce 112-, 128-, and 272-byte prologue prefixes respectively.
  # A 112-byte prefix leaves the helper reserve. 128 and 272 do not: the
  # self-stage compiler crash reproducer hit this exact boundary, so equality is
  # kept conservative.
  cat > "$boundary_ir" <<'EOF'
target triple = "arm64-apple-macosx"

define oxcaml_fpcc i64 @zero_byte_safe(i64 %ds, i64 %alloc, i64 %x) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" {
entry:
  %buf = alloca [96 x i8], align 16
  call void asm sideeffect "", "r"(ptr %buf)
  ret i64 %x
}

define oxcaml_fpcc i64 @zero_byte_boundary_needs_prologue(i64 %ds, i64 %alloc, i64 %x) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" {
entry:
  %buf = alloca [112 x i8], align 16
  call void asm sideeffect "", "r"(ptr %buf)
  ret i64 %x
}

define oxcaml_fpcc i64 @zero_byte_needs_prologue(i64 %ds, i64 %alloc, i64 %x) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" {
entry:
  %buf = alloca [256 x i8], align 16
  call void asm sideeffect "", "r"(ptr %buf)
  ret i64 %x
}
EOF

  "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -target arm64-apple-macosx \
    -Wno-override-module -x ir -S -O3 -o "$boundary_asm" "$boundary_ir"

  if boundary_has_prologue_realloc zero_byte_safe; then
    echo "zero-byte stack-check contract should allow a 112-byte frame without a prologue check" >&2
    exit 1
  fi
  if ! boundary_has_prologue_realloc zero_byte_boundary_needs_prologue; then
    echo "zero-byte stack-check contract should keep prologue checks for a 128-byte frame" >&2
    exit 1
  fi
  if ! boundary_has_prologue_realloc zero_byte_needs_prologue; then
    echo "zero-byte stack-check contract should keep prologue checks for a 256-byte frame" >&2
    exit 1
  fi

  sed 's/"oxcaml-stack-check-bytes"="0"/"oxcaml-stack-check-bytes"="16"/g' \
    "$boundary_ir" > "$boundary_ir.nonzero"
  sed 's/"oxcaml-stack-check-bytes"="16"/"oxcaml-stack-check-bytes"="16" "oxcaml-stack-check-before-bytes"="0"/g' \
    "$boundary_ir.nonzero" > "$boundary_ir.nonzero.before"
  mv "$boundary_ir.nonzero.before" "$boundary_ir.nonzero"
  mv "$boundary_ir.nonzero" "$boundary_ir"
  "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -target arm64-apple-macosx \
    -Wno-override-module -x ir -S -O3 -o "$boundary_asm" "$boundary_ir"

  # A nonzero byte-count attribute is a producer contract: OxCaml has inserted
  # an ordinary CFG stack check and LLVM must not add a duplicate prologue
  # check. The hand-written IR here checks the LLVM-side contract handling; the
  # generated-code checks below verify that real OxCaml IR also contains the
  # ordinary CFG check.
  if boundary_has_prologue_realloc zero_byte_safe; then
    echo "nonzero stack-check contract should rely on the CFG check, not add a prologue check for a 112-byte frame" >&2
    exit 1
  fi
  if boundary_has_prologue_realloc zero_byte_boundary_needs_prologue; then
    echo "nonzero stack-check contract should rely on the CFG check, not add a prologue check for a 128-byte frame" >&2
    exit 1
  fi
  if boundary_has_prologue_realloc zero_byte_needs_prologue; then
    echo "nonzero stack-check contract should rely on the CFG check, not add a prologue check for a 256-byte frame" >&2
    exit 1
  fi

  sed 's/"oxcaml-stack-check-before-bytes"="0"/"oxcaml-stack-check-before-bytes"="32"/g' \
    "$boundary_ir" > "$boundary_ir.before64"
  mv "$boundary_ir.before64" "$boundary_ir"
  "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -target arm64-apple-macosx \
    -Wno-override-module -x ir -S -O3 -o "$boundary_asm" "$boundary_ir"

  if boundary_has_prologue_realloc zero_byte_safe; then
    echo "nonzero stack-check contract should still rely on the CFG check when pre-check stack use spends the reserve" >&2
    exit 1
  fi
}

check_contract_matches_cfg() {
  name="$1"
  cfg_bytes=$(cfg_stack_check_bytes "$name")
  ir_bytes=$(stack_check_bytes "$name")
  if [ "$ir_bytes" != "$cfg_bytes" ]; then
    echo "$name stack-check bytes: CFG has $cfg_bytes, LLVM IR has ${ir_bytes:-absent}" >&2
    exit 1
  fi
  ir_before_bytes=$(stack_check_before_bytes "$name")
  if [ "$cfg_bytes" != "0" ] && [ -z "$ir_before_bytes" ]; then
    echo "$name missing OxCaml stack-check-before byte attribute" >&2
    exit 1
  fi
  cfg_before_bytes=$(cfg_stack_check_before_bytes "$name")
  if [ -n "$ir_before_bytes" ] && [ "$ir_before_bytes" != "$cfg_before_bytes" ]; then
    echo "$name stack-check-before bytes: CFG has $cfg_before_bytes, LLVM IR has $ir_before_bytes" >&2
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
  if grep -q '_caml_call_realloc_stack' "$asm"; then
    echo "unexpected OxCaml stack-check prologue in no-stack-checks build" >&2
    exit 1
  fi
  if grep -q '_caml_llvm_call_realloc_stack' "$asm"; then
    echo "unexpected ordinary LLVM stack check in no-stack-checks build" >&2
    exit 1
  fi
  exit 0
fi

if [ "$mode" = "no-cfg-stack-checks" ]; then
  if grep -q '^\*\*\* After cfg_stack_checks' "$cfg_dump"; then
    echo "unexpected cfg_stack_checks dump when CFG stack checks are disabled" >&2
    exit 1
  fi
  if grep -q '"oxcaml-stack-check-bytes"' "$ir"; then
    echo "unexpected OxCaml stack-check byte attribute without CFG stack checks" >&2
    exit 1
  fi
  if grep -q '"oxcaml-stack-check-before-bytes"' "$ir"; then
    echo "unexpected OxCaml stack-check-before byte attribute without CFG stack checks" >&2
    exit 1
  fi
  for name in small_leaf small_non_tail_call noalloc_outgoing_stack_args; do
    if ! has_stack_check_request "$name"; then
      echo "$name missing legacy OxCaml stack-check request attribute" >&2
      exit 1
    fi
  done
  if [ "$(cfg_stack_check_count small_non_tail_call)" != "0" ]; then
    echo "small_non_tail_call should have no CFG stack_check instructions" >&2
    exit 1
  fi
  assert_has_prologue_realloc small_non_tail_call
  assert_has_prologue_realloc noalloc_outgoing_stack_args
  assert_no_ordinary_realloc small_non_tail_call
  assert_no_ordinary_realloc noalloc_outgoing_stack_args
  exit 0
fi

check_zero_byte_prologue_boundary

check_contract_matches_cfg small_leaf
if [ "$(cfg_stack_check_bytes small_leaf)" != "0" ]; then
  echo "small_leaf should have CFG stack-check bytes 0" >&2
  exit 1
fi
if [ "$(cfg_stack_check_count small_leaf)" != "0" ]; then
  echo "small_leaf should have no CFG stack_check instructions" >&2
  exit 1
fi
assert_no_prologue_realloc small_leaf
assert_no_ordinary_realloc small_leaf

check_contract_matches_cfg small_non_tail_call
if [ "$(cfg_stack_check_bytes small_non_tail_call)" = "0" ]; then
  echo "small_non_tail_call should have nonzero CFG stack-check bytes" >&2
  exit 1
fi
assert_no_prologue_realloc small_non_tail_call
assert_has_ordinary_realloc small_non_tail_call

check_contract_matches_cfg noalloc_outgoing_stack_args
if [ "$(cfg_stack_check_bytes noalloc_outgoing_stack_args)" = "0" ]; then
  echo "noalloc_outgoing_stack_args should have nonzero CFG stack-check bytes" >&2
  exit 1
fi
assert_no_prologue_realloc noalloc_outgoing_stack_args
assert_has_ordinary_realloc noalloc_outgoing_stack_args
