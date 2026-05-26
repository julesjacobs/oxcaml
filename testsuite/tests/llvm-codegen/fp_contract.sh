#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/fp_contract_generated.ml"
out="$build_dir/fp_contract_generated.o"
ir="$build_dir/fp_contract_generated.ll"
asm="$build_dir/fp_contract_generated.s"

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
let[@inline never] muladd x y z = x +. (y *. z)

let[@inline never] mulsub x y z = x -. (y *. z)

let[@inline never] product_minus x y z = (y *. z) -. x

let[@inline never] opaque_blocks x y z =
  x +. Sys.opaque_identity (y *. z)
EOF

"$ocamlopt" -O3 -S -c -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

for generated_file in "$ir" "$asm"; do
  if [ ! -f "$generated_file" ]; then
    echo "expected generated file missing: $generated_file" >&2
    exit 1
  fi
done

function_ir() {
  name="$1"
  awk -v name="camlFp_contract_generated__${name}_" '
    /^define / { in_function = index($0, name) != 0 }
    in_function { print }
    in_function && /^}/ { in_function = 0 }
  ' "$ir"
}

function_asm() {
  name="$1"
  awk -v name="_camlFp_contract_generated__${name}_" '
    /^_/ { in_function = index($0, name) != 0 }
    in_function { print }
  ' "$asm"
}

check_contract_ir() {
  name="$1"
  op="$2"
  if ! function_ir "$name" | grep -q "fmul contract double"; then
    echo "$name should mark the multiply as contractable" >&2
    exit 1
  fi
  if ! function_ir "$name" | grep -q "$op contract double"; then
    echo "$name should mark the $op as contractable" >&2
    exit 1
  fi
}

check_contract_ir muladd fadd
check_contract_ir mulsub fsub
check_contract_ir product_minus fsub

if function_ir opaque_blocks | grep -q "contract double"; then
  echo "Sys.opaque_identity should prevent contractable FP IR" >&2
  exit 1
fi

check_fused_asm() {
  name="$1"
  if ! function_asm "$name" | grep -Eq '(^|[[:space:]])f(n?madd|n?msub)[[:space:]]'; then
    echo "$name should lower to a fused FP multiply-add/sub instruction" >&2
    exit 1
  fi
}

check_fused_asm muladd
check_fused_asm mulsub
check_fused_asm product_minus

if function_asm opaque_blocks | grep -Eq '(^|[[:space:]])f(n?madd|n?msub)[[:space:]]'; then
  echo "Sys.opaque_identity should block fused FP multiply-add/sub assembly" >&2
  exit 1
fi
