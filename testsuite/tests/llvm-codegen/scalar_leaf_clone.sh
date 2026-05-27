#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/scalar_leaf_clone_generated.ml"
out="$build_dir/scalar_leaf_clone_generated.o"
ir="$build_dir/scalar_leaf_clone_generated.ll"
asm="$build_dir/scalar_leaf_clone_generated.s"

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
type t = Attr of int | Prim of int | Other of int

let[@inline never] eval = function
  | Attr x -> if x = 17 then 1 else 2
  | Prim x -> if x = 42 then 3 else 4
  | Other i -> i land 7

let[@inline never] run xs n =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc + eval (Array.unsafe_get xs (i mod 5))
  done;
  !acc

let () =
  let xs = [| Attr 17; Attr 19; Prim 42; Prim 43; Other 17 |] in
  print_int (run xs 10)
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

if ! grep -aEq 'define private +i64 .*llvm_leaf_scalar.*\(ptr addrspace\(1\)' "$ir"; then
  echo "expected a private scalar leaf clone with the default LLVM calling convention" >&2
  exit 1
fi

if ! grep -aEq 'call +i64 .*llvm_leaf_scalar.*\(ptr addrspace\(1\)' "$ir"; then
  echo "expected the same-unit direct call to use the scalar leaf clone" >&2
  exit 1
fi

if ! grep -aq 'define  oxcaml_nofpcc .*eval_' "$ir"; then
  echo "expected the normal OxCaml ABI entry point to remain available" >&2
  exit 1
fi

if ! grep -aEq '[[:space:]]bl[[:space:]]+_camlScalar_leaf_clone_generated__eval_.*llvm_leaf_scalar' "$asm"; then
  echo "expected assembly to call the scalar leaf clone directly" >&2
  exit 1
fi
