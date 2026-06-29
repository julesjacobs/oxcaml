#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_sqrt_intrinsic_generated.ml"
out="$build_dir/amd64_sqrt_intrinsic_generated.o"
ir="$build_dir/amd64_sqrt_intrinsic_generated.ll"
asm="$build_dir/amd64_sqrt_intrinsic_generated.s"

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
external sqrt64 : float -> float = "caml_sqrt_float" "sqrt"
[@@unboxed] [@@noalloc]

external sqrt32 : float32 -> float32 = "caml_sqrt_float32_bytecode" "sqrtf"
[@@unboxed] [@@noalloc]

let[@inline never] test_sqrt64 x = sqrt64 x

let[@inline never] test_sqrt32 x = sqrt32 x
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

if ! grep -q "llvm.sqrt.f64" "$ir"; then
  echo "float sqrt should lower to llvm.sqrt.f64" >&2
  exit 1
fi

if ! grep -q "llvm.sqrt.f32" "$ir"; then
  echo "float32 sqrt should lower to llvm.sqrt.f32" >&2
  exit 1
fi

if grep -Eq "callq?[[:space:]]+sqrt(@|f@)" "$asm"; then
  echo "sqrt should not lower to a libcall in assembly" >&2
  exit 1
fi

if ! grep -Eq "(^|[[:space:]])v?sqrtsd[[:space:]]" "$asm"; then
  echo "float sqrt should lower to sqrtsd/vsqrtsd" >&2
  exit 1
fi

if ! grep -Eq "(^|[[:space:]])v?sqrtss[[:space:]]" "$asm"; then
  echo "float32 sqrt should lower to sqrtss/vsqrtss" >&2
  exit 1
fi
