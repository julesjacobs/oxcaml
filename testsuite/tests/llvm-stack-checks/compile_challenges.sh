#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/deep_compile_generated.ml"
exe="$build_dir/deep_compile_generated.exe"
out="$build_dir/deep_compile_generated.out"
boundary_out="$build_dir/deep_compile_boundary.out"
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

{
  echo 'external opaque : int -> int = "%opaque"'
  echo 'let[@inline never] leaf x = opaque (x + 1)'
  printf 'let result = '
  i=0
  while [ "$i" -lt 1600 ]; do
    printf 'let x%d = leaf %d in ' "$i" "$i"
    i=$((i + 1))
  done
  echo 'x1599'
  echo 'let () = Printf.printf "%d\n" result'
} > "$src"

OCAMLRUNPARAM=b,l=1000000 \
  "$ocamlopt" -O3 -g -llvm-backend \
  $extra_ocamlopt_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$src"

"$exe" > "$out"

cat > "$build_dir/deep_compile_generated.expected" <<'EOF'
1600
EOF

diff -u "$build_dir/deep_compile_generated.expected" "$out"

set +e
OCAMLRUNPARAM=b=1,l=16384 \
  "$ocamlopt" -O3 -g -llvm-backend \
  $extra_ocamlopt_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe.too_small" "$src" > "$boundary_out" 2>&1
status=$?
set -e

if [ "$status" -eq 0 ]; then
  echo "compiler too-small-stack challenge unexpectedly succeeded" >&2
  exit 1
fi

if [ "$status" -ge 128 ]; then
  cat "$boundary_out" >&2
  echo "compiler too-small-stack challenge got signal status $status" >&2
  exit 1
fi

if ! grep -Eq 'Stack[ _]overflow' "$boundary_out"; then
  cat "$boundary_out" >&2
  echo "compiler too-small-stack challenge failed without Stack_overflow" >&2
  exit 1
fi

i=1
while [ "$i" -le 5 ]; do
  set +e
  OCAMLRUNPARAM=b=1,l=32768 \
    "$ocamlopt" -O3 -g -llvm-backend \
    $extra_ocamlopt_flags \
    -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
    -o "$exe.boundary" "$src" > "$boundary_out" 2>&1
  status=$?
  set -e

  if [ "$status" -ge 128 ]; then
    cat "$boundary_out" >&2
    echo "compiler boundary challenge got signal status $status" >&2
    exit 1
  fi

  if [ "$status" -ne 0 ] && ! grep -Eq 'Stack[ _]overflow' "$boundary_out"; then
    cat "$boundary_out" >&2
    echo "compiler boundary challenge failed without Stack_overflow" >&2
    exit 1
  fi

  i=$((i + 1))
done
