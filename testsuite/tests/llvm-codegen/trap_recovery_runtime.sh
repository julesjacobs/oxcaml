#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/trap_recovery_runtime_generated.ml"
out="$build_dir/trap_recovery_runtime_generated.exe"
ir="$build_dir/trap_recovery_runtime_generated.ll"
asm="$build_dir/trap_recovery_runtime_generated.s"

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
exception E of int

let[@inline never] may_raise n =
  if n < 0 then raise_notrace (E (-n)) else n + 1

let[@inline never] catch_once n =
  try may_raise n with E k -> k + 100

let[@inline never] nested_reraise n =
  try
    try may_raise n with
    | E 1 -> raise_notrace (E 10)
    | E k -> k
  with E k -> k + 1000

let () =
  Printf.printf "%d %d %d %d\n"
    (catch_once 3)
    (catch_once (-5))
    (nested_reraise (-1))
    (nested_reraise (-2))
EOF

"$ocamlopt" -O3 -S -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$out" "$src"

"$ocamlopt" -O3 -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$out" "$src"

"$out" > "$build_dir/trap_recovery_runtime_stdout.txt"
grep -q "^4 105 1010 2$" "$build_dir/trap_recovery_runtime_stdout.txt"

grep -q "landingpad token" "$ir"
grep -q "@llvm.aarch64.oxcaml.trap.publish" "$ir"
grep -q "@llvm.aarch64.oxcaml.trap.recover" "$ir"
grep -q "unwind label" "$ir"
if grep -q '@"\\01_wrap_try"' "$ir" || grep -q '^_wrap_try:' "$asm"; then
  echo "AArch64 trap recovery should not emit wrap_try" >&2
  exit 1
fi
if grep -q "llvm.aarch64.oxcaml.trap" "$asm"; then
  echo "trap intrinsics leaked to assembly" >&2
  exit 1
fi

hot_src="$build_dir/trap_recovery_hot_push_pop.ml"
hot_cmx="$build_dir/trap_recovery_hot_push_pop.cmx"
hot_asm="$build_dir/trap_recovery_hot_push_pop.s"

cat > "$hot_src" <<'EOF'
exception E

let[@inline never] f x = x + 1

let[@inline never] run n =
  let acc = ref 0 in
  for i = 1 to n do
    try acc := f !acc with E -> decr acc
  done;
  !acc
EOF

"$ocamlopt" -O3 -S -c -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$hot_cmx" "$hot_src"

if grep -q '\[x28, #48\]' "$hot_asm"; then
  echo "AArch64 hot trap push/pop should keep x26 authoritative" >&2
  exit 1
fi
if grep -q '_wrap_try' "$hot_asm"; then
  echo "AArch64 hot trap push/pop should not emit wrap_try" >&2
  exit 1
fi
