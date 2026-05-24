#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/stack_check_attr_generated.ml"
out="$build_dir/stack_check_attr_generated.exe"
ir="$build_dir/stack_check_attr_generated.ll"
asm="$build_dir/stack_check_attr_generated.s"

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
let[@inline never] alloc_pair a b = a, b

let[@inline never] call_closure f x = f x + 1

let () =
  let x, y = alloc_pair 1 2 in
  Printf.printf "%d\n" (x + y + call_closure (( + ) 1) 40)
EOF

"$ocamlopt" -O3 -S -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$out" "$src"

"$out" > "$build_dir/stack_check_attr_stdout.txt"
grep -q "^45$" "$build_dir/stack_check_attr_stdout.txt"

alloc_pair_ir=$(
  grep 'define .*__alloc_pair_.*_code' "$ir" || true
)
case "$alloc_pair_ir" in
  *oxcaml-stack-check*)
    echo "allocation-only function unexpectedly has oxcaml-stack-check" >&2
    exit 1
    ;;
  "")
    echo "missing alloc_pair function in LLVM IR" >&2
    exit 1
    ;;
esac

call_closure_ir=$(
  grep 'define .*__call_closure_.*_code' "$ir" || true
)
case "$call_closure_ir" in
  *oxcaml-stack-check*) ;;
  "")
    echo "missing call_closure function in LLVM IR" >&2
    exit 1
    ;;
  *)
    echo "non-tail-call function is missing oxcaml-stack-check" >&2
    exit 1
    ;;
esac

awk '
  /__alloc_pair_.*_code:/ { in_alloc_pair = 1; next }
  in_alloc_pair && /-- End function/ { in_alloc_pair = 0; next }
  in_alloc_pair && /caml_llvm_prologue_realloc_stack|_caml_plat_pagesize/ {
    found = 1
  }
  END { exit found ? 1 : 0 }
' "$asm"

awk '
  /__call_closure_.*_code:/ { in_call_closure = 1; next }
  in_call_closure && /-- End function/ { in_call_closure = 0; next }
  in_call_closure && /caml_llvm_prologue_realloc_stack/ { found = 1 }
  END { exit found ? 0 : 1 }
' "$asm"
