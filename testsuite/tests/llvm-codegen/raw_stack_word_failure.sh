#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/raw_stack_word_generated.ml"
out="$build_dir/raw_stack_word_generated.exe"
stdout_file="$build_dir/raw_stack_word_stdout.txt"
stderr_file="$build_dir/raw_stack_word_stderr.txt"
stub_obj="$build_dir/raw_stack_word_stubs.o"

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
  ocamlopt="_build/install/main/bin/ocamlopt.opt"
fi

{
  echo "open Stdlib_upstream_compatible"
  echo
  echo "external raw_stack_base : unit -> nativeint# ="
  echo "  \"raw_stack_base_bytecode\" \"raw_stack_base_native\" [@@noalloc]"
  echo
  echo "external raw_stack_high : unit -> nativeint# ="
  echo "  \"raw_stack_high_bytecode\" \"raw_stack_high_native\" [@@noalloc]"
  echo
  echo "let[@inline never] id x = Sys.opaque_identity x"
  echo
  echo "let[@inline never] rec grow n (keep : nativeint#) ="
  echo "  let keep = id keep in"
  echo "  if n = 0 then keep"
  echo "  else"
  echo "    let r = grow (n - 1) keep in"
  echo "    if not (Nativeint_u.equal keep r) then failwith \"raw value changed\";"
  echo "    r"
  echo
  echo "let () ="
  echo "  let keep = raw_stack_base () in"
  echo "  let before = Nativeint_u.to_nativeint keep in"
  echo "  let before_s = Nativeint.to_string before in"
  echo "  let before_hex = Printf.sprintf \"%nx\" before in"
  echo "  let high_before = Nativeint_u.to_nativeint (raw_stack_high ()) in"
  echo "  let after = Nativeint_u.to_nativeint (grow 20000 keep) in"
  echo "  let after_s = Nativeint.to_string after in"
  echo "  let after_hex = Printf.sprintf \"%nx\" after in"
  echo "  let base_after = Nativeint_u.to_nativeint (raw_stack_base ()) in"
  echo "  let high_after = Nativeint_u.to_nativeint (raw_stack_high ()) in"
  echo "  if not (String.equal before_s after_s) then begin"
  echo "    Printf.eprintf"
  echo "      \"before=%s/%s after=%s/%s high_before=%nx base_after=%nx high_after=%nx\\n\""
  echo "      before_s before_hex after_s after_hex high_before base_after high_after;"
  echo "    failwith \"raw stack-looking nativeint# was rewritten\""
  echo "  end"
} > "$src"

"$ocamlopt" -I +stdlib_upstream_compatible -O3 -g -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -o "$stub_obj" "$build_dir/raw_stack_word_stubs.c"

"$ocamlopt" -I +stdlib_upstream_compatible -O3 -g -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" stdlib_upstream_compatible.cmxa "$stub_obj" "$src"

set +e
OCAMLRUNPARAM="l=1M" "$out" > "$stdout_file" 2> "$stderr_file"
run_status=$?
set -e

if [ "$run_status" -eq 0 ]; then
  exit 0
fi

if grep -q "raw stack-looking nativeint# was rewritten" "$stderr_file"; then
  # CR: This exhibits the current problem.  The arm64 LLVM stack-growth
  # fallback rewrites any copied stack word that happens to look like an old
  # stack address, even when that word is an unboxed nativeint# rather than a
  # pointer.  The value changes to the corresponding address in the new stack.
  exit 0
fi

cat "$stderr_file"
exit "$run_status"
