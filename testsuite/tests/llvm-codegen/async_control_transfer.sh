#!/bin/sh

set -eu

build_dir=$(pwd)
alloc_src="$build_dir/async_allocation_try.ml"
stack_src="$build_dir/async_stack_overflow_try.ml"

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

cat > "$alloc_src" <<'EOF'
let () = Sys.catch_break true

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () -> finished := true; raise Sys.Break) b;
  ref (Some b)

let () =
  let finished = ref false in
  let r = allocate_bytes finished in
  let result =
    try
      Sys.with_async_exns (fun () ->
        try
          r := None;
          let count = ref 0 in
          while true do
            incr count;
            if !count > 5_000_000 then failwith "finalizer did not run";
            let _ @ global = Sys.opaque_identity (42, !count) in
            ()
          done;
          "no-exn"
        with Sys.Break -> "inner")
    with Sys.Break ->
      if !finished then "outer" else "outer-before-finalizer"
  in
  print_endline result
EOF

cat > "$stack_src" <<'EOF'
let[@inline never] use x = Sys.opaque_identity x

let rec consume n =
  let a0 = use n in
  let a1 = use (n + 1) in
  let a2 = use (n + 2) in
  let a3 = use (n + 3) in
  1 + consume (a0 + a1 + a2 + a3)

let () =
  let result =
    try
      Sys.with_async_exns (fun () ->
        try ignore (consume 0); "no-exn"
        with Stack_overflow -> "inner")
    with Stack_overflow -> "outer"
  in
  print_endline result
EOF

"$ocamlopt" -O3 -g -o "$build_dir/async_allocation_native.exe" "$alloc_src"
"$build_dir/async_allocation_native.exe" > "$build_dir/async_allocation_native.out"
grep -q '^outer$' "$build_dir/async_allocation_native.out"

"$ocamlopt" -O3 -g -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$build_dir/async_allocation_llvm.exe" "$alloc_src"
"$build_dir/async_allocation_llvm.exe" > "$build_dir/async_allocation_llvm.out"
grep -q '^outer$' "$build_dir/async_allocation_llvm.out"

"$ocamlopt" -O3 -g -o "$build_dir/async_stack_native.exe" "$stack_src"
OCAMLRUNPARAM=l=100000 "$build_dir/async_stack_native.exe" \
  > "$build_dir/async_stack_native.out"
grep -q '^outer$' "$build_dir/async_stack_native.out"

"$ocamlopt" -O3 -g -S -keep-llvmir -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$build_dir/async_stack_llvm.exe" "$stack_src"
OCAMLRUNPARAM=l=100000 "$build_dir/async_stack_llvm.exe" \
  > "$build_dir/async_stack_llvm.out"
grep -q '^outer$' "$build_dir/async_stack_llvm.out"

grep -q '@"\\01_caml_llvm_call_realloc_stack"' "$build_dir/async_stack_overflow_try.ll"
grep -q 'bl[[:space:]]*_caml_llvm_call_realloc_stack' "$build_dir/async_stack_overflow_try.s"
