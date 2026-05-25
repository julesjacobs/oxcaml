#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/challenges_generated.ml"
exe="$build_dir/challenges_generated.exe"
out="$build_dir/challenges_generated.out"
overflow_src="$build_dir/overflow_generated.ml"
overflow_exe="$build_dir/overflow_generated.exe"
overflow_out="$build_dir/overflow_generated.out"
ir="$build_dir/challenges_generated.ll"
asm="$build_dir/challenges_generated.s"
cfg_dump="$build_dir/challenges_generated.cmx.dump"
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

cat > "$src" <<'EOF'
external opaque : 'a -> 'a = "%opaque"

let depth =
  match Sys.getenv_opt "OCAML_STACK_CHECK_CHALLENGE_DEPTH" with
  | None -> 20_000
  | Some s -> int_of_string s

let prologue_depth = min depth 1_000
let nested_try_finally_depth = min depth 128

let check name ok =
  if ok
  then Printf.printf "%s: ok\n%!" name
  else (Printf.printf "%s: failed\n%!" name; exit 1)

let[@inline never] callee x = opaque (x + 1)

let[@inline never] ordinary_stack_growth depth =
  let rec loop n acc =
    if n = 0
    then acc
    else
      let x0 = opaque (n + acc) in
      let x1 = opaque (callee n) in
      let after = loop (n - 1) (acc + (x0 land 7)) in
      after + (x1 land 3)
  in
  loop depth 0

let[@inline never] prologue_stack_growth depth =
  let rec loop n acc =
    if n = 0
    then acc
    else
      let a00 = opaque (n + 00) in
      let a01 = opaque (n + 01) in
      let a02 = opaque (n + 02) in
      let a03 = opaque (n + 03) in
      let a04 = opaque (n + 04) in
      let a05 = opaque (n + 05) in
      let a06 = opaque (n + 06) in
      let a07 = opaque (n + 07) in
      let a08 = opaque (n + 08) in
      let a09 = opaque (n + 09) in
      let a10 = opaque (n + 10) in
      let a11 = opaque (n + 11) in
      let a12 = opaque (n + 12) in
      let a13 = opaque (n + 13) in
      let a14 = opaque (n + 14) in
      let a15 = opaque (n + 15) in
      let a16 = opaque (n + 16) in
      let a17 = opaque (n + 17) in
      let a18 = opaque (n + 18) in
      let a19 = opaque (n + 19) in
      let a20 = opaque (n + 20) in
      let a21 = opaque (n + 21) in
      let a22 = opaque (n + 22) in
      let a23 = opaque (n + 23) in
      let a24 = opaque (n + 24) in
      let a25 = opaque (n + 25) in
      let a26 = opaque (n + 26) in
      let a27 = opaque (n + 27) in
      let a28 = opaque (n + 28) in
      let a29 = opaque (n + 29) in
      let a30 = opaque (n + 30) in
      let a31 = opaque (n + 31) in
      let after = loop (n - 1) (acc + (a00 land 7)) in
      after
      + ((a01 + a02 + a03 + a04 + a05 + a06 + a07 + a08) land 7)
      + ((a09 + a10 + a11 + a12 + a13 + a14 + a15 + a16) land 7)
      + ((a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24) land 7)
      + ((a25 + a26 + a27 + a28 + a29 + a30 + a31) land 7)
  in
  loop depth 0

exception Hit of int

let[@inline never] trap_stack_growth depth =
  let rec raise_at_bottom n acc =
    if n = 0
    then raise (Hit acc)
    else
      let x = opaque (callee n) in
      let y = raise_at_bottom (n - 1) (acc + (x land 7)) in
      y + 1
  in
  try raise_at_bottom depth 0 with Hit x -> x

let[@inline never] normal_trap_pop_after_stack_growth depth =
  try ordinary_stack_growth depth with Hit _ -> 0

let[@inline never] try_finally_after_stack_growth depth =
  let try_finally ?(always = fun () -> ()) ?(exceptionally = fun () -> ()) work
      =
    match work () with
    | result -> (
      match always () with
      | () -> result
      | exception always_exn ->
        let always_bt = Printexc.get_raw_backtrace () in
        exceptionally ();
        Printexc.raise_with_backtrace always_exn always_bt)
    | exception work_exn -> (
      let work_bt = Printexc.get_raw_backtrace () in
      match always () with
      | () ->
        exceptionally ();
        Printexc.raise_with_backtrace work_exn work_bt
      | exception always_exn ->
        let always_bt = Printexc.get_raw_backtrace () in
        exceptionally ();
        Printexc.raise_with_backtrace always_exn always_bt)
  in
  let cleanup_count = ref 0 in
  try_finally
    ~always:(fun () -> incr cleanup_count)
    ~exceptionally:(fun () -> cleanup_count := !cleanup_count + 10)
    (fun () -> ordinary_stack_growth depth)
  + !cleanup_count

let[@inline never] nested_try_finally_after_stack_growth nesting depth =
  let try_finally ?(always = fun () -> ()) ?(exceptionally = fun () -> ()) work
      =
    match work () with
    | result -> (
      match always () with
      | () -> result
      | exception always_exn ->
        let always_bt = Printexc.get_raw_backtrace () in
        exceptionally ();
        Printexc.raise_with_backtrace always_exn always_bt)
    | exception work_exn -> (
      let work_bt = Printexc.get_raw_backtrace () in
      match always () with
      | () ->
        exceptionally ();
        Printexc.raise_with_backtrace work_exn work_bt
      | exception always_exn ->
        let always_bt = Printexc.get_raw_backtrace () in
        exceptionally ();
        Printexc.raise_with_backtrace always_exn always_bt)
  in
  let cleanup_count = ref 0 in
  let rec loop n =
    if n = 0
    then ordinary_stack_growth depth
    else
      try_finally
        ~always:(fun () -> incr cleanup_count)
        ~exceptionally:(fun () -> cleanup_count := !cleanup_count + 10)
        (fun () -> loop (n - 1))
  in
  loop nesting + !cleanup_count

type _ Effect.t += Ping : int Effect.t

let[@inline never] effect_stack_growth depth =
  let rec perform_at_bottom n acc =
    if n = 0
    then Effect.perform Ping
    else
      let x = opaque (callee n) in
      let y = perform_at_bottom (n - 1) (acc + (x land 7)) in
      y + (acc land 1)
  in
  Effect.Deep.match_with (fun () -> perform_at_bottom depth 0) ()
    { retc = Fun.id;
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Ping ->
            Some
              (fun (k : (a, int) Effect.Deep.continuation) ->
                Effect.Deep.continue k 17)
          | _ -> None)
    }

let[@inline never] live_roots_stack_growth depth =
  let roots = Array.init 64 (fun i -> ref (i + 1)) in
  let rec loop n acc =
    if n = 0
    then acc + Array.fold_left (fun sum r -> sum + !r) 0 roots
    else
      let index = n land 63 in
      let root = roots.(index) in
      let before = !root in
      let after = loop (n - 1) (acc + before) in
      if !root <> before then exit 2;
      after + (before land 1)
  in
  loop depth 0

let () =
  check "ordinary" (ordinary_stack_growth depth > 0);
  check "prologue" (prologue_stack_growth prologue_depth > 0);
  check "trap" (trap_stack_growth depth > 0);
  check "normal-trap-pop" (normal_trap_pop_after_stack_growth depth > 0);
  check "try-finally" (try_finally_after_stack_growth depth > 0);
  check "nested-try-finally"
    (nested_try_finally_after_stack_growth nested_try_finally_depth depth > 0);
  check "effect" (effect_stack_growth depth > 0);
  check "live-roots" (live_roots_stack_growth depth > 0)
EOF

"$ocamlopt" -O3 -g -S -c -keep-llvmir -llvm-backend \
  -dcfg -dump-into-file \
  $extra_ocamlopt_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$build_dir/challenges_generated.o" "$src"

for generated_file in "$ir" "$asm" "$cfg_dump"; do
  if [ ! -f "$generated_file" ]; then
    echo "expected generated file missing: $generated_file" >&2
    exit 1
  fi
done

if ! grep -q 'stack_check size=[1-9]' "$cfg_dump"; then
  echo "expected CFG stack_check instructions in stress program" >&2
  exit 1
fi

if ! grep -q '"oxcaml-stack-check-bytes"="[1-9]' "$ir"; then
  echo "expected nonzero oxcaml-stack-check-bytes attributes in stress program" >&2
  exit 1
fi

if ! grep -q '_caml_llvm_call_realloc_stack' "$asm"; then
  echo "expected ordinary LLVM stack-check slow path in stress program" >&2
  exit 1
fi

if ! grep -q '_caml_call_realloc_stack' "$asm"; then
  echo "expected prologue LLVM stack-check slow path in stress program" >&2
  exit 1
fi

"$ocamlopt" -O3 -g -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$exe" "$src"

runparam="${OCAMLRUNPARAM:-}"
if [ -z "$runparam" ]; then
  runparam="l=100000"
else
  runparam="$runparam,l=100000"
fi
OCAMLRUNPARAM="$runparam" "$exe" > "$out"

cat > "$build_dir/challenges_generated.expected" <<'EOF'
ordinary: ok
prologue: ok
trap: ok
normal-trap-pop: ok
try-finally: ok
nested-try-finally: ok
effect: ok
live-roots: ok
EOF

diff -u "$build_dir/challenges_generated.expected" "$out"

cat > "$overflow_src" <<'EOF'
external opaque : 'a -> 'a = "%opaque"

let[@inline never] callee x = opaque (x + 1)

let[@inline never] stack_overflow () =
  let rec loop n =
    let x = opaque (callee n) in
    1 + loop (x + 1)
  in
  ignore (loop 0)

let () = stack_overflow ()
EOF

"$ocamlopt" -O3 -g -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$overflow_exe" "$overflow_src"

set +e
OCAMLRUNPARAM=l=100000 "$overflow_exe" > "$overflow_out" 2>&1
status=$?
set -e

if [ "$status" -eq 0 ]; then
  echo "stack-overflow challenge unexpectedly succeeded" >&2
  exit 1
fi

if [ "$status" -ge 128 ]; then
  cat "$overflow_out" >&2
  echo "stack-overflow challenge got signal status $status" >&2
  exit 1
fi

if ! grep -Eq 'Stack[ _]overflow' "$overflow_out"; then
  cat "$overflow_out" >&2
  echo "stack-overflow challenge failed without Stack_overflow" >&2
  exit 1
fi
