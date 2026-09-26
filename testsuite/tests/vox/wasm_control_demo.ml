(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_memory.ml wasm_memory_execution.ml wasm_memory_lowering.ml wasm_control.ml wasm_nesting.ml wasm_control_lift.ml wasm_control_codec.ml wasm_control_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module N = Wasm_nesting
module Lift = Wasm_control_lift
let rec fuel n rest = if n = 0 then rest else fuel (n - 1) (C.Succ rest)
let rec linear instructions tail = match instructions with
  | [] -> tail | instruction :: rest -> T.Instruction (instruction, linear rest tail)
let state = {X.memory = B.End; machine = {E.locals = S.Push (S.I32 0, S.Empty); stack = S.Empty}}
let check code =
  if N.structured code then (
    ghost_ (N.flatten_correct code C.Empty N.Empty (); Wasm_control_codec.roundtrip code ());
    let suffix = B.Byte (255, B.End) in
    let bytes = Wasm_control_codec.encode code suffix () in
    (match C.decode (C.length (T.flatten code C.Empty)) bytes with
    | Some (tokens, rest) ->
      if rest <> suffix || Wasm_control_codec.decode tokens <> Some code then failwith "control binary roundtrip"
    | None -> failwith "control binary decode");
    if N.scan (T.flatten code C.Empty) N.Empty <> Some N.Empty then failwith "flattened nesting";
    match T.run (fuel 1000 C.Zero) {T.code; labels = T.No_labels; state} with
    | T.Finished state -> if state.X.machine.E.stack <> S.Push (S.I32 42, S.Empty) then failwith "control result"
    | _ -> failwith "control failed")
  else failwith "structured fixture"
let () =
  let get = T.Instruction (I.Local_get 0, T.Empty) in
  let loop = linear [I.Local_get 0; I.I32_const 42; I.Plain I.I32_ge_u; I.Br_if 1;
    I.Local_get 0; I.I32_const 1; I.Plain I.I32_add; I.Local_set 0; I.Br 0] T.Empty in
  check (T.Block (T.Loop (loop, T.Empty), get));
  check (T.Instruction (I.I32_const 42, T.Block (linear [I.I32_const 99; I.Br 0] T.Empty, T.Empty)));
  check (T.Instruction (I.I32_const 0, T.If (linear [I.I32_const 99; I.Local_set 0] T.Empty,
    linear [I.I32_const 42; I.Local_set 0] T.Empty, get)));
  if N.structured (T.Instruction (I.Plain I.End, T.Empty)) then failwith "raw delimiter";
  if N.scan (C.Next (I.Plain I.Else, C.Empty)) N.Empty <> None then failwith "unmatched else";
  if N.scan (C.Next (I.Block, C.Next (I.Plain I.Else, C.Empty))) N.Empty <> None then failwith "else in block";
  if Wasm_control_codec.decode (C.Next (I.Block, C.Empty)) <> None then failwith "unclosed block";
  if Wasm_control_codec.decode (C.Next (I.Plain I.End, C.Empty)) <> None then failwith "extra end";
  if Wasm_control_codec.decode (C.Next (I.If, C.Next (I.Plain I.Else, C.Next (I.Plain I.Else, C.Empty)))) <> None then failwith "duplicate else";
  let code = C.Next (I.I32_const 42, C.Next (I.Local_set 0, C.Empty)) in
  if Lift.straight code then (match X.run code state with
  | X.Done after ->
    ghost_ (Lift.correct code get T.No_labels state after ());
    if T.run (C.length code) {T.code = Lift.embed code get; labels = T.No_labels; state}
       <> T.Running {T.code = get; labels = T.No_labels; state = after} then failwith "straight-line lift"
  | _ -> failwith "straight-line model") else failwith "straight-line classification";
  let endless = T.Loop (T.Instruction (I.Br 0, T.Empty), T.Empty) in
  (match T.run (fuel 64 C.Zero) {T.code = endless; labels = T.No_labels; state} with
  | T.Running {T.labels = T.Label (_, T.No_labels); _} -> ()
  | _ -> failwith "loop fuel or label growth");
  print_endline "Wasm control: loop and branch semantics, stack restoration, if, nesting, bounded execution, and straight-line lifting passed"
