(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_memory.ml wasm_memory_execution.ml wasm_memory_lowering.ml wasm_control.ml wasm_nesting.ml wasm_control_lift.ml wasm_control_codec.ml wasm_globals.ml wasm_global_execution.ml wasm_instance_control.ml wasm_global_entry.ml wasm_global_lowering.ml wasm_globals_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution
module P = Wasm_instance_control
module Entry = Wasm_global_entry
module Lower = Wasm_global_lowering
module Lift = Wasm_control_lift
let () =
  let full = S.I64 {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let globals = {G.values = S.Push (S.I32 0, S.Push (full, S.Empty));
    permissions = G.Global (true, G.Global (false, G.Empty))} in
  let state = {GE.globals; execution = {X.memory = B.Byte (7, B.End);
    machine = {E.locals = S.Push (S.I32 0, S.Empty); stack = S.Push (S.I32 91, S.Empty)}}} in
  if G.can_set globals 0 (S.I32 42) then (
    let code = Lower.write 0 (S.I32 42) (ghost_ state) () in
    match GE.run code state with
    | GE.Done after ->
      ghost_ (G.other_global globals 0 (S.I32 42) after.GE.globals 1 ());
      if G.get after.GE.globals 1 <> Some full then failwith "untouched global";
      if Lift.straight code then (
        ghost_ (P.straight_line code Wasm_control.Empty Wasm_control.No_labels state after ());
        if P.run (C.length code) {P.globals; body = {Wasm_control.code = Lift.embed code Wasm_control.Empty;
            labels = Wasm_control.No_labels; state = state.GE.execution}}
          <> P.Running {P.globals = after.GE.globals; body = {Wasm_control.code = Wasm_control.Empty;
            labels = Wasm_control.No_labels; state = after.GE.execution}} then failwith "global control lift")
      else failwith "write classification";
      if L.can_set after.GE.execution.X.machine.E.locals 0 (S.I32 42) then (
        let read = Lower.read 0 0 (ghost_ after) (ghost_ (S.I32 42)) () in
        ghost_ (GE.append_correct code read state);
        match GE.run (E.append code read) state with
        | GE.Done final ->
          if L.get final.GE.execution.X.machine.E.locals 0 <> Some (S.I32 42)
            || final.GE.execution.X.machine.E.stack <> state.GE.execution.X.machine.E.stack
            || final.GE.execution.X.memory <> state.GE.execution.X.memory then failwith "global read result"
        | _ -> failwith "read failed")
      else failwith "read destination"
    | _ -> failwith "write failed")
  else failwith "write rejected";
  if G.set globals 1 full <> None then failwith "immutable write";
  if G.set globals 0 full <> None then failwith "wrong type";
  if G.set globals 2 (S.I32 0) <> None then failwith "missing global";
  List.iter (fun entry ->
    let tail = B.Byte (255, B.End) in
    if Entry.decode (Entry.encode entry tail) <> Some (entry, tail) then failwith "global entry roundtrip")
    [{Entry.mutable_ = true; value = S.I32 4294967295}; {Entry.mutable_ = false; value = full}];
  if Entry.decode (B.Byte (127, B.Byte (2, B.End))) <> None then failwith "mutability byte";
  let wrong_type = B.Byte (127, B.Byte (0, I.encode (I.I64_const {Hmc_word64.lo = 0; hi = 0})
    (I.encode (I.Plain I.End) B.End))) in
  if Entry.decode wrong_type <> None then failwith "initializer type";
  print_endline "Wasm globals: permissions, types, untouched entries, read/write lowering, control lifting, and binary initializers passed"
