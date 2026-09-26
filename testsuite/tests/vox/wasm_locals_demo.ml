(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_word_lowering.ml wasm_locals.ml wasm_execution.ml wasm_local_lowering.ml wasm_locals_demo.ml";
 { bytecode; }
*)
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module O = Wasm_word_lowering
let word (n : W.limb) = {W.lo = n; hi = 0}
let check (destination : Wasm_u32.u32) =
  let a = word 50 in let b = word 8 in
  let rest = S.Push (S.I64 b, S.Push (S.I64 (word 99), S.Empty)) in
  let locals = S.Push (S.I64 a, rest) in
  let state = {E.locals; stack = S.Push (S.I32 91, S.Empty)} in
  let value = O.evaluate O.Subtract a b in
  ghost_ (L.get_def locals 0; L.get_def locals 1; L.get_def rest 0);
  if L.can_set locals destination value then (
    let code = Wasm_local_lowering.lower O.Subtract 0 1 destination
      (ghost_ state) (ghost_ a) (ghost_ b) () in
    match E.run code state with
    | E.Done after ->
      if after.E.stack <> state.E.stack || L.get after.E.locals destination <> Some (S.I64 (word 42)) then
        failwith "local arithmetic";
      if destination <> 0 then (
        ghost_ (L.other_local locals destination value after.E.locals 0 ());
        if L.get after.E.locals 0 <> Some (S.I64 a) then failwith "unrelated local");
      let tail = C.Next (I.Local_get destination, C.Empty) in
      ghost_ (E.append_correct code tail state);
      (match E.run (E.append code tail) state with
      | E.Done out -> if out.E.stack <> S.Push (S.I64 (word 42), state.E.stack) then failwith "continuation"
      | _ -> failwith "continuation failed")
    | _ -> failwith "lowering failed")
  else failwith "invalid destination"
let () =
  check 0; check 1; check 2;
  let state = {E.locals = S.Push (S.I32 0, S.Empty); stack = S.Push (S.I32 42, S.Empty)} in
  if E.step (I.Local_get 1) state <> E.Type_error then failwith "local bounds";
  let wrong = {E.locals = state.E.locals; stack = S.Push (S.I64 (word 42), S.Empty)} in
  if E.step (I.Local_set 0) wrong <> E.Type_error then failwith "local type";
  (match E.step (I.Local_tee 0) state with
  | E.Done after -> if after.E.stack <> state.E.stack || L.get after.E.locals 0 <> Some (S.I32 42) then failwith "tee"
  | _ -> failwith "tee failed");
  print_endline "Wasm locals: aliased destinations, operand order, untouched locals, continuations, tee, and invalid accesses passed"
