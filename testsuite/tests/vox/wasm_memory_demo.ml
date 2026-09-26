(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_memory.ml wasm_memory_execution.ml wasm_memory_lowering.ml wasm_memory_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Lower = Wasm_memory_lowering
let rec zeros n bytes = if n = 0 then bytes else zeros (n - 1) (B.Byte (0, bytes))
let word = {Hmc_word64.lo = 305419896; hi = 2882400001}
let () =
  let memory = zeros 16 B.End in
  let value = S.I64 word in
  let rest = S.Push (value, S.Push (S.I64 {Hmc_word64.lo = 0; hi = 0}, S.Empty)) in
  let locals = S.Push (S.I32 1, rest) in
  let before = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  ghost_ (L.get_def locals 0; L.get_def locals 1; L.get_def rest 0; M.width_def value);
  if M.can_store memory 1 2 value then (
    let store = Lower.store M.W64 2 0 1 (ghost_ before) (ghost_ 1) (ghost_ value) () in
    match X.run store before with
    | X.Done stored ->
      if L.can_set locals 2 value then (
        let load = Lower.load M.W64 2 0 2 (ghost_ stored) (ghost_ 1) (ghost_ value) () in
        ghost_ (X.append_correct store load before);
        (match X.run (E.append store load) before with
        | X.Done after ->
          if L.get after.X.machine.E.locals 2 <> Some value || after.X.machine.E.stack <> before.X.machine.E.stack then
            failwith "memory lowering result";
          let prefix = Hmc_linear_bytes.drop after.X.memory 3 in
          (match prefix with
          | Some (B.Byte (120, B.Byte (86, B.Byte (52, B.Byte (18,
              B.Byte (1, B.Byte (239, B.Byte (205, B.Byte (171, _))))))))) -> ()
          | _ -> failwith "little endian layout")
        | _ -> failwith "composed memory lowering"))
      else failwith "destination type"
    | _ -> failwith "store lowering")
  else failwith "unexpected bounds failure";
  if M.store memory 9 0 value <> None then failwith "overrun";
  if M.store memory 1 4294967295 value <> None then failwith "wrapped address";
  (match M.store memory 8 0 value with
  | Some after -> if M.load after 8 0 M.W64 <> Some value then failwith "exact bound"
  | None -> failwith "exact bound rejected");
  let state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 9, S.Empty)}} in
  if X.step (Wasm_instruction.I64_load (0, 0)) state <> X.Trap then failwith "load trap";
  let wrong = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 1, S.Push (S.I32 0, S.Empty))}} in
  if X.step (Wasm_instruction.I64_store (3, 0)) wrong <> X.Type_error then failwith "store operand type";
  print_endline "Wasm memory: byte-model lowering, little endian layout, unaligned access, exact bounds, overflow, traps, and composition passed"
