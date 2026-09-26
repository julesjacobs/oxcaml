(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_memory.ml wasm_memory_execution.ml wasm_memory_lowering.ml wasm_control.ml wasm_nesting.ml wasm_control_lift.ml wasm_control_codec.ml wasm_globals.ml wasm_global_execution.ml wasm_instance_control.ml wasm_global_entry.ml wasm_global_lowering.ml wasm_functions.ml wasm_calls.ml wasm_function_type.ml wasm_call_budget.ml wasm_calls_demo.ml";
 { bytecode; }
 { native; }
*)
module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module T = Wasm_control
module F = Wasm_functions
module M = Wasm_calls
module Budget = Wasm_call_budget
let rec fuel n tail = if n = 0 then tail else fuel (n - 1) (C.Succ tail)
let function_ code = {F.result = F.I32; locals = F.Local32 F.No_locals; code}
let leaf = function_ (T.Instruction (I.I32_const 42, T.Empty))
let globals = {Wasm_globals.values = S.Empty; permissions = Wasm_globals.Empty}
let module_ = {F.functions = F.Function (function_ (T.Instruction (I.Call 1, T.Empty)),
    F.Function (function_ (T.Instruction (I.Call 2, T.Empty)), F.Function (leaf, F.No_functions)));
  signatures = F.Signature (F.I32, F.No_signatures); table = F.Element (Some 2, F.Element (None, F.No_elements))}
let start (capacity : C.count @ immutable) :
    {out : M.configuration | Budget.budget out.M.callers out.M.capacity capacity} @ immutable =
  ghost_ (Budget.start module_ 0 B.End globals capacity;
    Budget.preserves_def (M.start module_ 0 B.End globals capacity) capacity);
  match M.start module_ 0 B.End globals capacity with
  | M.Running initial -> initial
  | _ -> failwith "start"
let () =
  let capacity = C.Succ (C.Succ C.Zero) in
  let initial = start capacity in
  ghost_ (Budget.budget_def M.Root capacity capacity);
  let run n =
    let steps = fuel n C.Zero in
    ghost_ (Budget.run steps module_ initial capacity ());
    M.run steps module_ initial in
  (match run 2 with
  | M.Running {M.capacity = C.Zero; callers = M.Caller (_, M.Caller (_, M.Root)); _} -> ()
  | _ -> failwith "nested capacity");
  (match run 4 with
  | M.Running {M.capacity = C.Succ C.Zero; callers = M.Caller (_, M.Root); _} -> ()
  | _ -> failwith "returned capacity");
  (match run 100 with
  | M.Finished state -> if state.GE.execution.X.machine.E.stack <> S.Push (S.I32 42, S.Empty) then failwith "returned word"
  | _ -> failwith "nested execution");
  if M.run (fuel 100 C.Zero) module_ (start (C.Succ C.Zero)) <> M.Host_limit then failwith "host capacity";
  let indirect = {initial with M.current = {initial.M.current with Wasm_instance_control.body =
    {initial.M.current.Wasm_instance_control.body with T.code = T.Instruction (I.I32_const 1,
      T.Instruction (I.Call_indirect 0, T.Empty))}}} in
  if M.run (fuel 100 C.Zero) module_ indirect <> M.Trap then failwith "null indirect call";
  List.iter (fun result ->
    let tail = B.Byte (255, B.End) in
    if Wasm_function_type.decode (Wasm_function_type.encode result tail) <> Some (result, tail) then
      failwith "signature codec") [F.Void; F.I32; F.I64];
  let parameters = B.Byte (96, B.encode_u32 1 (B.Byte (127, B.encode_u32 0 B.End))) in
  if Wasm_function_type.decode parameters <> None then failwith "parameters outside dispatcher ABI";
  print_endline "Wasm calls: nested execution, conserved capacity, explicit host limit, indirect traps, and function-type codec passed"
