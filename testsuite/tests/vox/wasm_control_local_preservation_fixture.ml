module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module I = Wasm_instruction
module C = Wasm_code
module L = Wasm_locals
module Preserve = Wasm_control_local_preservation
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let fixture (iterations : B.u32) (value : W.t @ immutable) =
  let body = T.Instruction (I.Local_get 0,
    T.Instruction (I.I32_const 1, T.Instruction (I.Plain I.I32_sub,
      T.Instruction (I.Local_set 0, T.Instruction (I.Local_get 0,
        T.Instruction (I.Br_if 1, T.Empty)))))) in
  let code = T.Loop (T.Block (body, T.Empty), T.Empty) in
  let state = {X.memory = B.End; machine = {E.locals = S.Push (S.I32 iterations, S.Push (S.I64 value, S.Empty)); stack = S.Empty}} in
  let initial = {T.code; labels = T.No_labels; state} in
  if not (Preserve.code code 1) then failwith "loop unexpectedly writes preserved local" else
  let steps = fuel (7 * iterations + 3) in
  match T.run steps initial with
  | T.Running final ->
    ghost_ (Preserve.labels_def T.No_labels 1; Preserve.run steps initial final 1 ());
    if final.T.code <> T.Empty || final.T.labels <> T.No_labels
      || L.get final.T.state.X.machine.E.locals 0 <> Some (S.I32 0)
      || L.get final.T.state.X.machine.E.locals 1 <> Some (S.I64 value) then failwith "loop preservation result"
  | _ -> failwith "loop preservation execution"
let fixtures () =
  List.iter (fun n -> List.iter (fixture n) [{W.lo = 0; hi = 0}; {W.lo = 4294967295; hi = 4294967295}]) [1; 3];
  let write = T.Instruction (I.Local_set 1, T.Empty) in
  if Preserve.code (T.If (T.Empty, write, T.Empty)) 1 then failwith "branch write accepted";
  if Preserve.labels (T.Label ({T.restart = Some write; continuation = T.Empty; saved = S.Empty}, T.No_labels)) 1
    then failwith "restart write accepted";
  if Preserve.labels (T.Label ({T.restart = None; continuation = write; saved = S.Empty}, T.No_labels)) 1
    then failwith "continuation write accepted"
