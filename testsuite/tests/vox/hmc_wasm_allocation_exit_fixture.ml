module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
type fixture = {code : C.t; expected : B.u32}
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let fixture (cursor : B.u32) (limit : B.u32) =
  let expected : B.u32 = if cursor + 32 <= limit then 10 else 20 in
  let locals = S.Push (S.I32 cursor, S.Push (S.I32 limit, S.Push (S.I32 20, S.Empty))) in
  let state = {X.memory = B.End; machine = {E.locals; stack = S.Empty}} in
  let trap = T.Instruction (I.Plain I.Unreachable, T.Empty) in
  let done_ = T.Instruction (I.Local_get 2, T.Empty) in
  let normal = T.Instruction (I.I32_const 10, T.Instruction (I.Local_set 2, T.Instruction (I.Br 2, T.Empty))) in
  let guarded = Exit.emit 32 0 1 C.Empty 3 normal in
  let loop_body = T.Block (guarded, trap) in
  let code = T.Block (T.Loop (loop_body, trap), done_) in
  let exit_label = T.Label ({T.restart = None; continuation = done_; saved = S.Empty}, T.No_labels) in
  let loop_label = T.Label ({T.restart = Some loop_body; continuation = trap; saved = S.Empty}, exit_label) in
  let inner_label = T.Label ({T.restart = None; continuation = trap; saved = S.Empty}, loop_label) in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1;
    Wasm_locals.get_def (S.Push (S.I32 limit, S.Push (S.I32 20, S.Empty))) 0;
    X.run_def C.Empty state;
    Exit.correct 32 0 1 C.Empty 3 normal inner_label cursor limit state state ();
    Continue.labels_def normal inner_label;
    T.branch_def 3 (Continue.labels normal inner_label) state;
    T.branch_def 2 inner_label state; T.branch_def 1 loop_label state; T.branch_def 0 exit_label state; T.stack_def state S.Empty);
  (match T.run (fuel 100) {T.code; labels = T.No_labels; state} with
  | T.Finished final ->
    if final.X.memory <> B.End || final.X.machine.E.stack <> S.Push (S.I32 expected, S.Empty)
    then failwith "allocation exit result"
  | _ -> failwith "allocation exit did not finish");
  let prefix = C.Next (I.I32_const cursor, C.Next (I.Local_set 0,
    C.Next (I.I32_const limit, C.Next (I.Local_set 1,
    C.Next (I.I32_const 20, C.Next (I.Local_set 2, C.Empty)))))) in
  {code = E.append prefix (T.flatten code C.Empty); expected}
let fixtures () = [fixture 0 32; fixture 0 31; fixture 100 132; fixture 100 131;
  fixture 4294967263 4294967295; fixture 4294967264 4294967295; fixture 4294967295 4294967295]
