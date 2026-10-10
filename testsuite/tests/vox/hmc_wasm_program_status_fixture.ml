module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module T = Wasm_control
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Fuel = Wasm_control_compose
let fixture success (failure : B.u32) =
  let condition : B.u32 = if success then 1 else 0 in
  let escape = T.Instruction (I.Br 2, T.Empty) in
  let branch = T.If (T.Empty, escape, T.Empty) in
  let body = T.Instruction (I.I32_const condition, branch) in
  let before = {X.memory = B.Byte (77, B.End); machine = {E.locals = S.Push (S.I32 99, S.Empty); stack = S.Empty}} in
  ghost_ (L.can_set_def before.X.machine.E.locals 0 (S.I32 failure);
    L.get_def before.X.machine.E.locals 0; S.same_type_def (S.I32 99) (S.I32 failure));
  let prepared = Status.prepare 0 failure body T.No_labels before () in
  let labels = Status.scope 0 T.No_labels in
  let guard = {T.restart = None; continuation = T.Empty; saved = S.Empty} in
  let selected = if success then T.Empty else escape in
  let pushed = {prepared with X.machine = {prepared.X.machine with E.stack = S.Push (S.I32 condition, S.Empty)}} in
  let start = {T.code = body; labels; state = prepared} in
  let choosing = {T.code = branch; labels; state = pushed} in
  let entered = {T.code = selected; labels = T.Label (guard, labels); state = prepared} in
  ghost_ (
    Status.two_def (); Status.exit_depth_def ();
    T.run_def (Status.two ()) start; T.step_def start;
    X.step_def (I.I32_const condition) prepared; E.step_def (I.I32_const condition) prepared.X.machine;
    S.step_def (I.I32_const condition) prepared.X.machine.E.stack;
    T.run_def (C.Succ C.Zero) choosing; T.step_def choosing; T.stack_def pushed S.Empty; T.stack_def prepared S.Empty;
    T.enter_def selected T.Empty None {choosing with T.state = T.stack pushed S.Empty};
    T.run_def C.Zero entered);
  let final, fuel = if success then (
    let body_fuel = Fuel.add (Status.two ()) (C.Succ C.Zero) in
    ghost_ (
      T.run_def (C.Succ C.Zero) entered; T.step_def entered; T.stack_def prepared S.Empty;
      T.run_def C.Zero {T.code = T.Empty; labels; state = prepared};
      Fuel.correct (Status.two ()) (C.Succ C.Zero) start;
      L.can_set_def prepared.X.machine.E.locals 0 (S.I32 (Status.zero ()));
      S.same_type_def (S.I32 failure) (S.I32 (Status.zero ())));
    let after = Status.normal 0 failure body T.No_labels before prepared prepared body_fuel () in
    after, Status.cost body_fuel)
  else (
    ghost_ (Status.exhausted 0 failure body T.No_labels before prepared prepared guard (Status.two ()) ());
    T.stack prepared S.Empty, Status.escape_cost (Status.two ())) in
  match T.run fuel {T.code = Emit.protected 0 failure body; labels = T.No_labels; state = before} with
  | T.Running after ->
    if after.T.code <> T.Empty || after.T.labels <> T.No_labels || after.T.state <> final
      || L.get after.T.state.X.machine.E.locals 0 <> Some (S.I32 (if success then 0 else failure))
      || after.T.state.X.memory <> before.X.memory then failwith "proved status scope"
  | _ -> failwith "status scope did not return"
let fixtures () = fixture true 2; fixture false 2; fixture true 3; fixture false 3
