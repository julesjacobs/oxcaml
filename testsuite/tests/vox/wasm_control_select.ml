module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
let[@def] (emit @ total) (code : C.t @ immutable)
    (success : T.code @ immutable) (exhausted : T.code @ immutable) (tail : T.code @ immutable) =
  Lift.embed (code) (T.If (success, exhausted, tail))
let[@def] (cost @ total) (code : C.t @ immutable) =
  Fuel.add (C.length (code)) (C.Succ C.Zero)
let (correct @ total) : (code : C.t) @ immutable -> (condition : bool) -> (state : X.state) @ immutable -> (labels : T.labels) @ immutable ->
    (success : T.code) @ immutable -> (exhausted : T.code) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && X.run code state === X.Done {X.memory = state.X.memory;
        machine = {E.locals = state.X.machine.E.locals; stack = S.Push (S.I32 (S.boolean condition), S.Empty)}}} ->
    {u : unit | T.run (cost code) {T.code = emit code success exhausted tail; labels; state}
      === T.Running {T.code = (if condition then success else exhausted);
        labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state}} @ ghost =
  fun code condition state labels success exhausted tail premise -> ghost_ (
    let branch = T.If (success, exhausted, tail) in
    let flag = S.boolean condition in
    let tested = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 flag, S.Empty)}} in
    let start = {T.code = emit code success exhausted tail; labels; state} in
    let pending = {T.code = branch; labels; state = tested} in
    Wasm_control_success.straight code state tested ();
    emit_def code success exhausted tail; cost_def code;
    Lift.correct code branch labels state tested ();
    Fuel.correct (C.length code) (C.Succ C.Zero) start;
    T.run_def (C.Succ C.Zero) pending; T.step_def pending; S.boolean_def (condition);
    T.stack_def tested S.Empty;
    T.enter_def (if flag <> 0 then success else exhausted) tail None {T.code = branch; labels; state};
    T.stack_def state S.Empty;
    T.run_def C.Zero {T.code = (if condition then success else exhausted);
      labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state})
