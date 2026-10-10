module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Guard = Hmc_wasm_allocation_guard
let[@def] (emit @ total) (bytes : B.u32) (cursor_local : B.u32) (limit_local : B.u32)
    (success : T.code @ immutable) (exhausted : T.code @ immutable) (tail : T.code @ immutable) =
  Lift.embed (Guard.emit bytes cursor_local limit_local) (T.If (success, exhausted, tail))
let[@def] (cost @ total) (bytes : B.u32) (cursor_local : B.u32) (limit_local : B.u32) =
  Fuel.add (C.length (Guard.emit bytes cursor_local limit_local)) (C.Succ C.Zero)
let (correct @ total) : (bytes : B.u32) -> (cursor_local : B.u32) -> (limit_local : B.u32) ->
    (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable -> (labels : T.labels) @ immutable ->
    (success : T.code) @ immutable -> (exhausted : T.code) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && Wasm_locals.get state.X.machine.E.locals cursor_local === Some (S.I32 cursor)
      && Wasm_locals.get state.X.machine.E.locals limit_local === Some (S.I32 limit)} ->
    {u : unit | T.run (cost bytes cursor_local limit_local) {T.code = emit bytes cursor_local limit_local success exhausted tail; labels; state}
      === T.Running {T.code = (if cursor + bytes <= limit then success else exhausted);
        labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state}} @ ghost =
  fun bytes cursor_local limit_local cursor limit state labels success exhausted tail premise -> ghost_ (
    let code = Guard.emit bytes cursor_local limit_local in
    let branch = T.If (success, exhausted, tail) in
    let condition = S.boolean (cursor + bytes <= limit) in
    let tested = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 condition, S.Empty)}} in
    let start = {T.code = emit bytes cursor_local limit_local success exhausted tail; labels; state} in
    let pending = {T.code = branch; labels; state = tested} in
    Guard.correct bytes cursor_local limit_local cursor limit state ();
    Wasm_control_success.straight code state tested ();
    emit_def bytes cursor_local limit_local success exhausted tail; cost_def bytes cursor_local limit_local;
    Lift.correct code branch labels state tested ();
    Fuel.correct (C.length code) (C.Succ C.Zero) start;
    T.run_def (C.Succ C.Zero) pending; T.step_def pending; S.boolean_def (cursor + bytes <= limit);
    T.stack_def tested S.Empty;
    T.enter_def (if condition <> 0 then success else exhausted) tail None {T.code = branch; labels; state};
    T.stack_def state S.Empty;
    T.run_def C.Zero {T.code = (if cursor + bytes <= limit then success else exhausted);
      labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state})
