module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Guard = Wasm_local_probe
module Stack = Hmc_memory_stack
module Q = Hmc_heap_state
module G = Hmc_cfg_ir
let[@def] (emit @ total) (stack_base : B.u32) (top_local : B.u32)
    (root : T.code @ immutable) (caller : T.code @ immutable) (tail : T.code @ immutable) =
  Lift.embed (Guard.emit stack_base top_local) (T.If (root, caller, tail))
let[@def] (cost @ total) (stack_base : B.u32) (top_local : B.u32) =
  Fuel.add (C.length (Guard.emit stack_base top_local)) (C.Succ C.Zero)
let (correct @ total) : (blocks : G.table) @ immutable -> (frames : Q.frames) @ immutable -> (width : B.u32) -> (stack_base : B.u32) -> (top_local : B.u32) ->
    (top : B.u32) -> (state : X.state) @ immutable -> (labels : T.labels) @ immutable ->
    (root : T.code) @ immutable -> (caller : T.code) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && width > 0 && Stack.related blocks width state.X.memory stack_base top frames
      && Wasm_locals.get state.X.machine.E.locals top_local === Some (S.I32 top)} ->
    {u : unit | (match frames with Q.Halt -> top = stack_base | Q.Frame _ -> top > stack_base)
      && T.run (cost stack_base top_local) {T.code = emit stack_base top_local root caller tail; labels; state}
      === T.Running {T.code = (if top = stack_base then root else caller);
        labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state}} @ ghost =
  fun blocks frames width stack_base top_local top state labels root caller tail premise -> ghost_ (
    let code = Guard.emit stack_base top_local in
    let branch = T.If (root, caller, tail) in
    let condition = S.boolean (top = stack_base) in
    let tested = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 condition, S.Empty)}} in
    let start = {T.code = emit stack_base top_local root caller tail; labels; state} in
    let pending = {T.code = branch; labels; state = tested} in
    Stack.related_def blocks width state.X.memory stack_base top frames; Stack.previous_def width top;
    Guard.correct stack_base top top_local state ();
    Wasm_control_success.straight code state tested ();
    emit_def stack_base top_local root caller tail; cost_def stack_base top_local;
    Lift.correct code branch labels state tested ();
    Fuel.correct (C.length code) (C.Succ C.Zero) start;
    T.run_def (C.Succ C.Zero) pending; T.step_def pending; S.boolean_def (top = stack_base);
    T.stack_def tested S.Empty;
    T.enter_def (if condition <> 0 then root else caller) tail None {T.code = branch; labels; state};
    T.stack_def state S.Empty;
    T.run_def C.Zero {T.code = (if top = stack_base then root else caller);
      labels = T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, labels); state})
