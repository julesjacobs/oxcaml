module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
let[@def] (labels @ total) (tail : T.code @ immutable) (outer : T.labels @ immutable) =
  T.Label ({T.restart = None; continuation = tail; saved = S.Empty}, outer)
let[@def] (cost @ total) (code : C.t @ immutable) = Fuel.add (C.length code) (C.Succ C.Zero)
let (correct @ total) : (code : C.t) @ immutable -> (tail : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | X.run code before === X.Done after && after.X.machine.E.stack === S.Empty} ->
    {u : unit | T.run (cost code) {T.code = Lift.embed code T.Empty; labels = labels tail outer; state = before}
      === T.Running {T.code = tail; labels = outer; state = after}} @ ghost =
  fun code tail outer before after premise -> ghost_ (
    Wasm_control_success.straight code before after ();
    Lift.correct code T.Empty (labels tail outer) before after ();
    cost_def code;
    Fuel.correct (C.length code) (C.Succ C.Zero) {T.code = Lift.embed code T.Empty; labels = labels tail outer; state = before};
    let returning = {T.code = T.Empty; labels = labels tail outer; state = after} in
    T.run_def (C.Succ C.Zero) returning; T.step_def returning; labels_def tail outer;
    T.stack_def after S.Empty;
    T.run_def C.Zero {T.code = tail; labels = outer; state = after})
