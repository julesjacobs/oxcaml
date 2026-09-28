module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
let[@def] (labels @ total) (unit : unit) = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels)
let[@def] (cost @ total) (code : C.t @ immutable) = Fuel.add (C.length code) (C.Succ (C.Succ C.Zero))
let (correct @ total) : (code : C.t) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | X.run code before === X.Done after && after.X.machine.E.stack === S.Empty} ->
    {u : unit | T.run (cost code) {T.code = Lift.embed code T.Empty; labels = labels (); state = before} === T.Finished after} @ ghost =
  fun code before after premise -> ghost_ (
    Wasm_control_success.straight code before after ();
    Lift.correct code T.Empty (labels ()) before after ();
    cost_def code;
    let start = {T.code = Lift.embed code T.Empty; labels = labels (); state = before} in
    Fuel.correct (C.length code) (C.Succ (C.Succ C.Zero)) start;
    let returning = {T.code = T.Empty; labels = labels (); state = after} in
    T.run_def (C.Succ (C.Succ C.Zero)) returning; T.step_def returning; labels_def ();
    T.stack_def after S.Empty;
    let final = {T.code = T.Empty; labels = T.No_labels; state = after} in
    T.run_def (C.Succ C.Zero) final; T.step_def final)
