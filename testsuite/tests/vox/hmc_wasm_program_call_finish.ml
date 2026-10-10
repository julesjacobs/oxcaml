module B = Wasm_u32
module T = Wasm_control
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module C = Wasm_code
module Fuel = Wasm_control_compose
module Empty = Wasm_empty_labels
module Emit = Hmc_wasm_program_emit
module Status = Hmc_wasm_program_status
let (correct @ total) : (local : B.u32) -> (failure : B.u32) -> (body : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (before : X.state) @ immutable -> (prepared : X.state) @ immutable -> (after_body : X.state) @ immutable ->
    (inner : T.labels) @ immutable -> (body_fuel : C.count) @ immutable -> (cleanup : C.count) @ immutable ->
    {u : unit | T.run (Status.four ()) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = body; labels = Status.scope local outer; state = prepared}
      && L.get prepared.X.machine.E.locals local === Some (S.I32 failure)
      && T.run body_fuel {T.code = body; labels = Status.scope local outer; state = prepared}
        === T.Running {T.code = T.Empty; labels = inner; state = after_body}
      && Empty.related cleanup (Status.scope local outer) inner && after_body.X.machine.E.stack === S.Empty} ->
    {after : X.state | after.X.memory === after_body.X.memory && after.X.machine.E.stack === S.Empty
      && L.replaced after_body.X.machine.E.locals local (S.I32 (Status.zero ())) after.X.machine.E.locals
      && L.get after.X.machine.E.locals local === Some (S.I32 (Status.zero ()))
      && T.run (Status.cost (Fuel.add body_fuel cleanup)) {T.code = Emit.protected local failure body; labels = outer; state = before}
        === T.Running {T.code = T.Empty; labels = outer; state = after}} @ immutable =
  fun local failure body outer before prepared after_body inner body_fuel cleanup premise ->
    ghost_ (L.can_set_def prepared.X.machine.E.locals local (S.I32 (Status.zero ()));
      S.same_type_def (S.I32 failure) (S.I32 (Status.zero ()));
      Wasm_control_local_type.run body_fuel {T.code = body; labels = Status.scope local outer; state = prepared}
        {T.code = T.Empty; labels = inner; state = after_body} local (S.I32 (Status.zero ())) ();
      Empty.correct cleanup (Status.scope local outer) inner after_body ();
      Fuel.correct body_fuel cleanup {T.code = body; labels = Status.scope local outer; state = prepared});
    Status.normal local failure body outer before prepared after_body (Fuel.add body_fuel cleanup) ()
