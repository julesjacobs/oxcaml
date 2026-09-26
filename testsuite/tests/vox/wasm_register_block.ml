module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module T = Wasm_control
module P = Wasm_instance_control
module Lift = Wasm_control_lift
module Body = Wasm_instance_body
module Registers = Wasm_global_registers
module Continue = Wasm_control_branch_continue
module Fuel = Wasm_control_compose
let[@def] (epilogue @ total) (stores : Registers.plan @ immutable) (tail : T.code @ immutable) =
  Lift.embed (Registers.store_code stores) tail
let[@def] (emit @ total) (loads : Registers.plan @ immutable) (code : T.code @ immutable)
    (stores : Registers.plan @ immutable) (tail : T.code @ immutable) =
  Lift.embed (Registers.load_code loads) (T.Block (code, epilogue stores tail))
let[@def] (cost @ total) (loads : Registers.plan @ immutable) (body_fuel : C.count @ immutable) (stores : Registers.plan @ immutable) =
  Fuel.add (C.length (Registers.load_code loads)) (C.Succ (Fuel.add body_fuel (C.Succ (C.length (Registers.store_code stores)))))
let (correct @ total) : (loads : Registers.plan) @ immutable -> (code : T.code) @ immutable ->
    (stores : Registers.plan) @ immutable -> (tail : T.code) @ immutable -> (labels : T.labels) @ immutable ->
    (before : GE.state) @ immutable -> (imported : GE.state) @ immutable -> (after : X.state) @ immutable ->
    (exported : GE.state) @ immutable -> (body_fuel : C.count) @ immutable ->
    {u : unit | GE.run (Registers.load_code loads) before === GE.Done imported
      && imported.GE.execution.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && T.run body_fuel {T.code = code; labels = Continue.labels (epilogue stores tail) labels; state = imported.GE.execution} ===
        T.Running {T.code = T.Empty; labels = Continue.labels (epilogue stores tail) labels; state = after}
      && GE.run (Registers.store_code stores) {GE.globals = imported.GE.globals; execution = after} === GE.Done exported} ->
    {u : unit | P.run (cost loads body_fuel stores) {P.body = {T.code = emit loads code stores tail; labels; state = before.GE.execution}; globals = before.GE.globals}
      === P.Running {P.body = {T.code = tail; labels; state = exported.GE.execution}; globals = exported.GE.globals}} @ ghost =
  fun loads code stores tail labels before imported after exported body_fuel premise -> ghost_ (
    let finish = epilogue stores tail in
    let scoped = Continue.labels finish labels in
    let start = {P.body = {T.code = emit loads code stores tail; labels; state = before.GE.execution}; globals = before.GE.globals} in
    let block = {P.body = {T.code = T.Block (code, finish); labels; state = imported.GE.execution}; globals = imported.GE.globals} in
    let body = {P.body = {T.code = code; labels = scoped; state = imported.GE.execution}; globals = imported.GE.globals} in
    let returned = {P.body = {T.code = T.Empty; labels = scoped; state = after}; globals = imported.GE.globals} in
    let store_fuel = C.length (Registers.store_code stores) in
    Registers.load_straight loads; Registers.store_straight stores;
    emit_def loads code stores tail; epilogue_def stores tail; cost_def loads body_fuel stores;
    P.straight_line (Registers.load_code loads) (T.Block (code, finish)) labels before imported ();
    P.step_def block; T.step_def block.P.body; T.enter_def code finish None block.P.body;
    Continue.labels_def finish labels; T.stack_def imported.GE.execution S.Empty;
    Body.run body_fuel body.P.body returned.P.body imported.GE.globals ();
    P.straight_line (Registers.store_code stores) tail labels {GE.globals = imported.GE.globals; execution = after} exported ();
    P.run_def (C.Succ store_fuel) returned; P.step_def returned; T.step_def returned.P.body; T.stack_def after S.Empty;
    Body.compose body_fuel (C.Succ store_fuel) body;
    P.run_def (C.Succ (Fuel.add body_fuel (C.Succ store_fuel))) block;
    Body.compose (C.length (Registers.load_code loads)) (C.Succ (Fuel.add body_fuel (C.Succ store_fuel))) start)
