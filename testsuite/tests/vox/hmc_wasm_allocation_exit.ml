module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_allocation_select
module Continue = Wasm_control_branch_continue
let[@def] (escape @ total) (depth : B.u32) = T.Instruction (I.Br depth, T.Empty)
let[@def] (emit @ total) (bytes : B.u32) (cursor_local : B.u32) (limit_local : B.u32)
    (success : C.t @ immutable) (depth : B.u32) (tail : T.code @ immutable) =
  Select.emit bytes cursor_local limit_local (Lift.embed success T.Empty) (escape depth) tail
let[@def] (cost @ total) (bytes : B.u32) (cursor_local : B.u32) (limit_local : B.u32)
    (success : C.t @ immutable) (cursor : B.u32) (limit : B.u32) =
  Fuel.add (Select.cost bytes cursor_local limit_local)
    (if cursor + bytes <= limit then Continue.cost success else C.Succ C.Zero)
let (correct @ total) : (bytes : B.u32) -> (cursor_local : B.u32) -> (limit_local : B.u32) ->
    (success : C.t) @ immutable -> (depth : B.u32) -> (tail : T.code) @ immutable -> (outer : T.labels) @ immutable ->
    (cursor : B.u32) -> (limit : B.u32) -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | before.X.machine.E.stack === S.Empty
      && L.get before.X.machine.E.locals cursor_local === Some (S.I32 cursor)
      && L.get before.X.machine.E.locals limit_local === Some (S.I32 limit)
      && (if cursor + bytes <= limit then X.run success before === X.Done after && after.X.machine.E.stack === S.Empty else true)} ->
    {u : unit | T.run (cost bytes cursor_local limit_local success cursor limit)
        {T.code = emit bytes cursor_local limit_local success depth tail; labels = outer; state = before}
      === (if cursor + bytes <= limit then T.Running {T.code = tail; labels = outer; state = after}
           else T.branch depth (Continue.labels tail outer) before)} @ ghost =
  fun bytes cursor_local limit_local success depth tail outer cursor limit before after premise -> ghost_ (
    emit_def bytes cursor_local limit_local success depth tail;
    cost_def bytes cursor_local limit_local success cursor limit;
    Select.correct bytes cursor_local limit_local cursor limit before outer (Lift.embed success T.Empty) (escape depth) tail ();
    Continue.labels_def tail outer;
    let start = {T.code = emit bytes cursor_local limit_local success depth tail; labels = outer; state = before} in
    if cursor + bytes <= limit then (
      Continue.correct success tail outer before after ();
      Fuel.correct (Select.cost bytes cursor_local limit_local) (Continue.cost success) start)
    else (
      let selected = {T.code = escape depth; labels = Continue.labels tail outer; state = before} in
      escape_def depth; T.run_def (C.Succ C.Zero) selected; T.step_def selected;
      (match T.branch depth (Continue.labels tail outer) before with
      | T.Running next -> T.run_def C.Zero next | _ -> ());
      Fuel.correct (Select.cost bytes cursor_local limit_local) (C.Succ C.Zero) start))
