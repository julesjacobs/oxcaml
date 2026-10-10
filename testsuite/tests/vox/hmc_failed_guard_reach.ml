module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module L = Wasm_locals
module M = Hmc_failed_guard_model
module Recognize = Hmc_failed_guard_proof
module Select = Hmc_wasm_allocation_select
module Fuel = Wasm_control_compose
module Emit = Hmc_wasm_program_emit
module Status = Hmc_wasm_program_status

type witness = {prefix : C.count; before : T.configuration}
let[@def] (reaches @ total) (resource : M.resource @ immutable) (start : T.configuration @ immutable)
    (witness : witness @ immutable) = ghost_ (
  T.run witness.prefix start === T.Running witness.before && M.failed resource witness.before)

let (prepend @ total) : (resource : M.resource) @ immutable -> (first : C.count) @ immutable ->
    (start : T.configuration) @ immutable -> (middle : T.configuration) @ immutable ->
    (witness : witness) @ immutable ->
    {u : unit | T.run first start === T.Running middle && reaches resource middle witness} ->
    {out : witness | reaches resource start out} @ immutable ghost = fun resource first start middle witness premise -> ghost_ (
  reaches_def resource middle witness; Fuel.correct first witness.prefix start;
  let out = {prefix = Fuel.add first witness.prefix; before = witness.before} in
  reaches_def resource start out; out)

let (protected_guard @ total) : (resource : M.resource) @ immutable -> (bytes : B.u32) ->
    (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable ->
    (outer : T.labels) @ immutable -> (success : T.code) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals (M.cursor_local resource) === Some (S.I32 cursor)
      && L.get state.X.machine.E.locals (M.limit_local resource) === Some (S.I32 limit)
      && L.can_set state.X.machine.E.locals (M.status_local ()) (S.I32 (M.status resource))
      && cursor + bytes > limit} ->
    {out : witness | reaches resource
      {T.code = Emit.protected (M.status_local ()) (M.status resource)
        (Select.emit bytes (M.cursor_local resource) (M.limit_local resource)
          success (T.Instruction (I.Br (M.failure_depth ()), T.Empty)) tail);
        labels = outer; state} out} @ immutable ghost =
  fun resource bytes cursor limit state outer success tail premise -> ghost_ (
    let body = Select.emit bytes (M.cursor_local resource) (M.limit_local resource)
      success (T.Instruction (I.Br (M.failure_depth ()), T.Empty)) tail in
    let prepared = Status.prepare (M.status_local ()) (M.status resource) body outer state () in
    M.cursor_local_def resource; M.limit_local_def resource; M.status_local_def ();
    L.other_local state.X.machine.E.locals (M.status_local ()) (S.I32 (M.status resource))
      prepared.X.machine.E.locals (M.cursor_local resource) ();
    L.other_local state.X.machine.E.locals (M.status_local ()) (S.I32 (M.status resource))
      prepared.X.machine.E.locals (M.limit_local resource) ();
    Recognize.recognize resource bytes cursor limit prepared (Status.scope (M.status_local ()) outer) success tail ();
    let out = {prefix = Status.four (); before = {T.code = body; labels = Status.scope (M.status_local ()) outer; state = prepared}} in
    reaches_def resource {T.code = Emit.protected (M.status_local ()) (M.status resource) body; labels = outer; state} out;
    out)
