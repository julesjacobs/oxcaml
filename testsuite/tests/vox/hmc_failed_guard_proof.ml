module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module L = Wasm_locals
module M = Hmc_failed_guard_model
module Guard = Hmc_wasm_allocation_guard
module Select = Hmc_wasm_allocation_select
module Lift = Wasm_control_lift

let (recognize @ total) : (resource : M.resource) @ immutable -> (bytes : B.u32) ->
    (cursor : B.u32) -> (limit : B.u32) -> (state : X.state) @ immutable ->
    (labels : T.labels) @ immutable -> (success : T.code) @ immutable -> (tail : T.code) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals (M.cursor_local resource) === Some (S.I32 cursor)
      && L.get state.X.machine.E.locals (M.limit_local resource) === Some (S.I32 limit)
      && L.get state.X.machine.E.locals (M.status_local ()) === Some (S.I32 (M.status resource))
      && cursor + bytes > limit} ->
    {u : unit | M.failed resource {T.code = Select.emit bytes (M.cursor_local resource) (M.limit_local resource)
      success (T.Instruction (I.Br (M.failure_depth ()), T.Empty)) tail; labels; state}} @ ghost =
  fun resource bytes cursor limit state labels success tail premise -> ghost_ (
    M.failure_depth_def ();
    Select.emit_def bytes (M.cursor_local resource) (M.limit_local resource) success (T.Instruction (I.Br (M.failure_depth ()), T.Empty)) tail;
    Guard.emit_def bytes (M.cursor_local resource) (M.limit_local resource);
    Hmc_wasm_header_update.number_def bytes;
    let branch = T.If (success, T.Instruction (I.Br (M.failure_depth ()), T.Empty), tail) in
    let c8 = C.Empty in
    let c7 = C.Next (I.Plain I.I32_eqz, c8) in
    let c6 = C.Next (I.Plain I.I64_lt_u, c7) in
    let c5 = C.Next (I.Plain I.I64_add, c6) in
    let c4 = C.Next (I.I64_const (Hmc_wasm_header_update.number bytes), c5) in
    let c3 = C.Next (I.Plain I.I64_extend_i32_u, c4) in
    let c2 = C.Next (I.Local_get (M.cursor_local resource), c3) in
    let c1 = C.Next (I.Plain I.I64_extend_i32_u, c2) in
    let c0 = C.Next (I.Local_get (M.limit_local resource), c1) in
    Lift.embed_def c0 branch;
    Lift.embed_def c1 branch;
    Lift.embed_def c2 branch;
    Lift.embed_def c3 branch;
    Lift.embed_def c4 branch;
    Lift.embed_def c5 branch;
    Lift.embed_def c6 branch;
    Lift.embed_def c7 branch;
    Lift.embed_def c8 branch;
    M.failed_def resource {T.code = Select.emit bytes (M.cursor_local resource) (M.limit_local resource)
      success (T.Instruction (I.Br (M.failure_depth ()), T.Empty)) tail; labels; state})
