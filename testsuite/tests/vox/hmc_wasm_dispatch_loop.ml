module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module T = Wasm_control
module Fuel = Wasm_control_compose
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Select = Hmc_wasm_dispatch_select
module Execute = Hmc_wasm_dispatch_execute
module Header = Hmc_wasm_header_update
let[@def] (back @ total) (unit : unit) = T.Instruction (I.Br 0, T.Empty)
let[@def] (body @ total) (table : Table.table @ immutable) (base_local : B.u32) = T.Block (Select.emit table base_local, back ())
let[@def] (emit @ total) (table : Table.table @ immutable) (base_local : B.u32) = T.Loop (body table base_local, T.Empty)
let[@def] (labels @ total) (table : Table.table @ immutable) (base_local : B.u32) (outer : T.labels @ immutable) =
  T.Label ({T.restart = Some (body table base_local); continuation = T.Empty; saved = S.Empty}, outer)
let[@def] (configuration @ total) (table : Table.table @ immutable) (base_local : B.u32) (outer : T.labels @ immutable) (state : X.state @ immutable) =
  {T.code = body table base_local; labels = labels table base_local outer; state}
let[@def] (cost @ total) (table : Table.table @ immutable) (pc : W.limb) (base_local : B.u32) (fragment : Block.fragment @ immutable) =
  C.Succ (Fuel.add (Execute.cost table pc base_local fragment) (C.Succ (C.Succ C.Zero)))
let (enter @ total) : (table : Table.table) @ immutable -> (base_local : B.u32) -> (outer : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty} ->
    {u : unit | T.step {T.code = emit table base_local; labels = outer; state} === T.Running (configuration table base_local outer state)} @ ghost =
  fun table base_local outer state premise -> ghost_ (
    emit_def table base_local; T.step_def {T.code = emit table base_local; labels = outer; state};
    T.enter_def (body table base_local) T.Empty (Some (body table base_local)) {T.code = emit table base_local; labels = outer; state};
    T.stack_def state S.Empty; configuration_def table base_local outer state; labels_def table base_local outer)
let (correct @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (base_local : B.u32) -> (base : B.u32) ->
    (fragment : Block.fragment) @ immutable -> (outer : T.labels) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | Table.lookup table pc === Some fragment && before.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && Wasm_locals.get before.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load before.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))
      && X.run (Block.emit fragment base_local) before === X.Done after} ->
    {u : unit | T.run (cost table pc base_local fragment) (configuration table base_local outer before)
      === T.Running (configuration table base_local outer after)} @ ghost =
  fun table pc base_local base fragment outer before after premise -> ghost_ (
    cost_def table pc base_local fragment; configuration_def table base_local outer before; configuration_def table base_local outer after;
    body_def table base_local;
    let loop_labels = labels table base_local outer in
    let inner = T.Label ({T.restart = None; continuation = back (); saved = S.Empty}, loop_labels) in
    let start = configuration table base_local outer before in
    T.run_def (cost table pc base_local fragment) start; T.step_def start;
    T.enter_def (Select.emit table base_local) (back ()) None start; T.stack_def before S.Empty;
    Execute.correct table pc base_local base fragment inner before after ();
    Fuel.correct (Execute.cost table pc base_local fragment) (C.Succ (C.Succ C.Zero)) {T.code = Select.emit table base_local; labels = inner; state = before};
    let done_ = {T.code = T.Empty; labels = inner; state = after} in
    T.run_def (C.Succ (C.Succ C.Zero)) done_; T.step_def done_; T.stack_def after S.Empty;
    let branch = {T.code = back (); labels = loop_labels; state = after} in
    T.run_def (C.Succ C.Zero) branch; back_def (); T.step_def branch;
    labels_def table base_local outer; T.branch_def 0 loop_labels after;
    T.run_def C.Zero (configuration table base_local outer after))
