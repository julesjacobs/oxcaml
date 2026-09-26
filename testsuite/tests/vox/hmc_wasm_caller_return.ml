module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Cells = Hmc_wasm_call_save_memory
module Return = Hmc_wasm_return_result
module Full = Hmc_wasm_return_frame
module Restore = Hmc_wasm_frame_restore
module Stack = Hmc_memory_stack
module Plan = Wasm_parallel_copy
module Cfg = Hmc_cfg_program
module Closure = Hmc_closure_program
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module Source = Hmc_wasm_return_frame_source
module Select = Hmc_wasm_return_select
module T = Wasm_control
module C = Wasm_code
module Lift = Wasm_control_lift
module Continue = Wasm_control_branch_continue
module Fuel = Wasm_control_compose
module P = Hmc_linear_preservation
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
let[@def] (emit @ total) (plan : Plan.plan @ immutable) (count : Hmc_wasm_relayout.count)
    (source_local : B.u32) (caller_local : B.u32) (stack_base : B.u32)
    (root : T.code @ immutable) (tail : T.code @ immutable) =
  Select.emit stack_base caller_local root (Lift.embed (Full.emit plan (Restore.width count) source_local caller_local) T.Empty) tail
let[@def] (cost @ total) (plan : Plan.plan @ immutable) (count : Hmc_wasm_relayout.count)
    (source_local : B.u32) (caller_local : B.u32) (stack_base : B.u32) =
  Fuel.add (Select.cost stack_base caller_local) (Continue.cost (Full.emit plan (Restore.width count) source_local caller_local))
let (correct @ total) : (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (root : T.code) @ immutable -> (tail : T.code) @ immutable -> (labels : T.labels) @ immutable -> (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stack_base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= source
      && I.lookup program.I.code activation.F.pc === Some (I.Keep G.Return) && activation.F.temporaries === F.Empty
      && Hmc_u32_index.represents saved.F.pc pc && Hmc_u32_index.represents activation.F.pc source_pc
      && count >= 2 && base + 16 + 16 * count = top && top <= limit
      && source + 16 + 16 * count <= stack_base && stack_base <= base && source_local <> caller_local
      && Hmc_u32_index.represents (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest)))) count
      && Restore.matches plan (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))))
      && Bounds.covers state.X.memory limit && Image.related state.X.memory heap
      && H.valid program.I.origin.Cfg.origin.Closure.table heap && (H.used heap <= stack_base || Above.above heap top)
      && Above.above heap (S.add32 source (Restore.width count))
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) state.X.memory stack_base top (Q.Frame (saved, frames))
      && Bytes.drop state.X.memory base === Some before && Bytes.drop state.X.memory source === Some source_bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, padding)
      && Wire.decode_cells (H.length (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest)) source_bytes ===
        Some (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest, source_suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals caller_local === Some (S.I32 top)} ->
    {out : Full.result | Table.related runtime out.Full.state.X.memory table_base table_count
      && T.run (cost plan count source_local caller_local stack_base)
        {T.code = emit plan count source_local caller_local stack_base root tail; labels; state}
        === T.Running {T.code = tail; labels; state = out.Full.state}
      && out.Full.state.X.machine.E.stack === state.X.machine.E.stack
      && P.equal_prefix source state.X.memory out.Full.state.X.memory
      && X.run (Full.emit plan (Restore.width count) source_local caller_local) state === X.Done out.Full.state
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.Full.state.X.memory heap
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) out.Full.state.X.memory stack_base base frames
      && L.replaced state.X.machine.E.locals caller_local (S.I32 base) out.Full.state.X.machine.E.locals
      && L.get out.Full.state.X.machine.E.locals caller_local === Some (S.I32 base)
      && L.get out.Full.state.X.machine.E.locals source_local === Some (S.I32 source)
      && V.length out.Full.state.X.memory === V.length state.X.memory && Bounds.covers out.Full.state.X.memory limit
      && Bytes.drop out.Full.state.X.memory source === Some out.Full.restored.Wasm_scatter_memory.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.Full.restored.Wasm_scatter_memory.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, out.Full.restored.Wasm_scatter_memory.suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, padding)} @ immutable =
  fun runtime table_base table_count root tail labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let out = Source.correct program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local () in
    let code = Full.emit plan (Restore.width count) source_local caller_local in
    ghost_ (Restore.width_def count;
      Table.preserve runtime state.X.memory out.Full.state.X.memory table_base table_count source ();
      Select.correct program.I.origin.Cfg.blocks (Q.Frame (saved, frames)) (Restore.width count) stack_base caller_local top state labels
        root (Lift.embed code T.Empty) tail ();
      Continue.labels_def tail labels;
      Continue.correct code tail labels state out.Full.state ();
      emit_def plan count source_local caller_local stack_base root tail;
      cost_def plan count source_local caller_local stack_base;
      Fuel.correct (Select.cost stack_base caller_local) (Continue.cost code)
        {T.code = emit plan count source_local caller_local stack_base root tail; labels; state});
    out
