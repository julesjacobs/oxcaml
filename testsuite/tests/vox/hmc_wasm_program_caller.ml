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
module Caller = Hmc_wasm_caller_return
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module LP = Wasm_local_preservation
type result = {source : Full.result; prepared : X.state; fuel : C.count}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (labels : T.labels) @ immutable -> (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stack_base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | source_local = locals.Emit.structured.Hmc_wasm_structured_block.frame && caller_local = locals.Emit.top
      && plan === lowered.Lower.restore && count = lowered.Lower.capacity
      && locals.Emit.status <> source_local && locals.Emit.status <> caller_local
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ()))
      && state.X.machine.E.stack === S.Empty
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
    {out : result | L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.prepared.X.machine.E.locals
      && L.replaced out.prepared.X.machine.E.locals caller_local (S.I32 base) out.source.Full.state.X.machine.E.locals
      && Table.related runtime out.source.Full.state.X.memory table_base table_count
      && T.run out.fuel {T.code = Emit.emit lowered Hmc_wasm_program_block.Return locals table_base stack_base; labels; state}
        === T.Running {T.code = T.Empty; labels; state = out.source.Full.state}
      && L.get out.source.Full.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && out.source.Full.state.X.machine.E.stack === state.X.machine.E.stack
      && P.equal_prefix source state.X.memory out.source.Full.state.X.memory
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.source.Full.state.X.memory heap
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) out.source.Full.state.X.memory stack_base base frames
      && L.get out.source.Full.state.X.machine.E.locals caller_local === Some (S.I32 base)
      && L.get out.source.Full.state.X.machine.E.locals source_local === Some (S.I32 source)
      && V.length out.source.Full.state.X.memory === V.length state.X.memory && Bounds.covers out.source.Full.state.X.memory limit
      && Bytes.drop out.source.Full.state.X.memory source === Some out.source.Full.restored.Wasm_scatter_memory.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.source.Full.restored.Wasm_scatter_memory.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, out.source.Full.restored.Wasm_scatter_memory.suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, padding)} @ immutable =
  fun lowered locals runtime table_base table_count labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let local = locals.Emit.status in
    let root = Lift.embed (Wasm_frame_snapshot.emit (Hmc_wasm_root_return.reads locals.Emit.result_tag locals.Emit.result_payload) source_local)
      (Emit.status local 1 T.Empty) in
    let body = Caller.emit plan count source_local caller_local stack_base root T.Empty in
    let prepared = Status.write local (Status.zero ()) body labels state () in
    ghost_ (L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals source_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals caller_local ());
    let returned = Caller.correct runtime table_base table_count root T.Empty labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count prepared source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local () in
    let body_fuel = Caller.cost plan count source_local caller_local stack_base in
    ghost_ (L.other_local prepared.X.machine.E.locals caller_local (S.I32 base) returned.Full.state.X.machine.E.locals local ();
      Fuel.correct (Status.two ()) body_fuel {T.code = Emit.status local (Status.zero ()) body; labels; state};
      Status.zero_def (); Emit.emit_def lowered Hmc_wasm_program_block.Return locals table_base stack_base);
    {source = returned; prepared; fuel = Fuel.add (Status.two ()) body_fuel}
