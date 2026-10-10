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
module New = Hmc_wasm_program_caller
module Registers = Hmc_wasm_program_registers
module Config = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module Step = Hmc_wasm_program_register_step
module Dispatch = Hmc_wasm_program_dispatch
module Func = Wasm_functions
module Calls = Wasm_calls
module Header = Hmc_wasm_header_update
module Block = Hmc_wasm_program_block
type result = {body : New.result; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}}
let (correct @ total) : (module_ : Func.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (host_capacity : C.count) @ immutable -> (function_index : B.u32) -> (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (labels : T.labels) @ immutable -> (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stack_base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | locals === (Config.config table_base stack_base).Assembly.locals
      && labels === Round.labels (Config.config table_base stack_base)
      && state.X.machine.E.locals === Registers.locals registers
      && source = registers.Registers.frame && top = registers.Registers.top && heap_limit = registers.Registers.heap_limit
      && source <= 4294967280
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table source_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered Block.Return (Config.config table_base stack_base))
      && source_local = locals.Emit.structured.Hmc_wasm_structured_block.frame && caller_local = locals.Emit.top
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
    {out : result | out.registers === {registers with Registers.top = base; status = Status.zero ()}
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals registers) state.X.memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.source.Full.state.X.memory (C.Succ host_capacity))
      && L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.body.New.prepared.X.machine.E.locals
      && L.replaced out.body.New.prepared.X.machine.E.locals caller_local (S.I32 base) out.body.New.source.Full.state.X.machine.E.locals
      && Table.related runtime out.body.New.source.Full.state.X.memory table_base table_count
      && T.run out.body.New.fuel {T.code = Emit.emit lowered Hmc_wasm_program_block.Return locals table_base stack_base; labels; state}
        === T.Running {T.code = T.Empty; labels; state = out.body.New.source.Full.state}
      && L.get out.body.New.source.Full.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && out.body.New.source.Full.state.X.machine.E.stack === state.X.machine.E.stack
      && P.equal_prefix source state.X.memory out.body.New.source.Full.state.X.memory
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.body.New.source.Full.state.X.memory heap
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) out.body.New.source.Full.state.X.memory stack_base base frames
      && L.get out.body.New.source.Full.state.X.machine.E.locals caller_local === Some (S.I32 base)
      && L.get out.body.New.source.Full.state.X.machine.E.locals source_local === Some (S.I32 source)
      && V.length out.body.New.source.Full.state.X.memory === V.length state.X.memory && Bounds.covers out.body.New.source.Full.state.X.memory limit
      && Bytes.drop out.body.New.source.Full.state.X.memory source === Some out.body.New.source.Full.restored.Wasm_scatter_memory.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.body.New.source.Full.restored.Wasm_scatter_memory.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, out.body.New.source.Full.restored.Wasm_scatter_memory.suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, padding)} @ immutable =
  fun module_ registers host_capacity function_index lowered locals runtime table_base table_count labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let body = New.correct lowered locals runtime table_base table_count labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local () in
    let contents = H.Cell (activation.F.current, H.Cell (activation.F.accumulator, source_rest)) in
    ghost_ (Config.config_def table_base stack_base;
      Cells.cells_def source_pc activation.F.current activation.F.accumulator source_rest;
      H.length_def (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest);
      Registers.local_values registers; Registers.exports_def (Registers.locals registers) registers;
      L.other_local state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.prepared.X.machine.E.locals 1 ();
      L.other_local state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.prepared.X.machine.E.locals 12 ();
      L.other_local state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.prepared.X.machine.E.locals 13 ();
      L.other_local body.New.prepared.X.machine.E.locals 4 (S.I32 base) body.New.source.Full.state.X.machine.E.locals 1 ();
      L.other_local body.New.prepared.X.machine.E.locals 4 (S.I32 base) body.New.source.Full.state.X.machine.E.locals 12 ();
      L.other_local body.New.prepared.X.machine.E.locals 4 (S.I32 base) body.New.source.Full.state.X.machine.E.locals 13 ());
    let after = Step.complete lowered Block.Return module_ table_base stack_base registers state.X.memory source_bytes contents source_suffix
      host_capacity source_pc function_index body.New.fuel body.New.source.Full.state () in
    ghost_ (Registers.exports_def body.New.source.Full.state.X.machine.E.locals after; Status.zero_def ());
    {body; registers = after; fuel = Hmc_wasm_program_cost.dispatch (Round.cost (Config.config table_base stack_base) body.New.fuel) after.Registers.status}

module Resources = Hmc_wasm_program_resources
module Shared_frame = Hmc_wasm_program_frame
module Descriptors = Hmc_wasm_program_descriptors
let (framed @ total) : (abstract : Hmc_cfg_semantics.state) @ immutable -> (frame_end : B.u32) -> (frame_count : B.u32) -> (module_ : Func.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (host_capacity : C.count) @ immutable -> (function_index : B.u32) -> (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (labels : T.labels) @ immutable -> (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stack_base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | Resources.valid program globals lowered.Lower.width stack_base frame_end abstract heap activation (Q.Frame (saved, frames)) registers state.X.memory
      && Descriptors.valid program registers state.X.memory runtime table_base table_count
      && lowered.Lower.width = Restore.width count && frame_end = source + 16 + 16 * count
      && frame_count = count + 1 && source <= 4294967248
      && locals === (Config.config table_base stack_base).Assembly.locals
      && labels === Round.labels (Config.config table_base stack_base)
      && state.X.machine.E.locals === Registers.locals registers
      && source = registers.Registers.frame && top = registers.Registers.top && heap_limit = registers.Registers.heap_limit
      && source <= 4294967280
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table source_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered Block.Return (Config.config table_base stack_base))
      && source_local = locals.Emit.structured.Hmc_wasm_structured_block.frame && caller_local = locals.Emit.top
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
    {out : result | Resources.valid program globals lowered.Lower.width stack_base frame_end (Hmc_tail_semantics.step program abstract)
        heap {saved with F.accumulator = activation.F.accumulator} frames out.registers out.body.New.source.Full.state.X.memory
      && Descriptors.valid program out.registers out.body.New.source.Full.state.X.memory runtime table_base table_count
      && Shared_frame.valid signature {saved with F.accumulator = activation.F.accumulator} out.registers out.body.New.source.Full.state.X.memory
        frame_end pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) padding
        out.body.New.source.Full.restored.Wasm_scatter_memory.bytes out.body.New.source.Full.restored.Wasm_scatter_memory.suffix frame_count
      && out.registers === {registers with Registers.top = base; status = Status.zero ()}
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals registers) state.X.memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.source.Full.state.X.memory (C.Succ host_capacity))
      && L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.body.New.prepared.X.machine.E.locals
      && L.replaced out.body.New.prepared.X.machine.E.locals caller_local (S.I32 base) out.body.New.source.Full.state.X.machine.E.locals
      && Table.related runtime out.body.New.source.Full.state.X.memory table_base table_count
      && T.run out.body.New.fuel {T.code = Emit.emit lowered Hmc_wasm_program_block.Return locals table_base stack_base; labels; state}
        === T.Running {T.code = T.Empty; labels; state = out.body.New.source.Full.state}
      && L.get out.body.New.source.Full.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && out.body.New.source.Full.state.X.machine.E.stack === state.X.machine.E.stack
      && P.equal_prefix source state.X.memory out.body.New.source.Full.state.X.memory
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.body.New.source.Full.state.X.memory heap
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) out.body.New.source.Full.state.X.memory stack_base base frames
      && L.get out.body.New.source.Full.state.X.machine.E.locals caller_local === Some (S.I32 base)
      && L.get out.body.New.source.Full.state.X.machine.E.locals source_local === Some (S.I32 source)
      && V.length out.body.New.source.Full.state.X.memory === V.length state.X.memory && Bounds.covers out.body.New.source.Full.state.X.memory limit
      && Bytes.drop out.body.New.source.Full.state.X.memory source === Some out.body.New.source.Full.restored.Wasm_scatter_memory.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.body.New.source.Full.restored.Wasm_scatter_memory.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, out.body.New.source.Full.restored.Wasm_scatter_memory.suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, padding)} @ immutable =
  fun abstract frame_end frame_count module_ registers host_capacity function_index lowered locals runtime table_base table_count labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let out = correct module_ registers host_capacity function_index lowered locals runtime table_base table_count labels program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local () in
    ghost_ (
      Resources.valid_def program globals lowered.Lower.width stack_base frame_end abstract heap activation (Q.Frame (saved, frames)) registers state.X.memory;
      Hmc_heap_invariant.step program globals registers.Registers.heap_limit stack_limit
        {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} abstract ();
      Bounds.same_length state.X.memory out.body.New.source.Full.state.X.memory registers.Registers.heap_limit ();
      Bounds.same_length state.X.memory out.body.New.source.Full.state.X.memory registers.Registers.stack_limit ();
      Resources.valid_def program globals lowered.Lower.width stack_base frame_end (Hmc_tail_semantics.step program abstract)
        heap {saved with F.accumulator = activation.F.accumulator} frames out.registers out.body.New.source.Full.state.X.memory;
      Descriptors.preserve program registers out.registers state.X.memory out.body.New.source.Full.state.X.memory runtime table_base table_count ();
      let cells = H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest)) in
      H.length_def cells; H.length_def (H.Cell (activation.F.accumulator, rest));
      H.length_def (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))); H.length_def (H.Cell (saved.F.accumulator, rest));
      Hmc_u32_index.represents_def (D.S (H.length cells)) frame_count;
      Cells.cells_def pc saved.F.current activation.F.accumulator rest;
      H.length_def (Cells.cells pc saved.F.current activation.F.accumulator rest);
      Shared_frame.valid_def signature {saved with F.accumulator = activation.F.accumulator} out.registers out.body.New.source.Full.state.X.memory
        frame_end pc cells padding out.body.New.source.Full.restored.Wasm_scatter_memory.bytes out.body.New.source.Full.restored.Wasm_scatter_memory.suffix frame_count);
    out
