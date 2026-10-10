module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Selection = Hmc_wasm_program_selection
module Edges = Hmc_wasm_program_edges
module Block = Hmc_wasm_program_block
module Codec = Hmc_pointer_frame_codec
module Cells = Hmc_wasm_call_save_memory
module Case = Hmc_wasm_program_source_caller
module Root = Hmc_wasm_program_step_root
module Read = Hmc_wasm_saved_frame_read
module Stack = Hmc_memory_stack
module Facts = Hmc_wasm_program_frame_facts
module Restore = Hmc_wasm_frame_restore
module Config = Hmc_wasm_program_runtime
module Round = Hmc_wasm_program_roundtrip
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module U = Hmc_tail_semantics
module Full = Hmc_wasm_return_frame
module New = Hmc_wasm_program_caller
module Index = Hmc_u32_index
module Progress = Hmc_heap_reachable_operands
let rec (objects_below @ total) : (heap : H.heap) @ immutable -> (limit : Wasm_u32.u32) -> (boundary : Wasm_u32.u32) ->
    {u : unit | Hmc_wasm_heap_suffix.above heap limit && boundary <= limit} ->
    {u : unit | Hmc_heap_image_suffix.above heap boundary} @ ghost = fun heap limit boundary premise -> ghost_ (
  Hmc_wasm_heap_suffix.above_def heap limit; Hmc_heap_image_suffix.above_def heap boundary;
  match heap with H.Empty_heap _ -> () | H.Allocate (_, rest) -> objects_below rest limit boundary ())
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && before.State.frames === Q.Frame (saved, frames)
      && I.lookup program.I.code before.State.activation.F.pc === Some (I.Keep G.Return)} ->
    {out : State.transition | State.valid program globals lowered context out.State.state
      && out.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && out.State.state.State.elapsed === D.S before.State.elapsed
      && out.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration out.State.state)
      && Wasm_calls.run out.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context out.State.state)} @ immutable =
  fun program globals lowered context before saved frames premise ->
    ghost_ (State.valid_def program globals lowered context before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory
        before.State.frame_end before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      State.configuration_def before; State.module__def program lowered context; State.loop_def context before);
    let selected = Selection.select_reachable program globals lowered context.State.max_pc context.State.block_count
      before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract before.State.activation before.State.frames
      before.State.pc context.State.table_base context.State.stack_base () in
    ghost_ (Root.select_return globals before.State.block.G.signature lowered.Lower.capacity context.State.max_pc selected.Selection.selected.Selection.fragment ();
      Edges.keep program before.State.activation.F.pc before.State.block G.Return ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks before.State.block;
      Block.corresponds_def globals before.State.block.G.signature (I.Keep G.Return) lowered.Lower.capacity context.State.max_pc selected.Selection.selected.Selection.fragment;
      Codec.shape_def before.State.block.G.signature before.State.activation;
      Codec.temporaries_shape_def before.State.block.G.signature.G.temporaries before.State.activation.F.temporaries;
      Codec.decode_def before.State.block.G.signature before.State.activation.F.pc before.State.cells;
      Lower.corresponds_def program globals context.State.max_pc lowered;
      Hmc_memory_stack.related_def program.I.origin.Hmc_cfg_program.blocks lowered.Lower.width before.State.memory
        context.State.stack_base before.State.registers.Registers.top before.State.frames);
    ghost_ (Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ();
      Restore.width_def lowered.Lower.capacity;
      Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract);
    let base = Stack.previous lowered.Lower.width before.State.registers.Registers.top in
    ghost_ (Stack.previous_def lowered.Lower.width before.State.registers.Registers.top);
    let caller = Read.read program.I.origin.Hmc_cfg_program.blocks before.State.memory base lowered.Lower.capacity saved () in
    ghost_ (Codec.decode_def caller.Read.block.G.signature saved.F.pc caller.Read.cells);
    match before.State.cells, caller.Read.cells with
    | H.Cell (_, H.Cell (_, source_rest)), H.Cell (_, H.Cell (_, rest)) ->
      let config = Config.config context.State.table_base context.State.stack_base in
      let target_state = {X.memory = before.State.memory; machine = {E.locals = Registers.locals before.State.registers; stack = S.Empty}} in
      ghost_ (Cells.cells_def before.State.pc before.State.activation.F.current before.State.activation.F.accumulator source_rest;
        H.length_def (Cells.cells before.State.pc before.State.activation.F.current before.State.activation.F.accumulator source_rest);
        Cells.cells_def caller.Read.pc saved.F.current saved.F.accumulator rest;
        H.length_def (Cells.cells caller.Read.pc saved.F.current saved.F.accumulator rest);
        H.length_def caller.Read.cells; H.length_def (H.Cell (saved.F.accumulator, rest));
        Index.represents_def (H.length caller.Read.cells) lowered.Lower.capacity;
        Index.represents_def (H.length (H.Cell (saved.F.accumulator, rest))) (lowered.Lower.capacity - 1);
        objects_below before.State.heap before.State.registers.Registers.stack_limit before.State.registers.Registers.top ();
        Hmc_wasm_heap_suffix.objects_above before.State.heap before.State.frame_end ();
        S.add32_def before.State.registers.Registers.frame (Restore.width lowered.Lower.capacity);
        Config.config_def context.State.table_base context.State.stack_base;
        Registers.local_values before.State.registers; Registers.matches_def (Registers.locals before.State.registers) before.State.registers;
        Hmc_wasm_program_status.zero_def ();
        L.can_set_def (Registers.locals before.State.registers) 11 (S.I32 0);
        S.same_type_def (S.I32 before.State.registers.Registers.status) (S.I32 0);
        Hmc_wasm_program_descriptors.valid_def program before.State.registers before.State.memory context.State.runtime context.State.table_base context.State.table_count);
      let result = Case.framed before.State.abstract before.State.frame_end before.State.cell_count (State.module_ program lowered context)
        before.State.registers context.State.host_capacity selected.Selection.selected.Selection.index lowered config.Hmc_wasm_program_functions.locals
        context.State.runtime context.State.table_base context.State.table_count (Round.labels config) program globals before.State.heap
        before.State.registers.Registers.heap_limit context.State.stack_capacity before.State.activation saved frames caller.Read.block.G.signature
        caller.Read.pc before.State.pc rest caller.Read.padding source_rest lowered.Lower.restore lowered.Lower.capacity target_state
        before.State.registers.Registers.frame base before.State.registers.Registers.top context.State.stack_base before.State.registers.Registers.stack_limit
        caller.Read.bytes caller.Read.suffix before.State.bytes before.State.suffix 0 4 () in
      let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
        activation = {saved with F.accumulator = before.State.activation.F.accumulator}; frames; block = caller.Read.block; pc = caller.Read.pc;
        registers = result.Case.registers; memory = result.Case.body.New.source.Full.state.X.memory;
        cells = H.Cell (saved.F.current, H.Cell (before.State.activation.F.accumulator, rest)); padding = caller.Read.padding;
        bytes = result.Case.body.New.source.Full.restored.Wasm_scatter_memory.bytes;
        suffix = result.Case.body.New.source.Full.restored.Wasm_scatter_memory.suffix} in
      ghost_ (H.length_def after.State.cells; H.length_def (H.Cell (before.State.activation.F.accumulator, rest));
        Progress.advance_next program context.State.input before.State.elapsed;
        State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
      {State.state = after; fuel = result.Case.fuel}
    | _ -> unreachable_ ()
