module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module F = Hmc_heap_frame
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module H = Hmc_heap_objects
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Selection = Hmc_wasm_program_selection
module Block = Hmc_wasm_program_block
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Operands = Hmc_heap_operand_shapes
module Facts = Hmc_wasm_program_frame_facts
module Ready = Hmc_wasm_program_call_ready
module Model = Hmc_frame_call_entry
module Layout = Hmc_wasm_call_header_layout
module Geometry = Hmc_wasm_relayout_geometry
module Case = Hmc_wasm_program_source_tail
module Entry = Hmc_wasm_dynamic_call_entry
module Body = Hmc_wasm_dynamic_call_frame
module Extend = Hmc_wasm_frame_extend
module Progress = Hmc_heap_reachable_operands
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (env_count : Hmc_wasm_relayout.count) ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some I.Tail_call
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Block.Tail_call env_count)} ->
    {out : State.transition | State.valid program globals lowered context out.State.state
      && out.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && out.State.state.State.frames === before.State.frames
      && out.State.state.State.heap === before.State.heap
      && out.State.state.State.registers.Registers.top = before.State.registers.Registers.top
      && out.State.state.State.registers.Registers.stack_limit = before.State.registers.Registers.stack_limit
      && out.State.state.State.elapsed === D.S before.State.elapsed
      && out.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration out.State.state)
      && Wasm_calls.run out.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context out.State.state)} @ immutable =
  fun program globals lowered context before env_count premise ->
    ghost_ (State.valid_def program globals lowered context before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory
        before.State.frame_end before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      State.configuration_def before; State.module__def program lowered context; State.loop_def context before;
      Progress.progress program context.State.input before.State.elapsed before.State.abstract ());
    let selected = Selection.select_reachable program globals lowered context.State.max_pc context.State.block_count
      before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract before.State.activation before.State.frames
      before.State.pc context.State.table_base context.State.stack_base () in
    ghost_ (Lower.corresponds_def program globals context.State.max_pc lowered;
      Block.corresponds_def globals before.State.block.G.signature I.Tail_call lowered.Lower.capacity context.State.max_pc (Block.Tail_call env_count);
      Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract;
      Operands.call_instruction_def I.Tail_call;
      Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ());
    let operands = Operands.call program before.State.heap before.State.activation before.State.frames before.State.abstract I.Tail_call () in
    let successor = Ready.entry_block program operands.Operands.id operands.Operands.entry operands.Operands.code () in
    ghost_ (Ready.capture_capacity program.I.origin.C.origin.Hmc_closure_program.table lowered.Lower.calls lowered.Lower.capacity
        operands.Operands.id operands.Operands.entry ();
      Codec.shape_def before.State.block.G.signature before.State.activation;
      Codec.temporaries_shape_def before.State.block.G.signature.G.temporaries before.State.activation.F.temporaries);
    match before.State.block.G.signature.G.temporaries with
    | G.Value (locals, ty, schema) ->
      (match Index.encode lowered.Lower.capacity (H.length operands.Operands.captured) with
      | None -> unreachable_ ()
      | Some count ->
        ghost_ (Geometry.size_represents (Codec.locals_size operands.Operands.entry.K.captured) count ());
        let used = (if operands.Operands.entry.K.recursive then 4 else 3) + count in
        let stop : B.u32 = before.State.registers.Registers.frame + 16 + 16 * used in
        ghost_ (Layout.width_def operands.Operands.entry.K.recursive;
          Ready.operand_address before.State.block.G.signature before.State.activation before.State.cells before.State.padding env_count
            before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ();
          Hmc_wasm_program_step_caller.objects_below before.State.heap before.State.frame_end stop ();
          Hmc_memory_stack_capacity.ordered lowered.Lower.width context.State.stack_capacity context.State.stack_base before.State.registers.Registers.stack_limit ();
          Hmc_wasm_program_descriptors.valid_def program before.State.registers before.State.memory context.State.runtime context.State.table_base context.State.table_count);
        let result = Case.framed lowered (State.module_ program lowered context) before.State.frames globals context.State.stack_capacity context.State.stack_base
          program before.State.heap operands.Operands.entry operands.Operands.code operands.Operands.id operands.Operands.address operands.Operands.captured
          before.State.block.G.signature before.State.activation locals ty schema before.State.cells before.State.padding before.State.pc env_count
          before.State.bytes before.State.suffix count context.State.runtime context.State.table_base context.State.table_count before.State.registers before.State.memory stop
          context.State.host_capacity selected.Selection.selected.Selection.index used before.State.frame_end before.State.cell_count before.State.abstract () in
        let step = result.Case.step in
        let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
          activation = step.Case.callee.Entry.entered; block = successor; pc = step.Case.pc; registers = step.Case.registers;
          memory = step.Case.callee.Entry.frame.Body.memory; cells = result.Case.frame.Extend.cells;
          padding = result.Case.frame.Extend.padding; bytes = step.Case.callee.Entry.frame.Body.bytes} in
        ghost_ (Model.activation_def operands.Operands.entry operands.Operands.code.C.start (Hmc_tagged_cell.Closure_pointer operands.Operands.address)
            before.State.activation.F.accumulator operands.Operands.captured;
          Progress.advance_next program context.State.input before.State.elapsed;
          State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
        {State.state = after; fuel = step.Case.fuel})
    | _ -> unreachable_ ()
