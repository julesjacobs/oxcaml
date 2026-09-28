module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_objects
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Selection = Hmc_wasm_program_selection
module Edges = Hmc_wasm_program_edges
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Global = Hmc_wasm_global_lower
module Case = Hmc_wasm_program_source_global
module Body = Hmc_wasm_global_invariant
module Progress = Hmc_heap_reachable_operands
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (value : Hmc_tagged_cell.value) @ immutable -> (pc : B.u32) ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Load (G.Global index, ty, derivation, next)))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Case.fragment value pc)} ->
    {out : State.transition | State.valid program globals lowered context out.State.state
      && out.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && out.State.state.State.elapsed === D.S before.State.elapsed
      && out.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration out.State.state)
      && Wasm_calls.run out.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context out.State.state)} @ immutable =
  fun program globals lowered context before index ty derivation next value pc premise ->
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
    ghost_ (
      Edges.keep program before.State.activation.Hmc_heap_frame.pc before.State.block (G.Load (G.Global index, ty, derivation, next)) ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks before.State.block;
      Case.fragment_def value pc;
      Block.corresponds_def globals before.State.block.G.signature (I.Keep (G.Load (G.Global index, ty, derivation, next))) lowered.Lower.capacity context.State.max_pc (Case.fragment value pc);
      Structured.corresponds_def globals before.State.block.G.signature (G.Load (G.Global index, ty, derivation, next)) lowered.Lower.capacity context.State.max_pc
        (Structured.Straight (Straight.Global {Global.value; pc}));
      Straight.corresponds_def globals before.State.block.G.signature (G.Load (G.Global index, ty, derivation, next)) lowered.Lower.capacity context.State.max_pc (Straight.Global {Global.value; pc});
      Global.corresponds_def globals (G.Load (G.Global index, ty, derivation, next)) {Global.value; pc});
    let successor = Edges.next program.I.origin.Hmc_cfg_program.blocks next before.State.block.G.signature.G.locals
      before.State.block.G.signature.G.temporaries (Some ty) () in
    let result = Case.framed lowered (State.module_ program lowered context) program globals before.State.heap
      before.State.block.G.signature successor.G.signature before.State.activation before.State.frames context.State.stack_capacity
      index ty derivation next value pc before.State.pc before.State.cells before.State.padding
      before.State.registers before.State.memory before.State.bytes before.State.suffix
      context.State.table_base context.State.stack_base context.State.host_capacity selected.Selection.selected.Selection.index
      before.State.frame_end before.State.cell_count before.State.abstract context.State.runtime context.State.table_count () in
    let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
      activation = result.Case.source.Body.activation; block = successor; pc;
      registers = result.Case.registers; memory = result.Case.source.Body.memory;
      cells = result.Case.source.Body.cells; bytes = result.Case.source.Body.bytes} in
    ghost_ (Progress.advance_next program context.State.input before.State.elapsed;
      State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
    {State.state = after; fuel = result.Case.fuel}
