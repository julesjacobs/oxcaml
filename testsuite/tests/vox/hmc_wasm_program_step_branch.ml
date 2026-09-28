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
module Ready = Hmc_wasm_program_branch_ready
module Branch = Hmc_wasm_program_source_branch
module Body = Hmc_wasm_branch_invariant
module Progress = Hmc_heap_reachable_operands
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable -> (yes_pc : B.u32) -> (no_pc : B.u32) ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Branch (yes, no)))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Branch.fragment yes_pc no_pc)} ->
    {out : State.transition | State.valid program globals lowered context out.State.state
      && out.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && out.State.state.State.elapsed === D.S before.State.elapsed
      && out.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration out.State.state)
      && Wasm_calls.run out.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context out.State.state)} @ immutable =
  fun program globals lowered context before yes no yes_pc no_pc premise ->
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
    ghost_ (Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract);
    let ready = Ready.prepare program globals before.State.heap before.State.activation before.State.frames before.State.abstract before.State.block
      lowered.Lower.capacity context.State.max_pc yes no yes_pc no_pc () in
    let result = Branch.framed lowered (State.module_ program lowered context) program globals before.State.heap
      before.State.block.G.signature ready.Ready.block.G.signature before.State.activation before.State.frames context.State.stack_capacity
      yes no yes_pc no_pc ready.Ready.condition ready.Ready.next ready.Ready.pc before.State.pc before.State.cells before.State.padding
      before.State.registers before.State.memory before.State.bytes before.State.suffix
      context.State.table_base context.State.stack_base context.State.host_capacity selected.Selection.selected.Selection.index
      before.State.frame_end before.State.cell_count before.State.abstract context.State.runtime context.State.table_count () in
    let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
      activation = result.Branch.source.Body.activation; block = ready.Ready.block; pc = ready.Ready.pc;
      registers = result.Branch.registers; memory = result.Branch.source.Body.memory;
      cells = result.Branch.source.Body.cells; bytes = result.Branch.source.Body.bytes} in
    ghost_ (Progress.advance_next program context.State.input before.State.elapsed;
      State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
    {State.state = after; fuel = result.Branch.fuel}
