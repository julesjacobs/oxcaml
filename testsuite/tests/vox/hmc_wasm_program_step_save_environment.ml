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
module Plan = Hmc_wasm_relayout
module Model = Hmc_frame_relayout_model
module Simple = Hmc_heap_simple
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Facts = Hmc_wasm_program_frame_facts
module Case = Hmc_wasm_program_source_save_environment
module Body = Hmc_wasm_relayout_save_invariant
module Progress = Hmc_heap_reachable_operands
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (next : D.index) @ immutable -> (plan : Plan.fragment) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Save_environment next))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Case.fragment plan)} ->
    {out : State.transition | State.valid program globals lowered context out.State.state
      && out.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && out.State.state.State.elapsed === D.S before.State.elapsed
      && out.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration out.State.state)
      && Wasm_calls.run out.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context out.State.state)} @ immutable =
  fun program globals lowered context before next plan premise ->
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
      Edges.keep program before.State.activation.Hmc_heap_frame.pc before.State.block (G.Save_environment next) ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks before.State.block;
      Case.fragment_def plan;
      Block.corresponds_def globals before.State.block.G.signature (I.Keep (G.Save_environment next)) lowered.Lower.capacity context.State.max_pc (Case.fragment plan);
      Structured.corresponds_def globals before.State.block.G.signature (G.Save_environment next) lowered.Lower.capacity context.State.max_pc
        (Structured.Straight (Straight.Relayout plan));
      Straight.corresponds_def globals before.State.block.G.signature (G.Save_environment next) lowered.Lower.capacity context.State.max_pc (Straight.Relayout plan);
      ());
    ghost_ (Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract;
      Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ());
    let old_signature = before.State.block.G.signature in
    let schema = G.Environment (old_signature.G.locals, old_signature.G.temporaries) in
    let successor = Edges.next program.I.origin.Hmc_cfg_program.blocks next old_signature.G.locals schema None () in
    let signature = {old_signature with G.temporaries = schema} in
    let activation = {before.State.activation with F.pc = next;
      temporaries = F.Environment (before.State.activation.F.env, before.State.activation.F.temporaries)} in
    ghost_ (Model.successor_def old_signature (G.Save_environment next);
      Simple.step_def (G.Save_environment next) (Q.Running (before.State.activation, before.State.frames)));
    let result = Case.framed lowered (State.module_ program lowered context) program globals before.State.heap
      old_signature signature before.State.activation activation before.State.frames context.State.stack_capacity
      next plan lowered.Lower.capacity context.State.max_pc before.State.pc
      before.State.cells before.State.padding before.State.registers before.State.memory before.State.bytes before.State.suffix
      context.State.table_base context.State.stack_base context.State.host_capacity selected.Selection.selected.Selection.index
      before.State.frame_end before.State.cell_count before.State.abstract context.State.runtime context.State.table_count () in
    let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
      activation; block = successor; pc = plan.Plan.pc; registers = result.Case.registers; memory = result.Case.source.Body.memory;
      cells = result.Case.source.Body.cells; padding = result.Case.source.Body.padding; bytes = result.Case.source.Body.bytes} in
    ghost_ (Frame.valid_def signature activation result.Case.registers result.Case.source.Body.memory before.State.frame_end plan.Plan.pc
        result.Case.source.Body.cells result.Case.source.Body.padding result.Case.source.Body.bytes before.State.suffix before.State.cell_count;
      Facts.decode_signature signature successor.G.signature activation result.Case.source.Body.cells result.Case.source.Body.padding ();
      Frame.valid_def successor.G.signature activation result.Case.registers result.Case.source.Body.memory before.State.frame_end plan.Plan.pc
        result.Case.source.Body.cells result.Case.source.Body.padding result.Case.source.Body.bytes before.State.suffix before.State.cell_count;
      Progress.advance_next program context.State.input before.State.elapsed;
      State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
    {State.state = after; fuel = result.Case.fuel}
