module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
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
module Plan = Hmc_wasm_closure_lower
module Facts = Hmc_wasm_program_frame_facts
module Case = Hmc_wasm_program_source_closure
module Body = Hmc_wasm_closure_finish
module Progress = Hmc_heap_reachable_operands
module Resource = Hmc_wasm_program_resource_step
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module New = Hmc_wasm_program_closure
module Success = Hmc_wasm_closure_success
module Guarded = Hmc_wasm_closure_guarded
module Allocate = Hmc_heap_allocate
module X = Wasm_memory_execution
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (id : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    (next : D.index) @ immutable -> (plan : Plan.fragment) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Load (G.Closure id, ty, derivation, next)))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Case.fragment plan.Plan.object_ plan.Plan.pc)} ->
    {out : Resource.result | State.valid program globals lowered context out.Resource.state
      && out.Resource.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && (match out.Resource.exhausted with
        | None -> out.Resource.state.State.elapsed === D.S before.State.elapsed
          && out.Resource.state.State.abstract === U.step program before.State.abstract
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Advanced (State.configuration out.Resource.state)
          && Wasm_calls.run out.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Running (State.loop context out.Resource.state)
        | Some why -> why === Machine.Heap
          && (match out.Resource.failed_guard with
            | Guard.Absent -> false
            | Guard.Present guard -> Guard.reaches Failed.Heap (State.module_ program lowered context)
              (State.loop context before) guard)
          && State.configuration out.Resource.state === State.configuration before
          && out.Resource.state.State.elapsed === before.State.elapsed
          && out.Resource.state.State.abstract === before.State.abstract
          && out.Resource.state.State.memory === before.State.memory
          && out.Resource.state.State.registers.Registers.status = 2
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Exhausted why
          && Wasm_calls.run out.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals out.Resource.state.State.registers;
              execution = {X.memory = before.State.memory; machine = {Wasm_execution.locals = Wasm_scalar.Empty;
                stack = Wasm_scalar.Push (Wasm_scalar.I32 out.Resource.state.State.registers.Registers.status, Wasm_scalar.Empty)}}})} @ immutable =
  fun program globals lowered context before id ty derivation next plan premise ->
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
      Edges.keep program before.State.activation.Hmc_heap_frame.pc before.State.block (G.Load (G.Closure id, ty, derivation, next)) ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks before.State.block;
      Case.fragment_def plan.Plan.object_ plan.Plan.pc;
      Block.corresponds_def globals before.State.block.G.signature (I.Keep (G.Load (G.Closure id, ty, derivation, next))) lowered.Lower.capacity context.State.max_pc (Case.fragment plan.Plan.object_ plan.Plan.pc);
      Structured.corresponds_def globals before.State.block.G.signature (G.Load (G.Closure id, ty, derivation, next)) lowered.Lower.capacity context.State.max_pc
        (Structured.Closure plan);
      Plan.matches_def before.State.block.G.signature id next lowered.Lower.capacity context.State.max_pc plan;
      Lower.corresponds_def program globals context.State.max_pc lowered);
    ghost_ (Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract;
      Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ());
    ghost_ (Hmc_memory_stack_capacity.ordered lowered.Lower.width context.State.stack_capacity context.State.stack_base before.State.registers.Registers.stack_limit ());
    let successor = Edges.next program.I.origin.Hmc_cfg_program.blocks next before.State.block.G.signature.G.locals
      before.State.block.G.signature.G.temporaries (Some ty) () in
    let result = Case.framed lowered (State.module_ program lowered context) context.State.table_base context.State.stack_base
      program globals context.State.stack_capacity before.State.heap before.State.block.G.signature successor.G.signature
      before.State.activation before.State.frames id ty derivation next before.State.cells before.State.padding before.State.pc plan.Plan.pc
      lowered.Lower.capacity context.State.max_pc plan.Plan.object_ before.State.registers before.State.memory before.State.frame_end
      before.State.bytes before.State.suffix context.State.host_capacity selected.Selection.selected.Selection.index
      before.State.abstract context.State.runtime context.State.table_count before.State.cell_count () in
    match result.Case.body.New.source.Guarded.success with
    | None ->
      let after = {before with State.registers = result.Case.registers} in
      ghost_ (State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
      {Resource.failed_guard = ghost_ result.Case.failed_guard; state = after; fuel = result.Case.fuel; exhausted = Some Machine.Heap}
    | Some success ->
      let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
        heap = success.Success.allocation.Allocate.heap; activation = success.Success.frame.Body.activation;
        block = successor; pc = plan.Plan.pc; registers = result.Case.registers; memory = result.Case.body.New.state.X.memory;
        cells = success.Success.frame.Body.cells; bytes = success.Success.frame.Body.bytes; suffix = success.Success.tail} in
      ghost_ (Progress.advance_next program context.State.input before.State.elapsed;
        State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
      {Resource.failed_guard = ghost_ result.Case.failed_guard; state = after; fuel = result.Case.fuel; exhausted = None}
