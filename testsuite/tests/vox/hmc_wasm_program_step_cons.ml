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
module Plan = Hmc_wasm_value_pop
module Model = Hmc_frame_value_pop
module Facts = Hmc_wasm_program_frame_facts
module Case = Hmc_wasm_program_source_cons
module Body = Hmc_wasm_cons_finish_invariant
module Progress = Hmc_heap_reachable_operands
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Geometry = Hmc_wasm_relayout_geometry
module Resource = Hmc_wasm_program_resource_step
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module New = Hmc_wasm_program_cons
module Entry = Hmc_wasm_cons_entry
module Guarded = Hmc_wasm_cons_guarded
module Allocate = Hmc_heap_allocate
module X = Wasm_memory_execution
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (next : D.index) @ immutable -> (plan : Plan.fragment) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Cons next))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Case.fragment plan)} ->
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
      Edges.keep program before.State.activation.Hmc_heap_frame.pc before.State.block (G.Cons next) ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks before.State.block;
      Case.fragment_def plan;
      Block.corresponds_def globals before.State.block.G.signature (I.Keep (G.Cons next)) lowered.Lower.capacity context.State.max_pc (Case.fragment plan);
      Structured.corresponds_def globals before.State.block.G.signature (G.Cons next) lowered.Lower.capacity context.State.max_pc
        (Structured.Cons plan);
      Plan.matches_def before.State.block.G.signature next lowered.Lower.capacity context.State.max_pc plan;
      Lower.corresponds_def program globals context.State.max_pc lowered);
    ghost_ (Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract;
      Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ());
    ghost_ (Hmc_memory_stack_capacity.ordered lowered.Lower.width context.State.stack_capacity context.State.stack_base before.State.registers.Registers.stack_limit ());
    let old_signature = before.State.block.G.signature in
    ghost_ (Codec.shape_def old_signature before.State.activation;
      Codec.temporaries_shape_def old_signature.G.temporaries before.State.activation.F.temporaries);
    match old_signature.G.temporaries, old_signature.G.accumulator, before.State.activation.F.temporaries with
    | G.Value (locals, head_type, schema), Some (D.List_type element), F.Value (head, env, rest) ->
      let signature = {G.locals; temporaries = schema; accumulator = Some (D.List_type element)} in
      let activation = {before.State.activation with F.pc = next; env; temporaries = rest;
        accumulator = V.Cons_pointer before.State.registers.Registers.heap} in
      let successor = Edges.next program.I.origin.Hmc_cfg_program.blocks next locals schema (Some (D.List_type element)) () in
      ghost_ (Model.successor_def old_signature (D.List_type element);
        Model.transition_def before.State.activation next (V.Cons_pointer before.State.registers.Registers.heap));
      (match Index.encode lowered.Lower.capacity (Codec.locals_size old_signature.G.locals),
        Index.encode lowered.Lower.capacity (Codec.locals_size locals), Index.encode lowered.Lower.capacity (Codec.temporaries_size schema) with
      | Some env_count, Some saved_count, Some rest_count ->
        ghost_ (Geometry.size_represents (Codec.locals_size old_signature.G.locals) env_count ();
          Geometry.size_represents (Codec.locals_size locals) saved_count ();
          Geometry.size_represents (Codec.temporaries_size schema) rest_count ());
        let result = Case.framed lowered (State.module_ program lowered context) context.State.table_base context.State.stack_base env_count
          program globals context.State.stack_capacity old_signature signature before.State.activation activation before.State.frames
          (D.List_type element) head_type next locals schema plan lowered.Lower.capacity context.State.max_pc before.State.pc
          head before.State.activation.F.accumulator before.State.cells before.State.padding before.State.heap before.State.frame_end
          before.State.registers before.State.memory before.State.bytes before.State.suffix context.State.host_capacity selected.Selection.selected.Selection.index
          before.State.abstract context.State.runtime context.State.table_count before.State.cell_count () in
        let allocated = result.Case.body.New.source.Entry.result in
        (match allocated.Guarded.allocation with
        | Allocate.Exhausted ->
          let after = {before with State.registers = result.Case.registers} in
          ghost_ (State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
          {Resource.failed_guard = ghost_ result.Case.failed_guard; state = after; fuel = result.Case.fuel; exhausted = Some Machine.Heap}
        | Allocate.Allocated allocation ->
          (match allocated.Guarded.frame with
          | Some frame ->
            let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
              heap = allocation.Allocate.heap; activation; block = successor; pc = plan.Plan.pc; registers = result.Case.registers;
              memory = result.Case.body.New.state.X.memory; cells = frame.Body.cells; padding = frame.Body.padding;
              bytes = frame.Body.bytes; suffix = allocated.Guarded.tail} in
            ghost_ (Frame.valid_def signature activation result.Case.registers after.State.memory before.State.frame_end plan.Plan.pc
                frame.Body.cells frame.Body.padding frame.Body.bytes allocated.Guarded.tail before.State.cell_count;
              Facts.decode_signature signature successor.G.signature activation frame.Body.cells frame.Body.padding ();
              Frame.valid_def successor.G.signature activation result.Case.registers after.State.memory before.State.frame_end plan.Plan.pc
                frame.Body.cells frame.Body.padding frame.Body.bytes allocated.Guarded.tail before.State.cell_count;
              Progress.advance_next program context.State.input before.State.elapsed;
              State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
            {Resource.failed_guard = ghost_ result.Case.failed_guard; state = after; fuel = result.Case.fuel; exhausted = None}
          | None -> unreachable_ ()))
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
