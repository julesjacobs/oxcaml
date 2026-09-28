module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
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
module Case = Hmc_wasm_program_source_root
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
let (select_return @ total) : (globals : Machine.globals) @ immutable -> (signature : G.signature) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (max_pc : Wasm_u32.u32) -> (fragment : Block.fragment) @ immutable ->
    {u : unit | Block.corresponds globals signature (I.Keep G.Return) capacity max_pc fragment} ->
    {u : unit | fragment === Block.Return} @ ghost = fun globals signature capacity max_pc fragment premise -> ghost_ (
  Block.corresponds_def globals signature (I.Keep G.Return) capacity max_pc fragment;
  match fragment with
  | Block.Structured body ->
    Structured.corresponds_def globals signature G.Return capacity max_pc body;
    (match body with
    | Structured.Straight code ->
      Straight.corresponds_def globals signature G.Return capacity max_pc code;
      (match code with
      | Straight.Simple simple -> Hmc_wasm_simple_lower.corresponds_def G.Return simple
      | Straight.Relayout layout -> Hmc_wasm_relayout_geometry.matches_def signature G.Return capacity max_pc
          layout.Hmc_wasm_relayout.copies layout.Hmc_wasm_relayout.pc layout.Hmc_wasm_relayout.required
      | Straight.Primitive primitive -> Hmc_wasm_primitive_lower.matches_def signature G.Return capacity max_pc primitive
      | Straight.Global global -> Hmc_wasm_global_lower.corresponds_def globals G.Return global)
    | _ -> ())
  | _ -> ())
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && before.State.frames === Q.Halt
      && I.lookup program.I.code before.State.activation.F.pc === Some (I.Keep G.Return)} ->
    {out : Case.result | out.Case.registers.Registers.status = 1
      && out.Case.registers.Registers.tag === V.tag before.State.activation.F.accumulator
      && out.Case.registers.Registers.payload === V.payload before.State.activation.F.accumulator
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
      && Hmc_heap_invariant.valid program globals before.State.registers.Registers.heap_limit
        {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
        (Hmc_tail_semantics.step program before.State.abstract)
      && Wasm_calls.run out.Case.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals out.Case.registers;
          execution = {Wasm_memory_execution.memory = before.State.memory;
            machine = {Wasm_execution.locals = Wasm_scalar.Empty;
              stack = Wasm_scalar.Push (Wasm_scalar.I32 (Hmc_wasm_program_root.finished ()), Wasm_scalar.Empty)}}}} @ immutable =
  fun program globals lowered context before premise ->
    ghost_ (State.valid_def program globals lowered context before;
      Resources.valid_def program globals lowered.Lower.width context.State.stack_base before.State.frame_end before.State.abstract
        before.State.heap before.State.activation before.State.frames before.State.registers before.State.memory;
      Frame.valid_def before.State.block.G.signature before.State.activation before.State.registers before.State.memory
        before.State.frame_end before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
      State.configuration_def before; State.module__def program lowered context; State.loop_def context before);
    let selected = Selection.select_reachable program globals lowered context.State.max_pc context.State.block_count
      before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract before.State.activation before.State.frames
      before.State.pc context.State.table_base context.State.stack_base () in
    ghost_ (select_return globals before.State.block.G.signature lowered.Lower.capacity context.State.max_pc selected.Selection.selected.Selection.fragment ();
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
    match before.State.cells with
    | H.Cell (_, H.Cell (_, rest)) ->
      ghost_ (Cells.cells_def before.State.pc before.State.activation.F.current before.State.activation.F.accumulator rest;
        H.length_def (Cells.cells before.State.pc before.State.activation.F.current before.State.activation.F.accumulator rest));
      let result = Case.correct lowered (State.module_ program lowered context) program globals before.State.heap before.State.activation
        context.State.stack_capacity before.State.registers before.State.memory before.State.bytes before.State.suffix before.State.pc rest
        context.State.table_base context.State.stack_base context.State.host_capacity selected.Selection.selected.Selection.index () in
      ghost_ (Hmc_heap_invariant.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity
        (State.configuration before) before.State.abstract ());
      result
    | _ -> unreachable_ ()
