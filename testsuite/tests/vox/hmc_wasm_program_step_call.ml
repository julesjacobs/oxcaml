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
module Case = Hmc_wasm_program_source_call
module Entry = Hmc_wasm_dynamic_call_entry
module Progress = Hmc_heap_reachable_operands
module Edges = Hmc_wasm_program_edges
module Q = Hmc_heap_state
module Cap = Hmc_frame_capacity
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Capacity = Hmc_memory_stack_capacity
module Save = Hmc_wasm_call_save
module Config = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Emit = Hmc_wasm_program_emit
module Structured = Hmc_wasm_structured_block
module Round = Hmc_wasm_program_roundtrip
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Capture = Hmc_wasm_cons_capture
module Slots = Hmc_wasm_descriptor_load
module New = Hmc_wasm_program_call
module Resource = Hmc_wasm_program_resource_step
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module Call = Hmc_wasm_ordinary_call
module Call_Loaded = Hmc_wasm_loaded_call
module Dispatch = Hmc_wasm_call_dispatch
module Enter = Hmc_wasm_call_plan_enter
module Saved_body = Hmc_wasm_call_save_push
module Loaded = Hmc_wasm_call_save_loaded
module Source = Hmc_wasm_call_save_source
module Slice = Hmc_frame_call_slices
let (step @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    (next : D.index) @ immutable -> (call : Block.call) @ immutable ->
    {u : unit | State.valid program globals lowered context before
      && I.lookup program.I.code before.State.activation.Hmc_heap_frame.pc === Some (I.Keep (G.Call next))
      && Table.lookup lowered.Lower.blocks before.State.pc === Some (Block.Call call)} ->
    {out : Resource.result | State.valid program globals lowered context out.Resource.state
      && out.Resource.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && (match out.Resource.exhausted with
        | None -> out.Resource.state.State.elapsed === D.S before.State.elapsed
          && out.Resource.state.State.abstract === U.step program before.State.abstract
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Advanced (State.configuration out.Resource.state)
          && Wasm_calls.run out.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Running (State.loop context out.Resource.state)
        | Some why -> why === Machine.Stack
          && (match out.Resource.failed_guard with
            | Guard.Absent -> false
            | Guard.Present guard -> Guard.reaches Failed.Stack (State.module_ program lowered context)
              (State.loop context before) guard)
          && State.configuration out.Resource.state === State.configuration before
          && out.Resource.state.State.elapsed === before.State.elapsed
          && out.Resource.state.State.abstract === before.State.abstract
          && out.Resource.state.State.memory === before.State.memory
          && out.Resource.state.State.registers.Registers.status = 3
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Exhausted why
          && Wasm_calls.run out.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals out.Resource.state.State.registers;
              execution = {X.memory = before.State.memory; machine = {Wasm_execution.locals = Wasm_scalar.Empty;
                stack = Wasm_scalar.Push (Wasm_scalar.I32 out.Resource.state.State.registers.Registers.status, Wasm_scalar.Empty)}}})} @ immutable =
  fun program globals lowered context before next call premise ->
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
      Block.corresponds_def globals before.State.block.G.signature (I.Keep (G.Call next)) lowered.Lower.capacity context.State.max_pc (Block.Call call);
      Edges.keep program before.State.activation.F.pc before.State.block (G.Call next) ();
      G.block_valid_def (Hmc_monomorphic.manifest program.I.origin.C.origin.Hmc_closure_program.origin.Hmc_monomorphic.definitions)
        program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.blocks before.State.block;
      Hmc_heap_invariant.valid_def program globals before.State.registers.Registers.heap_limit (State.configuration before) before.State.abstract;
      Operands.call_instruction_def (I.Keep (G.Call next));
      Facts.extent before.State.cells lowered.Lower.capacity before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ());
    let operands = Operands.call program before.State.heap before.State.activation before.State.frames before.State.abstract (I.Keep (G.Call next)) () in
    let successor = Ready.entry_block program operands.Operands.id operands.Operands.entry operands.Operands.code () in
    ghost_ (Ready.capture_capacity program.I.origin.C.origin.Hmc_closure_program.table lowered.Lower.calls lowered.Lower.capacity
        operands.Operands.id operands.Operands.entry ());
    match before.State.block.G.signature.G.temporaries with
    | G.Value (locals, (D.Function (_, result_type) as ty), schema) ->
      let continuation = Edges.next program.I.origin.C.blocks next locals schema (Some result_type) () in
      (match Index.encode lowered.Lower.capacity (H.length operands.Operands.captured) with
      | None -> unreachable_ ()
      | Some capture_count ->
        ghost_ (Geometry.size_represents (Codec.locals_size operands.Operands.entry.K.captured) capture_count ();
          Save.matches_def before.State.block.G.signature next lowered.Lower.capacity call.Block.save;
          Geometry.size_represents (Codec.locals_size before.State.block.G.signature.G.locals) call.Block.environment ();
          Geometry.size_represents (D.add (Codec.locals_size locals) (Codec.temporaries_size schema)) call.Block.saved ();
          Ready.size_add (Codec.locals_size locals) (Codec.temporaries_size schema);
          Block.difference_def lowered.Lower.capacity (2 + call.Block.saved));
        let padding_count = lowered.Lower.capacity - 2 - call.Block.saved in
        let used = (if operands.Operands.entry.K.recursive then 4 else 3) + capture_count in
        let stop : B.u32 = before.State.registers.Registers.frame + 16 + 16 * used in
        let config = Config.config context.State.table_base context.State.stack_base in
        let capture = config.Assembly.locals.Emit.structured.Structured.scratch in
        let slots = config.Assembly.locals.Emit.descriptor in
        let state = {X.memory = before.State.memory; machine = {E.locals = Registers.locals before.State.registers; stack = S.Empty}} in
        ghost_ (Layout.width_def operands.Operands.entry.K.recursive;
          Ready.operand_address before.State.block.G.signature before.State.activation before.State.cells before.State.padding call.Block.environment
            before.State.cell_count before.State.registers.Registers.frame before.State.frame_end ();
          Hmc_wasm_program_step_caller.objects_below before.State.heap before.State.frame_end stop ();
          Hmc_wasm_heap_suffix.objects_above before.State.heap before.State.registers.Registers.stack_limit ();
          Capacity.depth program.I.origin.C.blocks lowered.Lower.width before.State.memory context.State.stack_base before.State.registers.Registers.top before.State.frames ();
          Capacity.ordered lowered.Lower.width (Q.depth before.State.frames) context.State.stack_base before.State.registers.Registers.top ();
          Hmc_wasm_program_descriptors.valid_def program before.State.registers before.State.memory context.State.runtime context.State.table_base context.State.table_count;
          Saved.slots_def program.I.origin.C.blocks; Stack.zero_def ();
          Index.represents_def (D.S (Cap.capacity program.I.origin.C.blocks)) (lowered.Lower.capacity + 1));
        (match Hmc_wasm_reservation.reserve (Saved.slots program.I.origin.C.blocks) (lowered.Lower.capacity + 1) 0 lowered.Lower.width () with
        | None -> unreachable_ ()
        | Some _ ->
        ghost_ (New.failure_def (); Config.config_def context.State.table_base context.State.stack_base;
          Registers.local_values before.State.registers;
          Registers.matches_def (Registers.locals before.State.registers) before.State.registers;
          Wasm_locals.can_set_def (Registers.locals before.State.registers) 11 (S.I32 3); S.same_type_def (S.I32 before.State.registers.Registers.status) (S.I32 3);
          Capture.distinct_def capture; Capture.writable_def capture (Registers.locals before.State.registers);
          Capture.word_slot_def (Registers.locals before.State.registers) 14; Capture.word_slot_def (Registers.locals before.State.registers) 15;
          Capture.word_slot_def (Registers.locals before.State.registers) 16; Capture.word_slot_def (Registers.locals before.State.registers) 17;
          Slots.distinct_def slots 7; Slots.writable_def slots (Registers.locals before.State.registers);
          Slots.limb_slot_def (Registers.locals before.State.registers) 8; Slots.limb_slot_def (Registers.locals before.State.registers) 9;
          Slots.limb_slot_def (Registers.locals before.State.registers) 10;
          Capture.separate_def capture 0; Hmc_wasm_loaded_call.separate_def capture 3 6 0;
          Capture.separate_def capture 3; Hmc_wasm_loaded_call.separate_def capture 3 6 3;
          Capture.separate_def capture 6; Hmc_wasm_loaded_call.separate_def capture 3 6 6;
          Capture.separate_def capture 4; Hmc_wasm_loaded_call.separate_def capture 3 6 4;
          Capture.separate_def capture 7; Hmc_wasm_loaded_call.separate_def capture 3 6 7;
          Capture.separate_def capture 8; Hmc_wasm_loaded_call.separate_def capture 3 6 8;
          Capture.separate_def capture 9; Hmc_wasm_loaded_call.separate_def capture 3 6 9;
          Capture.separate_def capture 10; Hmc_wasm_loaded_call.separate_def capture 3 6 10;
          Hmc_wasm_selected_call_entry.separate_def slots 7 4;
          Hmc_wasm_selected_call_entry.separate_def slots 7 6;
          Hmc_wasm_selected_call_entry.separate_def slots 7 3;
          Hmc_wasm_selected_call_entry.separate_def slots 7 0;
          Hmc_wasm_selected_call_entry.separate_def slots 7 16;
          Hmc_wasm_selected_call_entry.separate_def slots 7 17);
        let result = Case.framed before.State.abstract before.State.cell_count used (State.module_ program lowered context) before.State.registers
          context.State.host_capacity selected.Selection.selected.Selection.index lowered config.Assembly.locals before.State.frames
          context.State.stack_base lowered.Lower.width context.State.stack_capacity before.State.registers.Registers.stack_limit 5 (Round.labels config)
          program.I.origin.C.blocks continuation lowered.Lower.capacity program.I.origin.C.origin.Hmc_closure_program.table before.State.heap
          context.State.runtime context.State.table_base context.State.table_count lowered.Lower.capacity before.State.frame_end
          before.State.block.G.signature before.State.activation before.State.cells before.State.padding locals ty schema next call.Block.environment call.Block.saved
          before.State.pc call.Block.save lowered.Lower.capacity call.Block.padding padding_count call.Block.padding_length state
          before.State.registers.Registers.frame before.State.registers.Registers.top before.State.registers.Registers.heap_limit
          before.State.bytes before.State.suffix 0 4 program operands.Operands.entry operands.Operands.code operands.Operands.id operands.Operands.address
          operands.Operands.captured capture capture_count lowered.Lower.calls 6 lowered.Lower.capacity 7 slots stop 3 globals () in
        let step = result.Case.step in
        match step.Case.body.New.source with
        | New.Stack_exhausted ->
          let after = {before with State.registers = step.Case.registers} in
          ghost_ (New.failure_def ();
            Frame.valid_def before.State.block.G.signature before.State.activation step.Case.registers before.State.memory before.State.frame_end
              before.State.pc before.State.cells before.State.padding before.State.bytes before.State.suffix before.State.cell_count;
            State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
          {Resource.failed_guard = ghost_ step.Case.failed_guard; state = after; fuel = step.Case.fuel; exhausted = Some Machine.Stack}
        | New.Called called ->
          let callee = called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry in
          let saved = called.Call.saved.Saved_body.save.Loaded.saved.Source.view.Slice.saved in
          let after = {before with State.abstract = U.step program before.State.abstract; elapsed = D.S before.State.elapsed;
            activation = callee.Entry.entered; frames = Q.Frame (saved, before.State.frames); block = successor; pc = called.Call.callee.Call_Loaded.pc;
            registers = step.Case.registers; memory = step.Case.body.New.state.X.memory;
            cells = result.Case.cells; padding = result.Case.padding; bytes = result.Case.bytes; suffix = result.Case.suffix} in
          ghost_ (Model.activation_def operands.Operands.entry operands.Operands.code.C.start (Hmc_tagged_cell.Closure_pointer operands.Operands.address)
              before.State.activation.F.accumulator operands.Operands.captured;
            Frame.valid_def (Model.signature operands.Operands.entry) callee.Entry.entered step.Case.registers after.State.memory
              before.State.frame_end after.State.pc after.State.cells after.State.padding after.State.bytes after.State.suffix before.State.cell_count;
            Index.injective (D.S (H.length before.State.cells)) (D.S (H.length after.State.cells)) before.State.cell_count ();
            Progress.advance_next program context.State.input before.State.elapsed;
            State.valid_def program globals lowered context after; State.configuration_def after; State.loop_def context after);
          {Resource.failed_guard = ghost_ step.Case.failed_guard; state = after; fuel = step.Case.fuel; exhausted = None}))
    | _ -> unreachable_ ()
