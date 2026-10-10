module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Model = Hmc_frame_call_entry
module Decode = Hmc_frame_call_decode
module Copy = Hmc_wasm_call_captures
module Frame = Hmc_wasm_dynamic_call_frame
module Layout = Hmc_wasm_call_header_layout
module Read = Hmc_wasm_closure_read
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Plans = Hmc_wasm_call_plan_table
module Select = Hmc_wasm_call_plan_select
module Local_select = Wasm_local_select
module Entry = Hmc_wasm_dynamic_call_entry
module T = Wasm_control
module Code = Wasm_code
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Descriptor = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Slots = Hmc_wasm_descriptor_load
module Descriptor_select = Hmc_wasm_descriptor_select
module Preserve = Hmc_wasm_descriptor_preserve
module Separate = Hmc_wasm_selected_call_entry
module Enter = Hmc_wasm_call_plan_enter
module Dispatch = Hmc_wasm_call_dispatch
module Target = Hmc_wasm_call_target
module Target_preserve = Hmc_wasm_call_target_preserve
module Bound = Hmc_runtime_descriptor_bound
module Capture = Hmc_wasm_cons_capture
module Source = Hmc_heap_frame
module G = Hmc_cfg_ir
module Position = Hmc_wasm_simple_lower
module Loaded = Hmc_wasm_loaded_call
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Structured = Hmc_wasm_structured_block
module Block = Hmc_wasm_program_block
module Status = Hmc_wasm_program_status
module Complete = Hmc_wasm_program_call_finish
module Q = Hmc_heap_state
module Stack = Hmc_memory_stack
module Bounds = Hmc_linear_bounds
module Transition = Hmc_heap_call_transition
module New = Hmc_wasm_program_tail
module Registers = Hmc_wasm_program_registers
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module Dispatcher = Hmc_wasm_program_dispatch
module Step = Hmc_wasm_program_register_step
module Func = Wasm_functions
module Calls = Wasm_calls
module Preserves = Wasm_control_local_preservation
type result = {callee : Entry.result; pc : B.u32; registers : Registers.registers; fuel : {n : Code.count | not (n === Code.Zero)}}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (frames : Q.frames) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (stack_base : B.u32) -> (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (signature : G.signature) @ immutable -> (activation : Source.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (env_count : Position.slot) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (stop : B.u32) ->
    (host_capacity : Code.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | Stack.related program.I.origin.C.blocks lowered.Lower.width memory stack_base before.Registers.top frames
      && stop <= stack_base && before.Registers.frame <= 4294967280
      && I.lookup program.I.code activation.Source.pc === Some I.Tail_call
      && Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime memory table_base table_count && table_base + 32 * table_count <= before.Registers.frame
      && Codec.decode signature activation.Source.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Source.temporaries with Source.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Source.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && before.Registers.frame + 48 + 16 * env_count <= 4294967280
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table lowered.Lower.capacity lowered.Lower.calls
      && stop = before.Registers.frame + Layout.width entry.K.recursive + 16 * count && stop <= before.Registers.heap_limit
      && Bounds.covers memory before.Registers.heap_limit
      && Func.signature module_.Func.signatures (Dispatcher.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (Block.Tail_call env_count) (Runtime.config table_base stack_base))} ->
    {out : result | out.registers === {before with Registers.status = Status.zero ()}
      && Index.represents function_.C.start out.pc
      && Machine.invoke program heap (V.Closure_pointer closure) activation.Source.accumulator === Some out.callee.Entry.entered
      && out.callee.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.callee.Entry.entered, frames)}
      && Table.related runtime out.callee.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.callee.Entry.frame.Frame.memory
      && Image.related out.callee.Entry.frame.Frame.memory heap && Bounds.covers out.callee.Entry.frame.Frame.memory before.Registers.heap_limit
      && V.length out.callee.Entry.frame.Frame.memory === V.length memory
      && Codec.decode (Model.signature entry) function_.C.start out.callee.Entry.cells === Some (out.callee.Entry.entered, Heap.Empty)
      && Bytes.drop out.callee.Entry.frame.Frame.memory before.Registers.frame === Some out.callee.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number out.pc), out.callee.Entry.cells))) out.callee.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number out.pc), out.callee.Entry.cells), out.callee.Entry.frame.Frame.suffix)
      && Bytes.drop out.callee.Entry.frame.Frame.memory stop === Bytes.drop memory stop
      && Stack.related program.I.origin.C.blocks lowered.Lower.width out.callee.Entry.frame.Frame.memory stack_base before.Registers.top frames
      && Calls.run out.fuel module_ (Dispatcher.loop (Registers.globals before) memory (Code.Succ host_capacity)) ===
        Calls.Running (Dispatcher.loop (Registers.globals out.registers) out.callee.Entry.frame.Frame.memory (Code.Succ host_capacity))} @ immutable =
  fun lowered module_ frames globals stack_capacity stack_base program heap entry function_ id closure captures signature activation context ty schema
      cells padding old_pc env_count bytes suffix count runtime table_base table_count before memory stop host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let locals = config.Assembly.locals in
    let capture = locals.Emit.structured.Structured.scratch in
    let slots = locals.Emit.descriptor in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      Status.zero_def (); L.can_set_def (Registers.locals before) 11 (S.I32 (Status.zero ())); S.same_type_def (S.I32 before.Registers.status) (S.I32 (Status.zero ()));
      Capture.distinct_def capture; Capture.writable_def capture (Registers.locals before);
      Capture.word_slot_def (Registers.locals before) 14; Capture.word_slot_def (Registers.locals before) 15;
      Capture.word_slot_def (Registers.locals before) 16; Capture.word_slot_def (Registers.locals before) 17;
      Slots.distinct_def slots 7; Slots.writable_def slots (Registers.locals before);
      Slots.limb_slot_def (Registers.locals before) 8; Slots.limb_slot_def (Registers.locals before) 9; Slots.limb_slot_def (Registers.locals before) 10;
      Capture.separate_def capture 0; New.separate_def capture 3 6 0;
      Capture.separate_def capture 3; New.separate_def capture 3 6 3;
      Capture.separate_def capture 6; New.separate_def capture 3 6 6;
      Capture.separate_def capture 11; New.separate_def capture 3 6 11;
      Capture.separate_def capture 4; New.separate_def capture 3 6 4;
      Capture.separate_def capture 7; New.separate_def capture 3 6 7;
      Capture.separate_def capture 8; New.separate_def capture 3 6 8;
      Capture.separate_def capture 9; New.separate_def capture 3 6 9;
      Capture.separate_def capture 10; New.separate_def capture 3 6 10;
      Separate.separate_def slots 7 11;
      Separate.separate_def slots 7 4;
      Separate.separate_def slots 7 6;
      Separate.separate_def slots 7 3;
      Separate.separate_def slots 7 0;
      Separate.separate_def slots 7 16;
      Separate.separate_def slots 7 17);
    let body = New.correct lowered locals frames globals stack_capacity (Round.labels config) stack_base before.Registers.top 4 program heap entry function_ id closure captures
      signature activation context ty schema cells padding old_pc env_count capture bytes suffix count lowered.Lower.calls 6 lowered.Lower.capacity runtime table_base table_count
      7 slots state before.Registers.frame stop before.Registers.heap_limit 3 0 () in
    ghost_ (Hmc_wasm_call_locals.tail lowered env_count table_base stack_base 1 ();
      Hmc_wasm_call_locals.tail lowered env_count table_base stack_base 12 ();
      Hmc_wasm_call_locals.tail lowered env_count table_base stack_base 13 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Tail_call env_count) locals table_base stack_base; labels = Round.labels config; state}
        {T.code = T.Empty; labels = Round.labels config; state = body.New.state} 1 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Tail_call env_count) locals table_base stack_base; labels = Round.labels config; state}
        {T.code = T.Empty; labels = Round.labels config; state = body.New.state} 12 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Tail_call env_count) locals table_base stack_base; labels = Round.labels config; state}
        {T.code = T.Empty; labels = Round.labels config; state = body.New.state} 13 ());
    let registers = Step.complete lowered (Block.Tail_call env_count) module_ table_base stack_base before memory bytes cells suffix host_capacity old_pc function_index body.New.fuel body.New.state () in
    ghost_ (Registers.exports_def body.New.state.X.machine.E.locals registers);
    {callee = body.New.source.Loaded.entry.Dispatch.call.Enter.entry; pc = body.New.source.Loaded.pc;
      registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.New.fuel) registers.Registers.status}

type framed_result = {step : result; frame : Hmc_wasm_frame_extend.result}
let (framed @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (frames : Q.frames) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (stack_base : B.u32) -> (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (signature : G.signature) @ immutable -> (activation : Source.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (env_count : Position.slot) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (stop : B.u32) ->
    (host_capacity : Code.count) @ immutable -> (function_index : B.u32) ->
    (used : Hmc_wasm_relayout.count) -> (frame_end : B.u32) -> (cell_count : B.u32) -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    {u : unit | Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end abstract heap activation frames before memory
      && Hmc_wasm_program_frame.valid signature activation before memory frame_end old_pc cells padding bytes suffix cell_count
      && Hmc_wasm_program_descriptors.valid program before memory runtime table_base table_count
      && Index.represents (Heap.length cells) lowered.Lower.capacity
      && used = (if entry.K.recursive then 4 + count else 3 + count) && used <= lowered.Lower.capacity
      && frame_end = before.Registers.frame + 16 + 16 * lowered.Lower.capacity
      && Stack.related program.I.origin.C.blocks lowered.Lower.width memory stack_base before.Registers.top frames
      && stop <= stack_base && before.Registers.frame <= 4294967280
      && I.lookup program.I.code activation.Source.pc === Some I.Tail_call
      && Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime memory table_base table_count && table_base + 32 * table_count <= before.Registers.frame
      && Codec.decode signature activation.Source.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Source.temporaries with Source.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Source.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && before.Registers.frame + 48 + 16 * env_count <= 4294967280
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table lowered.Lower.capacity lowered.Lower.calls
      && stop = before.Registers.frame + Layout.width entry.K.recursive + 16 * count && stop <= before.Registers.heap_limit
      && Bounds.covers memory before.Registers.heap_limit
      && Func.signature module_.Func.signatures (Dispatcher.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (Block.Tail_call env_count) (Runtime.config table_base stack_base))} ->
    {out : framed_result |
      Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end (Hmc_tail_semantics.step program abstract)
        heap out.step.callee.Entry.entered frames out.step.registers out.step.callee.Entry.frame.Frame.memory
      && Hmc_wasm_program_descriptors.valid program out.step.registers out.step.callee.Entry.frame.Frame.memory runtime table_base table_count
      && Hmc_wasm_program_frame.valid (Model.signature entry) out.step.callee.Entry.entered out.step.registers out.step.callee.Entry.frame.Frame.memory
        frame_end out.step.pc out.frame.Hmc_wasm_frame_extend.cells out.frame.Hmc_wasm_frame_extend.padding out.step.callee.Entry.frame.Frame.bytes suffix cell_count
      && Heap.length out.frame.Hmc_wasm_frame_extend.cells === Heap.length cells
      &&  out.step.registers === {before with Registers.status = Status.zero ()}
      && Index.represents function_.C.start out.step.pc
      && Machine.invoke program heap (V.Closure_pointer closure) activation.Source.accumulator === Some out.step.callee.Entry.entered
      && out.step.callee.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.step.callee.Entry.entered, frames)}
      && Table.related runtime out.step.callee.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.step.callee.Entry.frame.Frame.memory
      && Image.related out.step.callee.Entry.frame.Frame.memory heap && Bounds.covers out.step.callee.Entry.frame.Frame.memory before.Registers.heap_limit
      && V.length out.step.callee.Entry.frame.Frame.memory === V.length memory
      && Codec.decode (Model.signature entry) function_.C.start out.step.callee.Entry.cells === Some (out.step.callee.Entry.entered, Heap.Empty)
      && Bytes.drop out.step.callee.Entry.frame.Frame.memory before.Registers.frame === Some out.step.callee.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number out.step.pc), out.step.callee.Entry.cells))) out.step.callee.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number out.step.pc), out.step.callee.Entry.cells), out.step.callee.Entry.frame.Frame.suffix)
      && Bytes.drop out.step.callee.Entry.frame.Frame.memory stop === Bytes.drop memory stop
      && Stack.related program.I.origin.C.blocks lowered.Lower.width out.step.callee.Entry.frame.Frame.memory stack_base before.Registers.top frames
      && Calls.run out.step.fuel module_ (Dispatcher.loop (Registers.globals before) memory (Code.Succ host_capacity)) ===
        Calls.Running (Dispatcher.loop (Registers.globals out.step.registers) out.step.callee.Entry.frame.Frame.memory (Code.Succ host_capacity))} @ immutable =
  fun lowered module_ frames globals stack_capacity stack_base program heap entry function_ id closure captures signature activation context ty schema
      cells padding old_pc env_count bytes suffix count runtime table_base table_count before memory stop host_capacity function_index used frame_end cell_count abstract premise ->
    let step = correct lowered module_ frames globals stack_capacity stack_base program heap entry function_ id closure captures signature activation context ty schema
      cells padding old_pc env_count bytes suffix count runtime table_base table_count before memory stop host_capacity function_index () in
    ghost_ (Model.activation_def entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures;
      Hmc_wasm_frame_extend.callee_size entry function_.C.start step.pc closure activation.Source.accumulator captures step.callee.Entry.cells count used ();
      Layout.width_def entry.K.recursive; Heap.length_def (Heap.Cell (V.Word (Header.number step.pc), step.callee.Entry.cells)));
    let frame = Hmc_wasm_frame_extend.correct memory step.callee.Entry.frame.Frame.memory before.Registers.frame stop old_pc step.pc cells step.callee.Entry.cells
      lowered.Lower.capacity used bytes step.callee.Entry.frame.Frame.bytes suffix step.callee.Entry.frame.Frame.suffix (Model.signature entry) step.callee.Entry.entered () in
    ghost_ (
      Hmc_wasm_program_frame.valid_def signature activation before memory frame_end old_pc cells padding bytes suffix cell_count;
      let old_full = Heap.Cell (V.Word (Header.number old_pc), cells) in
      let new_full = Heap.Cell (V.Word (Header.number step.pc), frame.Hmc_wasm_frame_extend.cells) in
      Heap.length_def old_full; Heap.length_def new_full;
      Hmc_wasm_frame_preservation.correct memory step.callee.Entry.frame.Frame.memory before.Registers.frame frame_end bytes step.callee.Entry.frame.Frame.bytes old_full new_full suffix cell_count ();
      Hmc_wasm_program_resources.preserve program globals lowered.Lower.width stack_base frame_end abstract heap activation step.callee.Entry.entered frames
        stack_capacity before step.registers memory step.callee.Entry.frame.Frame.memory ();
      Hmc_wasm_program_descriptors.preserve program before step.registers memory step.callee.Entry.frame.Frame.memory runtime table_base table_count ();
      Hmc_wasm_program_frame.valid_def (Model.signature entry) step.callee.Entry.entered step.registers step.callee.Entry.frame.Frame.memory frame_end step.pc
        frame.Hmc_wasm_frame_extend.cells frame.Hmc_wasm_frame_extend.padding step.callee.Entry.frame.Frame.bytes suffix cell_count);
    {step; frame}
