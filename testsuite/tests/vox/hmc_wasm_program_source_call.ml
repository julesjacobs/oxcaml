module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Save = Hmc_wasm_call_save
module Finish = Hmc_wasm_frame_pad_finish
module Pad = Hmc_wasm_frame_padding
module Slice = Hmc_frame_call_slices
module Write = Wasm_mixed_write
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Source = Hmc_wasm_call_save_source
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Transport = Hmc_wasm_frame_transport
module Saved = Hmc_memory_saved_frame
module Cap = Hmc_frame_capacity
module Loaded = Hmc_wasm_call_save_loaded
module Stack = Hmc_memory_stack
module Q = Hmc_heap_state
module Extent = Hmc_heap_extent
module Body = Hmc_wasm_call_save_push
module Capacity = Hmc_memory_stack_capacity
module Continue = Wasm_control_branch_continue
module T = Wasm_control
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module Model = Hmc_frame_call_entry
module Frame = Hmc_wasm_dynamic_call_frame
module Layout = Hmc_wasm_call_header_layout
module Above = Hmc_heap_image_suffix
module Machine = Hmc_heap_machine
module Plans = Hmc_wasm_call_plan_table
module Select = Hmc_wasm_call_plan_select
module Entry = Hmc_wasm_dynamic_call_entry
module Code = Wasm_code
module Fuel = Wasm_control_compose
module Slots = Hmc_wasm_descriptor_load
module Separate = Hmc_wasm_selected_call_entry
module Enter = Hmc_wasm_call_plan_enter
module Dispatch = Hmc_wasm_call_dispatch
module Capture = Hmc_wasm_cons_capture
module Call_Loaded = Hmc_wasm_loaded_call
module Guarded = Hmc_wasm_call_save_guard
module Stack_entry = Hmc_wasm_loaded_call_stack
module Transition = Hmc_heap_call_transition
module Call = Hmc_wasm_ordinary_call
module Status = Hmc_wasm_program_status
module Complete = Hmc_wasm_program_call_finish
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module New = Hmc_wasm_program_call
module Registers = Hmc_wasm_program_registers
module Config = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module Step = Hmc_wasm_program_register_step
module Dispatcher = Hmc_wasm_program_dispatch
module Func = Wasm_functions
module Calls = Wasm_calls
module GE = Wasm_global_execution
module Preserves = Wasm_control_local_preservation
module Failed = Hmc_failed_guard_model
module Guard = Hmc_failed_guard_calls
module Guard_blocks = Hmc_failed_guard_blocks
type result = {failed_guard : Guard.optional @@ ghost; body : New.result; registers : Registers.registers; fuel : {n : Code.count | not (n === Code.Zero)}}
let (correct @ total) : (module_ : Func.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (host_capacity : Code.count) @ immutable -> (function_index : B.u32) -> (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (frames : Q.frames) @ immutable ->
    (stack_base : B.u32) ->
    (width : B.u32) ->
    (stack_capacity : D.index) @ immutable ->
    (stack_limit : B.u32) ->
    (limit_local : B.u32) ->
    (outer : T.labels) @ immutable ->
    (blocks : G.table) @ immutable ->
    (block : G.block) @ immutable ->
    (stored_capacity : R.count) ->
    (table : K.table) @ immutable ->
    (heap : H.heap) @ immutable ->
    (runtime : Runtime.table) @ immutable ->
    (table_base : B.u32) ->
    (table_count : Table.count) ->
    (frame_count : R.count) ->
    (frame_stop : B.u32) ->
    (signature : G.signature) @ immutable ->
    (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable ->
    (old_padding : H.cells) @ immutable ->
    (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable ->
    (schema : G.temporaries) @ immutable ->
    (next : D.index) @ immutable ->
    (env_count : R.count) ->
    (count : R.count) ->
    (old_pc : B.u32) ->
    (fragment : Save.fragment) @ immutable ->
    (capacity : R.count) ->
    (padding : Write.writes) @ immutable ->
    (padding_count : R.count) ->
    (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable ->
    (source : B.u32) ->
    (base : B.u32) ->
    (limit : B.u32) ->
    (bytes : B.bytes) @ immutable ->
    (suffix : B.bytes) @ immutable ->
    (source_local : B.u32) ->
    (base_local : B.u32) ->
    (program : I.program) @ immutable ->
    (entry : K.entry) @ immutable ->
    (function_ : C.function_entry) @ immutable ->
    (id : D.index) @ immutable ->
    (closure : B.u32) ->
    (captures : H.cells) @ immutable ->
    (capture : Capture.slots) @ immutable ->
    (capture_count : Hmc_wasm_relayout.count) ->
    (plans : Plans.table) @ immutable ->
    (code_local : B.u32) ->
    (call_capacity : Hmc_wasm_relayout.count) ->
    (address_local : B.u32) ->
    (slots : Slots.slots) @ immutable ->
    (callee_stop : B.u32) ->
    (object_local : B.u32) ->
    (globals : Machine.globals) @ immutable ->
    {u : unit | locals === (Config.config table_base stack_base).Assembly.locals
      && outer === Round.labels (Config.config table_base stack_base)
      && state.X.machine.E.locals === Registers.locals registers
      && source = registers.Registers.frame && base = registers.Registers.top && limit = registers.Registers.heap_limit
      && stack_limit = registers.Registers.stack_limit && source <= 4294967280
      && Func.signature module_.Func.signatures (Dispatcher.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (Block.Call {Block.save = fragment; padding; padding_length; saved = count; environment = env_count}) (Config.config table_base stack_base))
      && source_local = locals.Emit.structured.Structured.frame && base_local = locals.Emit.top && limit_local = locals.Emit.stack_limit
      && object_local = locals.Emit.structured.Structured.object_ && code_local = locals.Emit.code && address_local = locals.Emit.address
      && slots === locals.Emit.descriptor && capture === locals.Emit.structured.Structured.scratch
      && plans === lowered.Lower.calls && call_capacity = lowered.Lower.capacity && stored_capacity = lowered.Lower.capacity && width = lowered.Lower.width
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (New.failure ()))
      && locals.Emit.status <> source_local && locals.Emit.status <> base_local && locals.Emit.status <> limit_local && locals.Emit.status <> object_local && locals.Emit.status <> code_local && locals.Emit.status <> address_local && locals.Emit.status <> slots.Slots.start && locals.Emit.status <> slots.Slots.captures && locals.Emit.status <> slots.Slots.recursive && locals.Emit.status <> capture.Capture.head_tag && locals.Emit.status <> capture.Capture.head_payload && locals.Emit.status <> capture.Capture.tail_tag && locals.Emit.status <> capture.Capture.tail_payload
      && blocks === program.I.origin.C.blocks && table === program.I.origin.C.origin.Hmc_closure_program.table
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Call next))
      && base_local <> source_local
      && width > 0 && stack_base <= base && stack_limit <= limit && width = 48 + 16 * count + 16 * padding_count
      && Extent.span (Saved.slots blocks) (Stack.zero ()) width
      && Capacity.region width stack_capacity stack_base stack_limit
      && state.X.machine.E.stack === S.Empty && L.get state.X.machine.E.locals limit_local === Some (S.I32 stack_limit) && Stack.related blocks width state.X.memory stack_base base frames
      && G.lookup blocks next === Some block && block.G.signature.G.locals === context && block.G.signature.G.temporaries === schema
      && Index.represents (Cap.capacity blocks) stored_capacity && stored_capacity = 2 + count + padding_count
      && H.valid table heap && Image.related state.X.memory heap && (H.used heap <= base || Hmc_heap_image_suffix.above heap stack_limit)
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Index.represents (H.length cells) frame_count && frame_stop = source + 16 + 16 * frame_count && frame_stop <= base
      && signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, old_padding)
      && Index.represents activation.F.pc old_pc && Save.matches signature next capacity fragment
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) count
      && 3 + env_count + count <= 268435452 && source + 64 + 16 * env_count + 16 * count <= 4294967296
      && Index.represents padding_length padding_count && 3 + count + padding_count <= 268435452
      && Pad.matches padding (3 + count) padding_length
      && Bounds.covers state.X.memory limit && Bytes.drop state.X.memory source === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Call_Loaded.separate capture object_local code_local base_local
      && Separate.separate slots address_local base_local
      && callee_stop <= stack_base && Stack.related program.I.origin.C.blocks width state.X.memory stack_base base frames
      && Runtime.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= source
      && (match activation.F.temporaries with F.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.F.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && source + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory source === Some bytes
      && object_local <> source_local && object_local <> code_local && code_local <> source_local
      && Capture.distinct capture && Capture.separate capture source_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && Call_Loaded.separate capture object_local code_local address_local && Call_Loaded.separate capture object_local code_local slots.Slots.start
      && Call_Loaded.separate capture object_local code_local slots.Slots.captures && Call_Loaded.separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local source_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && H.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap callee_stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (H.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (H.length captures) capture_count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table call_capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && callee_stop = source + Layout.width entry.K.recursive + 16 * capture_count && callee_stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)} ->
    {out : result | (match out.body.New.source with
      | New.Stack_exhausted -> (match out.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Stack module_
          (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) guard)
        && out.registers === {registers with Registers.status = New.failure ()}
        && Calls.run out.fuel module_ (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) ===
          Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory = state.X.memory;
            machine = {E.locals = S.Empty; stack = S.Push (S.I32 (New.failure ()), S.Empty)}}}
      | New.Called _ -> out.registers === {registers with Registers.top = S.add32 base width; status = Status.zero ()}
        && Calls.run out.fuel module_ (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) ===
          Calls.Running (Dispatcher.loop (Registers.globals out.registers) out.body.New.state.X.memory (Code.Succ host_capacity)))
      && T.run out.body.New.fuel
        {T.code = Emit.emit lowered (Block.Call {Block.save = fragment; padding; padding_length; saved = count; environment = env_count}) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.body.New.state}
      && out.body.New.state.X.machine.E.stack === S.Empty
      && (match out.body.New.source with
      | New.Stack_exhausted -> L.get out.body.New.state.X.machine.E.locals base_local === Some (S.I32 base)
        && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} === Machine.Exhausted Machine.Stack
        && out.body.New.state.X.memory === state.X.memory && L.get out.body.New.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (New.failure ()))
      | New.Called called -> base + width <= stack_limit
        && called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.F.accumulator captures
        && Bytes.drop called.Call.saved.Body.save.Loaded.saved.Source.written.Finish.memory source === Some called.Call.saved.Body.save.Loaded.active.Transport.bytes
        && Wire.decode_cells (D.S (H.length cells)) called.Call.saved.Body.save.Loaded.active.Transport.bytes ===
          Some (H.Cell (V.Word (Header.number old_pc), cells), called.Call.saved.Body.save.Loaded.active.Transport.tail)
        && Bytes.drop out.body.New.state.X.memory callee_stop === Bytes.drop called.Call.saved.Body.save.Loaded.saved.Source.written.Finish.memory callee_stop
        && L.get out.body.New.state.X.machine.E.locals base_local === Some (S.I32 (S.add32 base width))
        && out.body.New.state.X.memory === called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
        && L.get out.body.New.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
        && Wasm_empty_labels.related called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup
          (Status.scope locals.Emit.status outer) called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
        && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
          Machine.Advanced {Machine.heap; state = Q.Running (called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))}
        && Stack.related blocks width called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stack_base (S.add32 base width) (Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))
        && L.get called.Call.callee.Call_Loaded.entry.Dispatch.locals base_local === Some (S.I32 (S.add32 base width))
        && Image.related called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap
        && Table.related runtime called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
        && Bounds.covers called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
        && V.length called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
        && Index.represents function_.C.start called.Call.callee.Call_Loaded.pc
        && Codec.decode (Model.signature entry) function_.C.start called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells ===
          Some (called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, H.Empty)
        && Bytes.drop called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory source ===
          Some called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
        && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number called.Call.callee.Call_Loaded.pc), called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells)))
          called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
          Some (H.Cell (V.Word (Header.number called.Call.callee.Call_Loaded.pc), called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells),
            called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
)} @ immutable =
  fun module_ registers host_capacity function_index lowered locals frames stack_base width stack_capacity stack_limit limit_local outer blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals premise ->
    let body = New.correct lowered locals frames stack_base width stack_capacity stack_limit limit_local outer blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals () in
    let call = {Block.save = fragment; padding; padding_length; saved = count; environment = env_count} in
    ghost_ (Config.config_def table_base stack_base; Registers.local_values registers;
      Registers.exports_def (Registers.locals registers) registers;
      Hmc_wasm_call_locals.ordinary lowered call table_base stack_base 1 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Call call) locals table_base stack_base; labels = outer; state}
        {T.code = T.Empty; labels = outer; state = body.New.state} 1 ();
      Hmc_wasm_call_locals.ordinary lowered call table_base stack_base 12 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Call call) locals table_base stack_base; labels = outer; state}
        {T.code = T.Empty; labels = outer; state = body.New.state} 12 ();
      Hmc_wasm_call_locals.ordinary lowered call table_base stack_base 13 ();
      Preserves.run body.New.fuel
        {T.code = Emit.emit lowered (Block.Call call) locals table_base stack_base; labels = outer; state}
        {T.code = T.Empty; labels = outer; state = body.New.state} 13 ());
    let after = Step.complete lowered (Block.Call call) module_ table_base stack_base registers state.X.memory bytes cells suffix
      host_capacity old_pc function_index body.New.fuel body.New.state () in
    ghost_ (Registers.exports_def body.New.state.X.machine.E.locals after; New.failure_def (); Status.zero_def ());
    let failed_guard = ghost_ (match body.New.source with
      | New.Called _ -> Guard.Absent
      | New.Stack_exhausted ->
        let local = Guard_blocks.call lowered call table_base stack_base registers state.X.memory () in
        Guard.Present (Guard.from_body Failed.Stack lowered (Block.Call call) module_ table_base stack_base
          registers state.X.memory bytes cells suffix host_capacity old_pc function_index local ())) in
    {failed_guard; body; registers = after; fuel = Hmc_wasm_program_cost.dispatch (Round.cost (Config.config table_base stack_base) body.New.fuel) after.Registers.status}

module Extend = Hmc_wasm_frame_extend
module Resources = Hmc_wasm_program_resources
module Descriptors = Hmc_wasm_program_descriptors
module Shared_frame = Hmc_wasm_program_frame
type framed_result = {step : result; cells : H.cells; padding : H.cells; bytes : B.bytes; suffix : B.bytes}
let (framed @ total) : (abstract : Hmc_cfg_semantics.state) @ immutable -> (wire_count : B.u32) -> (used : R.count) -> (module_ : Func.module_) @ immutable -> (registers : Registers.registers) @ immutable ->
    (host_capacity : Code.count) @ immutable -> (function_index : B.u32) -> (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (frames : Q.frames) @ immutable ->
    (stack_base : B.u32) ->
    (width : B.u32) ->
    (stack_capacity : D.index) @ immutable ->
    (stack_limit : B.u32) ->
    (limit_local : B.u32) ->
    (outer : T.labels) @ immutable ->
    (blocks : G.table) @ immutable ->
    (block : G.block) @ immutable ->
    (stored_capacity : R.count) ->
    (table : K.table) @ immutable ->
    (heap : H.heap) @ immutable ->
    (runtime : Runtime.table) @ immutable ->
    (table_base : B.u32) ->
    (table_count : Table.count) ->
    (frame_count : R.count) ->
    (frame_stop : B.u32) ->
    (signature : G.signature) @ immutable ->
    (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable ->
    (old_padding : H.cells) @ immutable ->
    (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable ->
    (schema : G.temporaries) @ immutable ->
    (next : D.index) @ immutable ->
    (env_count : R.count) ->
    (count : R.count) ->
    (old_pc : B.u32) ->
    (fragment : Save.fragment) @ immutable ->
    (capacity : R.count) ->
    (padding : Write.writes) @ immutable ->
    (padding_count : R.count) ->
    (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable ->
    (source : B.u32) ->
    (base : B.u32) ->
    (limit : B.u32) ->
    (bytes : B.bytes) @ immutable ->
    (suffix : B.bytes) @ immutable ->
    (source_local : B.u32) ->
    (base_local : B.u32) ->
    (program : I.program) @ immutable ->
    (entry : K.entry) @ immutable ->
    (function_ : C.function_entry) @ immutable ->
    (id : D.index) @ immutable ->
    (closure : B.u32) ->
    (captures : H.cells) @ immutable ->
    (capture : Capture.slots) @ immutable ->
    (capture_count : Hmc_wasm_relayout.count) ->
    (plans : Plans.table) @ immutable ->
    (code_local : B.u32) ->
    (call_capacity : Hmc_wasm_relayout.count) ->
    (address_local : B.u32) ->
    (slots : Slots.slots) @ immutable ->
    (callee_stop : B.u32) ->
    (object_local : B.u32) ->
    (globals : Machine.globals) @ immutable ->
    {u : unit | Resources.valid program globals width stack_base frame_stop abstract heap activation frames registers state.X.memory
      && Descriptors.valid program registers state.X.memory runtime table_base table_count
      && frame_count = lowered.Lower.capacity && wire_count = frame_count + 1 && source <= 4294967248
      && used = (if entry.K.recursive then 4 + capture_count else 3 + capture_count) && used <= frame_count
      && locals === (Config.config table_base stack_base).Assembly.locals
      && outer === Round.labels (Config.config table_base stack_base)
      && state.X.machine.E.locals === Registers.locals registers
      && source = registers.Registers.frame && base = registers.Registers.top && limit = registers.Registers.heap_limit
      && stack_limit = registers.Registers.stack_limit && source <= 4294967280
      && Func.signature module_.Func.signatures (Dispatcher.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (Block.Call {Block.save = fragment; padding; padding_length; saved = count; environment = env_count}) (Config.config table_base stack_base))
      && source_local = locals.Emit.structured.Structured.frame && base_local = locals.Emit.top && limit_local = locals.Emit.stack_limit
      && object_local = locals.Emit.structured.Structured.object_ && code_local = locals.Emit.code && address_local = locals.Emit.address
      && slots === locals.Emit.descriptor && capture === locals.Emit.structured.Structured.scratch
      && plans === lowered.Lower.calls && call_capacity = lowered.Lower.capacity && stored_capacity = lowered.Lower.capacity && width = lowered.Lower.width
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (New.failure ()))
      && locals.Emit.status <> source_local && locals.Emit.status <> base_local && locals.Emit.status <> limit_local && locals.Emit.status <> object_local && locals.Emit.status <> code_local && locals.Emit.status <> address_local && locals.Emit.status <> slots.Slots.start && locals.Emit.status <> slots.Slots.captures && locals.Emit.status <> slots.Slots.recursive && locals.Emit.status <> capture.Capture.head_tag && locals.Emit.status <> capture.Capture.head_payload && locals.Emit.status <> capture.Capture.tail_tag && locals.Emit.status <> capture.Capture.tail_payload
      && blocks === program.I.origin.C.blocks && table === program.I.origin.C.origin.Hmc_closure_program.table
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Call next))
      && base_local <> source_local
      && width > 0 && stack_base <= base && stack_limit <= limit && width = 48 + 16 * count + 16 * padding_count
      && Extent.span (Saved.slots blocks) (Stack.zero ()) width
      && Capacity.region width stack_capacity stack_base stack_limit
      && state.X.machine.E.stack === S.Empty && L.get state.X.machine.E.locals limit_local === Some (S.I32 stack_limit) && Stack.related blocks width state.X.memory stack_base base frames
      && G.lookup blocks next === Some block && block.G.signature.G.locals === context && block.G.signature.G.temporaries === schema
      && Index.represents (Cap.capacity blocks) stored_capacity && stored_capacity = 2 + count + padding_count
      && H.valid table heap && Image.related state.X.memory heap && (H.used heap <= base || Hmc_heap_image_suffix.above heap stack_limit)
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Index.represents (H.length cells) frame_count && frame_stop = source + 16 + 16 * frame_count && frame_stop <= base
      && signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, old_padding)
      && Index.represents activation.F.pc old_pc && Save.matches signature next capacity fragment
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) count
      && 3 + env_count + count <= 268435452 && source + 64 + 16 * env_count + 16 * count <= 4294967296
      && Index.represents padding_length padding_count && 3 + count + padding_count <= 268435452
      && Pad.matches padding (3 + count) padding_length
      && Bounds.covers state.X.memory limit && Bytes.drop state.X.memory source === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Call_Loaded.separate capture object_local code_local base_local
      && Separate.separate slots address_local base_local
      && callee_stop <= stack_base && Stack.related program.I.origin.C.blocks width state.X.memory stack_base base frames
      && Runtime.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= source
      && (match activation.F.temporaries with F.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.F.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && source + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory source === Some bytes
      && object_local <> source_local && object_local <> code_local && code_local <> source_local
      && Capture.distinct capture && Capture.separate capture source_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && Call_Loaded.separate capture object_local code_local address_local && Call_Loaded.separate capture object_local code_local slots.Slots.start
      && Call_Loaded.separate capture object_local code_local slots.Slots.captures && Call_Loaded.separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local source_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && H.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap callee_stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (H.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (H.length captures) capture_count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table call_capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && callee_stop = source + Layout.width entry.K.recursive + 16 * capture_count && callee_stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)} ->
    {out : framed_result | Descriptors.valid program out.step.registers out.step.body.New.state.X.memory runtime table_base table_count
      && (match out.step.body.New.source with
      | New.Stack_exhausted -> Resources.valid program globals width stack_base frame_stop abstract heap activation frames out.step.registers out.step.body.New.state.X.memory
        && Shared_frame.valid signature activation out.step.registers out.step.body.New.state.X.memory frame_stop old_pc out.cells out.padding out.bytes out.suffix wire_count
      | New.Called called -> Resources.valid program globals width stack_base frame_stop (Hmc_tail_semantics.step program abstract)
          heap called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered (Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames)) out.step.registers out.step.body.New.state.X.memory
        && Shared_frame.valid (Model.signature entry) called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered out.step.registers out.step.body.New.state.X.memory frame_stop called.Call.callee.Call_Loaded.pc
          out.cells out.padding out.bytes out.suffix wire_count)
      && (match out.step.body.New.source with
      | New.Stack_exhausted -> (match out.step.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Stack module_
          (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) guard)
        && out.step.registers === {registers with Registers.status = New.failure ()}
        && Calls.run out.step.fuel module_ (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) ===
          Calls.Finished {GE.globals = Registers.globals out.step.registers; execution = {X.memory = state.X.memory;
            machine = {E.locals = S.Empty; stack = S.Push (S.I32 (New.failure ()), S.Empty)}}}
      | New.Called _ -> out.step.registers === {registers with Registers.top = S.add32 base width; status = Status.zero ()}
        && Calls.run out.step.fuel module_ (Dispatcher.loop (Registers.globals registers) state.X.memory (Code.Succ host_capacity)) ===
          Calls.Running (Dispatcher.loop (Registers.globals out.step.registers) out.step.body.New.state.X.memory (Code.Succ host_capacity)))
      && T.run out.step.body.New.fuel
        {T.code = Emit.emit lowered (Block.Call {Block.save = fragment; padding; padding_length; saved = count; environment = env_count}) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.step.body.New.state}
      && out.step.body.New.state.X.machine.E.stack === S.Empty
      && (match out.step.body.New.source with
      | New.Stack_exhausted -> L.get out.step.body.New.state.X.machine.E.locals base_local === Some (S.I32 base)
        && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} === Machine.Exhausted Machine.Stack
        && out.step.body.New.state.X.memory === state.X.memory && L.get out.step.body.New.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (New.failure ()))
      | New.Called called -> base + width <= stack_limit
        && called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.F.accumulator captures
        && Bytes.drop called.Call.saved.Body.save.Loaded.saved.Source.written.Finish.memory source === Some called.Call.saved.Body.save.Loaded.active.Transport.bytes
        && Wire.decode_cells (D.S (H.length cells)) called.Call.saved.Body.save.Loaded.active.Transport.bytes ===
          Some (H.Cell (V.Word (Header.number old_pc), cells), called.Call.saved.Body.save.Loaded.active.Transport.tail)
        && Bytes.drop out.step.body.New.state.X.memory callee_stop === Bytes.drop called.Call.saved.Body.save.Loaded.saved.Source.written.Finish.memory callee_stop
        && L.get out.step.body.New.state.X.machine.E.locals base_local === Some (S.I32 (S.add32 base width))
        && out.step.body.New.state.X.memory === called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
        && L.get out.step.body.New.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
        && Wasm_empty_labels.related called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup
          (Status.scope locals.Emit.status outer) called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
        && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
          Machine.Advanced {Machine.heap; state = Q.Running (called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))}
        && Stack.related blocks width called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stack_base (S.add32 base width) (Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))
        && L.get called.Call.callee.Call_Loaded.entry.Dispatch.locals base_local === Some (S.I32 (S.add32 base width))
        && Image.related called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap
        && Table.related runtime called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
        && Bounds.covers called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
        && V.length called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
        && Index.represents function_.C.start called.Call.callee.Call_Loaded.pc
        && Codec.decode (Model.signature entry) function_.C.start called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells ===
          Some (called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, H.Empty)
        && Bytes.drop called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory source ===
          Some called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
        && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number called.Call.callee.Call_Loaded.pc), called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells)))
          called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
          Some (H.Cell (V.Word (Header.number called.Call.callee.Call_Loaded.pc), called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells),
            called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
)} @ immutable =
  fun abstract wire_count used module_ registers host_capacity function_index lowered locals frames stack_base width stack_capacity stack_limit limit_local outer blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals premise ->
    let step = correct module_ registers host_capacity function_index lowered locals frames stack_base width stack_capacity stack_limit limit_local outer blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals () in
    ghost_ (Resources.valid_def program globals width stack_base frame_stop abstract heap activation frames registers state.X.memory;
      Descriptors.valid_def program registers state.X.memory runtime table_base table_count;
      Descriptors.valid_def program step.registers step.body.New.state.X.memory runtime table_base table_count);
    match step.body.New.source with
    | New.Stack_exhausted ->
      ghost_ (Resources.valid_def program globals width stack_base frame_stop abstract heap activation frames step.registers step.body.New.state.X.memory;
        Index.represents_def (D.S (H.length cells)) wire_count;
        Shared_frame.valid_def signature activation step.registers step.body.New.state.X.memory frame_stop old_pc cells old_padding bytes suffix wire_count);
      {step; cells; padding = old_padding; bytes; suffix}
    | New.Called called ->
      let callee = called.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry in
      let saved = called.Call.saved.Body.save.Loaded.saved.Source.written.Finish.memory in
      let active = called.Call.saved.Body.save.Loaded.active in
      ghost_ (Model.activation_def entry function_.C.start (V.Closure_pointer closure) activation.F.accumulator captures;
        Extend.callee_size entry function_.C.start called.Call.callee.Call_Loaded.pc closure activation.F.accumulator captures callee.Entry.cells capture_count used ();
        Layout.width_def entry.K.recursive;
        H.length_def (H.Cell (V.Word (Header.number called.Call.callee.Call_Loaded.pc), callee.Entry.cells)));
      let full = Extend.correct saved step.body.New.state.X.memory source callee_stop old_pc called.Call.callee.Call_Loaded.pc cells callee.Entry.cells frame_count used
        active.Transport.bytes callee.Entry.frame.Frame.bytes active.Transport.tail callee.Entry.frame.Frame.suffix (Model.signature entry) callee.Entry.entered () in
      ghost_ (Hmc_heap_invariant.step program globals registers.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} abstract ();
        S.add32_def base width;
        Bounds.same_length state.X.memory step.body.New.state.X.memory registers.Registers.heap_limit ();
        Bounds.same_length state.X.memory step.body.New.state.X.memory registers.Registers.stack_limit ();
        Resources.valid_def program globals width stack_base frame_stop (Hmc_tail_semantics.step program abstract)
          heap callee.Entry.entered (Q.Frame (called.Call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames)) step.registers step.body.New.state.X.memory;
        Index.represents_def (D.S (H.length full.Extend.cells)) wire_count;
        Shared_frame.valid_def (Model.signature entry) callee.Entry.entered step.registers step.body.New.state.X.memory frame_stop called.Call.callee.Call_Loaded.pc
          full.Extend.cells full.Extend.padding callee.Entry.frame.Frame.bytes active.Transport.tail wire_count);
      {step; cells = full.Extend.cells; padding = full.Extend.padding; bytes = callee.Entry.frame.Frame.bytes; suffix = active.Transport.tail}
