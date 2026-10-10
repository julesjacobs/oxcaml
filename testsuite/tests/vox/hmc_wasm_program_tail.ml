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
let[@def] (separate @ total) (capture : Capture.slots @ immutable) (object_local : B.u32) (code_local : B.u32) (local : B.u32) =
  Capture.separate capture local && object_local <> local && code_local <> local
type result = {source : Loaded.result; state : X.state; fuel : Code.count}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable ->
    (frames : Q.frames) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (outer : T.labels) @ immutable -> (stack_base : B.u32) -> (stack_top : B.u32) -> (keep : B.u32) -> (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (signature : G.signature) @ immutable -> (activation : Source.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (env_count : Position.slot) ->
    (capture : Capture.slots) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code_local : B.u32) -> (capacity : Hmc_wasm_relayout.count) -> (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (address_local : B.u32) -> (slots : Slots.slots) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) ->
    {u : unit | Stack.related program.I.origin.C.blocks lowered.Lower.width state.X.memory stack_base stack_top frames
      && stop <= stack_base && L.get state.X.machine.E.locals keep === Some (S.I32 stack_top)
      && lowered.Lower.calls === plans && lowered.Lower.capacity = capacity
      && locals.Emit.structured.Structured.frame = frame_local && locals.Emit.structured.Structured.object_ = object_local
      && locals.Emit.structured.Structured.scratch === capture && locals.Emit.code = code_local
      && locals.Emit.address = address_local && locals.Emit.descriptor === slots
      && locals.Emit.status <> keep && locals.Emit.status <> frame_local
      && separate capture object_local code_local locals.Emit.status && Separate.separate slots address_local locals.Emit.status
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ()))
      && I.lookup program.I.code activation.Source.pc === Some I.Tail_call
      && Separate.separate slots address_local keep
      && separate capture object_local code_local keep
      && Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Codec.decode signature activation.Source.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Source.temporaries with Source.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Source.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && object_local <> frame_local && object_local <> code_local && code_local <> frame_local
      && Capture.distinct capture && Capture.separate capture frame_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && separate capture object_local code_local address_local && separate capture object_local code_local slots.Slots.start
      && separate capture object_local code_local slots.Slots.captures && separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local frame_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
} ->
    {out : result | Wasm_empty_labels.related out.source.Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup (Status.scope locals.Emit.status outer) out.source.Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
      && L.get out.source.Loaded.entry.Dispatch.locals keep === L.get state.X.machine.E.locals keep
      && Index.represents function_.C.start out.source.Loaded.pc
      && Machine.invoke program heap (V.Closure_pointer closure) activation.Source.accumulator === Some out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.entered
      && Table.related runtime out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
      && Image.related out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
      && V.length out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures
      && Codec.decode (Model.signature entry) function_.C.start out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.cells === Some (out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory base === Some out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number out.source.Loaded.pc), out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.cells)))
        out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number out.source.Loaded.pc), out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.cells), out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && out.state.X.memory === out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
      && Stack.related program.I.origin.C.blocks lowered.Lower.width out.state.X.memory stack_base stack_top frames
      && out.state.X.machine.E.stack === S.Empty
      && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && L.get out.state.X.machine.E.locals keep === L.get state.X.machine.E.locals keep
      && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.source.Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, frames)}
      && T.run out.fuel {T.code = Emit.emit lowered (Block.Tail_call env_count) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.state}} @ immutable =
  fun lowered locals frames globals stack_capacity outer stack_base stack_top keep program heap entry function_ id closure captures signature activation context ty schema cells padding old_pc env_count capture bytes suffix count plans code_local capacity runtime table_base table_count address_local slots state base stop limit object_local frame_local premise ->
    let local = locals.Emit.status in
    let body = Loaded.emit env_count capture plans table_base code_local address_local slots object_local frame_local in
    let prepared = Status.prepare local (Status.zero ()) body outer state () in
    ghost_ (Status.zero_def (); separate_def capture object_local code_local local; Capture.separate_def capture local;
      Separate.separate_def slots address_local local;
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals keep ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals frame_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals object_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals code_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals address_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals slots.Slots.start ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals slots.Slots.captures ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals slots.Slots.recursive ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals capture.Capture.head_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals capture.Capture.head_payload ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals capture.Capture.tail_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (Status.zero ())) prepared.X.machine.E.locals capture.Capture.tail_payload ();
      Capture.writable_def capture state.X.machine.E.locals; Slots.writable_def slots state.X.machine.E.locals;
      Capture.word_slot_def state.X.machine.E.locals capture.Capture.head_tag;
      Capture.word_slot_def state.X.machine.E.locals capture.Capture.head_payload;
      Capture.word_slot_def state.X.machine.E.locals capture.Capture.tail_tag;
      Capture.word_slot_def state.X.machine.E.locals capture.Capture.tail_payload;
      Slots.limb_slot_def state.X.machine.E.locals slots.Slots.start;
      Slots.limb_slot_def state.X.machine.E.locals slots.Slots.captures;
      Slots.limb_slot_def state.X.machine.E.locals slots.Slots.recursive;
      Capture.writable_def capture prepared.X.machine.E.locals; Slots.writable_def slots prepared.X.machine.E.locals;
      Capture.word_slot_def prepared.X.machine.E.locals capture.Capture.head_tag;
      Capture.word_slot_def prepared.X.machine.E.locals capture.Capture.head_payload;
      Capture.word_slot_def prepared.X.machine.E.locals capture.Capture.tail_tag;
      Capture.word_slot_def prepared.X.machine.E.locals capture.Capture.tail_payload;
      Slots.limb_slot_def prepared.X.machine.E.locals slots.Slots.start;
      Slots.limb_slot_def prepared.X.machine.E.locals slots.Slots.captures;
      Slots.limb_slot_def prepared.X.machine.E.locals slots.Slots.recursive;
      Loaded.separate_def capture object_local code_local keep;
      Loaded.separate_def capture object_local code_local address_local;
      Loaded.separate_def capture object_local code_local slots.Slots.start;
      Loaded.separate_def capture object_local code_local slots.Slots.captures;
      Loaded.separate_def capture object_local code_local slots.Slots.recursive;
      separate_def capture object_local code_local keep;
      separate_def capture object_local code_local address_local;
      separate_def capture object_local code_local slots.Slots.start;
      separate_def capture object_local code_local slots.Slots.captures;
      separate_def capture object_local code_local slots.Slots.recursive;
      Emit.emit_def lowered (Block.Tail_call env_count) locals table_base stack_base);
    let source = Loaded.correct_preserving keep program heap entry function_ id closure captures signature activation context ty schema cells padding old_pc env_count capture bytes suffix count plans code_local (Status.scope local outer) capacity runtime table_base table_count address_local slots prepared base stop limit object_local frame_local () in
    let after_body = {X.memory = source.Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory;
      machine = {E.locals = source.Loaded.entry.Dispatch.locals; stack = S.Empty}} in
    let cleanup = source.Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup in
    let inner = source.Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels in
    let after = Complete.correct local (Status.zero ()) body outer state prepared after_body inner source.Loaded.fuel cleanup () in
    ghost_ (let _ = Bounds.suffix state.X.memory limit stop () in
      let _ = Bounds.suffix after.X.memory limit stop () in
      Bounds.covers_def state.X.memory stop; Bounds.covers_def after.X.memory stop;
      Stack.preserve_suffix program.I.origin.C.blocks lowered.Lower.width state.X.memory after.X.memory stack_base stack_top frames stop ();
      L.other_local after_body.X.machine.E.locals local (S.I32 (Status.zero ())) after.X.machine.E.locals keep ();
      (match activation.Source.temporaries with
      | Source.Value (value, env, rest) -> Transition.tail program globals heap limit stack_capacity activation frames value env rest source.Loaded.entry.Dispatch.call.Enter.entry.Entry.entered ()
      | _ -> ()));
    {source; state = after; fuel = Status.cost (Fuel.add source.Loaded.fuel cleanup)}
