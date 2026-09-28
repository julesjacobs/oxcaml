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
let[@def] (emit @ total) (plans : Plans.table @ immutable) (table_base : B.u32) (code_local : B.u32)
    (address_local : B.u32) (slots : Slots.slots @ immutable) (object_local : B.u32) (frame_local : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  Lift.embed (Descriptor_select.emit table_base code_local address_local slots)
    (Select.emit plans code_local slots.Slots.start object_local frame_local argument_tag argument_payload)
type result = {call : Enter.result; locals : S.stack; fuel : Code.count}
let (correct_preserving @ total) : (keep : B.u32) -> (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (argument : V.value) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code : Table.count) -> (code_local : B.u32) -> (labels : T.labels) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (address_local : B.u32) -> (slots : Slots.slots) @ immutable -> (descriptor : Descriptor.descriptor) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | Separate.separate slots address_local keep
      && Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Descriptor.lookup runtime code === Some descriptor
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local frame_local && Separate.separate slots address_local argument_tag
      && Separate.separate slots address_local argument_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && Index.represents id code && L.get state.X.machine.E.locals code_local === Some (S.I32 code)
      && state.X.machine.E.stack === S.Empty
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | Wasm_empty_labels.related out.call.Enter.selected.Select.cleanup labels out.call.Enter.selected.Select.selected.T.labels
      && L.get out.locals keep === L.get state.X.machine.E.locals keep
      && Machine.invoke program heap (V.Closure_pointer closure) argument === Some out.call.Enter.entry.Entry.entered
      && Table.related runtime out.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.call.Enter.entry.Entry.frame.Frame.memory
      && Image.related out.call.Enter.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.call.Enter.entry.Entry.frame.Frame.memory limit
      && V.length out.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures
      && Codec.decode (Model.signature entry) function_.C.start out.call.Enter.entry.Entry.cells === Some (out.call.Enter.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.call.Enter.entry.Entry.frame.Frame.memory base === Some out.call.Enter.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number descriptor.Descriptor.start), out.call.Enter.entry.Entry.cells))) out.call.Enter.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number descriptor.Descriptor.start), out.call.Enter.entry.Entry.cells), out.call.Enter.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.call.Enter.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && T.run out.fuel
        {T.code = emit plans table_base code_local address_local slots object_local frame_local argument_tag argument_payload; labels; state}
        === T.Running {T.code = T.Empty; labels = out.call.Enter.selected.Select.selected.T.labels;
          state = {X.memory = out.call.Enter.entry.Entry.frame.Frame.memory; machine = {E.locals = out.locals; stack = S.Empty}}}} @ immutable =
  fun keep program heap entry function_ id closure captures argument count plans code code_local labels capacity runtime table_base table_count address_local slots descriptor state base stop limit object_local frame_local argument_tag argument_payload premise ->
    let selected = Descriptor_select.correct program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      id code descriptor state table_base table_count code_local address_local slots () in
    let middle = {X.memory = state.X.memory; machine = {E.locals = selected.Descriptor_select.locals; stack = S.Empty}} in
    ghost_ (Separate.separate_def slots address_local keep;
      Preserve.correct table_base code_local address_local slots state middle keep ();
      Separate.separate_def slots address_local code_local; Separate.separate_def slots address_local object_local;
      Separate.separate_def slots address_local frame_local; Separate.separate_def slots address_local argument_tag;
      Separate.separate_def slots address_local argument_payload;
      Preserve.correct table_base code_local address_local slots state middle code_local ();
      Preserve.correct table_base code_local address_local slots state middle object_local ();
      Preserve.correct table_base code_local address_local slots state middle frame_local ();
      Preserve.correct table_base code_local address_local slots state middle argument_tag ();
      Preserve.correct table_base code_local address_local slots state middle argument_payload ());
    let call = Enter.correct program heap entry function_ id closure captures argument count plans code code_local labels capacity
      descriptor.Descriptor.start slots.Slots.start middle base stop limit object_local frame_local argument_tag argument_payload () in
    let setup = Descriptor_select.emit table_base code_local address_local slots in
    let body = Frame.emit call.Enter.selected.Select.fragment slots.Slots.start object_local frame_local argument_tag argument_payload in
    let remaining = Fuel.add (Local_select.cost (Plans.prepare plans slots.Slots.start object_local frame_local argument_tag argument_payload) code code_local)
      (Code.length body) in
    let fuel = Fuel.add (Code.length setup) remaining in
    ghost_ (Table.preserve runtime state.X.memory call.Enter.entry.Entry.frame.Frame.memory table_base table_count base ();
      Wasm_control_success.straight setup state middle ();
      Lift.correct setup (Select.emit plans code_local slots.Slots.start object_local frame_local argument_tag argument_payload) labels state middle ();
      Fuel.correct (Code.length setup) remaining {T.code = emit plans table_base code_local address_local slots object_local frame_local argument_tag argument_payload; labels; state};
      emit_def plans table_base code_local address_local slots object_local frame_local argument_tag argument_payload);
    {call; locals = selected.Descriptor_select.locals; fuel}

let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (argument : V.value) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code : Table.count) -> (code_local : B.u32) -> (labels : T.labels) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (address_local : B.u32) -> (slots : Slots.slots) @ immutable -> (descriptor : Descriptor.descriptor) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Descriptor.lookup runtime code === Some descriptor
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local frame_local && Separate.separate slots address_local argument_tag
      && Separate.separate slots address_local argument_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && Index.represents id code && L.get state.X.machine.E.locals code_local === Some (S.I32 code)
      && state.X.machine.E.stack === S.Empty
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | Wasm_empty_labels.related out.call.Enter.selected.Select.cleanup labels out.call.Enter.selected.Select.selected.T.labels
      && Machine.invoke program heap (V.Closure_pointer closure) argument === Some out.call.Enter.entry.Entry.entered
      && Table.related runtime out.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.call.Enter.entry.Entry.frame.Frame.memory
      && Image.related out.call.Enter.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.call.Enter.entry.Entry.frame.Frame.memory limit
      && V.length out.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures
      && Codec.decode (Model.signature entry) function_.C.start out.call.Enter.entry.Entry.cells === Some (out.call.Enter.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.call.Enter.entry.Entry.frame.Frame.memory base === Some out.call.Enter.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number descriptor.Descriptor.start), out.call.Enter.entry.Entry.cells))) out.call.Enter.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number descriptor.Descriptor.start), out.call.Enter.entry.Entry.cells), out.call.Enter.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.call.Enter.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && T.run out.fuel
        {T.code = emit plans table_base code_local address_local slots object_local frame_local argument_tag argument_payload; labels; state}
        === T.Running {T.code = T.Empty; labels = out.call.Enter.selected.Select.selected.T.labels;
          state = {X.memory = out.call.Enter.entry.Entry.frame.Frame.memory; machine = {E.locals = out.locals; stack = S.Empty}}}} @ immutable =
  fun program heap entry function_ id closure captures argument count plans code code_local labels capacity runtime table_base table_count address_local slots descriptor state base stop limit object_local frame_local argument_tag argument_payload premise ->
    correct_preserving frame_local program heap entry function_ id closure captures argument count plans code code_local labels capacity runtime table_base table_count address_local slots descriptor state base stop limit object_local frame_local argument_tag argument_payload ()
