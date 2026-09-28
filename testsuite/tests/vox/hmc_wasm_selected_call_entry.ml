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
module R = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Load = Hmc_wasm_descriptor_load
module Select = Hmc_wasm_descriptor_select
module Preserve = Hmc_wasm_descriptor_preserve
module Entry = Hmc_wasm_dynamic_call_entry
let[@def] (separate @ total) (slots : Load.slots @ immutable) (address_local : B.u32) (local : B.u32) =
  address_local <> local && slots.Load.start <> local && slots.Load.captures <> local && slots.Load.recursive <> local
let[@def] (emit @ total) (fragment : Copy.fragment @ immutable) (table_base : B.u32) (code_local : B.u32)
    (address_local : B.u32) (slots : Load.slots @ immutable) (object_local : B.u32) (frame_local : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  E.append (Select.emit table_base code_local address_local slots)
    (Frame.emit fragment slots.Load.start object_local frame_local argument_tag argument_payload)
type result = {entry : Entry.result; locals : S.stack}
let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (argument : V.value) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (fragment : Copy.fragment) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (runtime : R.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (code : Table.count) -> (code_local : B.u32) -> (address_local : B.u32) -> (slots : Load.slots) @ immutable -> (descriptor : R.descriptor) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | R.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= 4294967295
      && Index.represents id code && R.lookup runtime code === Some descriptor
      && L.get state.X.machine.E.locals code_local === Some (S.I32 code)
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Load.distinct slots address_local && Load.writable slots state.X.machine.E.locals
      && separate slots address_local object_local && separate slots address_local frame_local
      && separate slots address_local argument_tag && separate slots address_local argument_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Copy.matches entry capacity fragment
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | Machine.invoke program heap (V.Closure_pointer closure) argument === Some out.entry.Entry.entered
      && X.run (emit fragment table_base code_local address_local slots object_local frame_local argument_tag argument_payload) state === X.Done {X.memory = out.entry.Entry.frame.Frame.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.entry.Entry.frame.Frame.memory
      && Image.related out.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.entry.Entry.frame.Frame.memory limit
      && V.length out.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures
      && Codec.decode (Model.signature entry) function_.C.start out.entry.Entry.cells === Some (out.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.entry.Entry.frame.Frame.memory base === Some out.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number descriptor.R.start), out.entry.Entry.cells))) out.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number descriptor.R.start), out.entry.Entry.cells), out.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop} @ immutable =
  fun program heap entry function_ id closure captures argument count fragment capacity runtime table_base table_count code code_local address_local slots descriptor state base stop limit object_local frame_local argument_tag argument_payload premise ->
    let selected = Select.correct program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      id code descriptor state table_base table_count code_local address_local slots () in
    let middle = {X.memory = state.X.memory; machine = {E.locals = selected.Select.locals; stack = state.X.machine.E.stack}} in
    ghost_ (separate_def slots address_local object_local; separate_def slots address_local frame_local;
      separate_def slots address_local argument_tag; separate_def slots address_local argument_payload;
      Preserve.correct table_base code_local address_local slots state middle object_local ();
      Preserve.correct table_base code_local address_local slots state middle frame_local ();
      Preserve.correct table_base code_local address_local slots state middle argument_tag ();
      Preserve.correct table_base code_local address_local slots state middle argument_payload ());
    let entered = Entry.correct program heap entry function_ id closure captures argument count fragment capacity descriptor.R.start slots.Load.start
      middle base stop limit object_local frame_local argument_tag argument_payload () in
    ghost_ (emit_def fragment table_base code_local address_local slots object_local frame_local argument_tag argument_payload;
      X.append_correct (Select.emit table_base code_local address_local slots)
        (Frame.emit fragment slots.Load.start object_local frame_local argument_tag argument_payload) state);
    {entry = entered; locals = selected.Select.locals}
