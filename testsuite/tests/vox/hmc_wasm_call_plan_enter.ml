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
type result = {selected : Select.result; entry : Entry.result}
let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (argument : V.value) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code : B.u32) -> (code_local : B.u32) -> (labels : T.labels) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (pc : W.limb) -> (pc_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | L.get state.X.machine.E.locals pc_local === Some (S.I32 pc)
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && Index.represents id code && L.get state.X.machine.E.locals code_local === Some (S.I32 code)
      && state.X.machine.E.stack === S.Empty && Index.represents function_.C.start pc
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | Wasm_empty_labels.related out.selected.Select.cleanup labels out.selected.Select.selected.T.labels
      && Machine.invoke program heap (V.Closure_pointer closure) argument === Some out.entry.Entry.entered
      && X.run (Frame.emit out.selected.Select.fragment pc_local object_local frame_local argument_tag argument_payload) state === X.Done {X.memory = out.entry.Entry.frame.Frame.memory; machine = state.X.machine}
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.entry.Entry.frame.Frame.memory
      && Image.related out.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.entry.Entry.frame.Frame.memory limit
      && V.length out.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures
      && Codec.decode (Model.signature entry) function_.C.start out.entry.Entry.cells === Some (out.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.entry.Entry.frame.Frame.memory base === Some out.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number pc), out.entry.Entry.cells))) out.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number pc), out.entry.Entry.cells), out.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && T.run (Fuel.add (Local_select.cost (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code code_local)
          (Code.length (Frame.emit out.selected.Select.fragment pc_local object_local frame_local argument_tag argument_payload)))
        {T.code = Select.emit plans code_local pc_local object_local frame_local argument_tag argument_payload; labels; state}
        === T.Running {T.code = T.Empty; labels = out.selected.Select.selected.T.labels;
          state = {X.memory = out.entry.Entry.frame.Frame.memory; machine = state.X.machine}}} @ immutable =
  fun program heap entry function_ id closure captures argument count plans code code_local labels capacity pc pc_local state base stop limit object_local frame_local argument_tag argument_payload premise ->
    let selected = Select.correct program.I.origin.C.origin.Hmc_closure_program.table capacity plans id code entry
      code_local pc_local object_local frame_local argument_tag argument_payload state labels () in
    let entered = Entry.correct program heap entry function_ id closure captures argument count selected.Select.fragment capacity pc pc_local
      state base stop limit object_local frame_local argument_tag argument_payload () in
    ghost_ (
      let body = Frame.emit selected.Select.fragment pc_local object_local frame_local argument_tag argument_payload in
      let after = {X.memory = entered.Entry.frame.Frame.memory; machine = state.X.machine} in
      Wasm_control_success.straight body state after ();
      Lift.correct body T.Empty selected.Select.selected.T.labels state after ();
      Fuel.correct (Local_select.cost (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code code_local)
        (Code.length body) {T.code = Select.emit plans code_local pc_local object_local frame_local argument_tag argument_payload; labels; state});
    {selected; entry = entered}
