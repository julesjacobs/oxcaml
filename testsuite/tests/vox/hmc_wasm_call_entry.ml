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
module Frame = Hmc_wasm_call_frame
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
let rec (environment_size @ total) : (context : D.context) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | Codec.environment context cells} -> {u : unit | Heap.length cells === Codec.locals_size context} @ ghost =
  fun context cells premise -> ghost_ (
    Codec.environment_def context cells; Codec.locals_size_def context; Heap.length_def cells;
    match context, cells with D.Binding (_, rest), Heap.Cell (_, tail) -> environment_size rest tail () | _ -> ())
type result = {frame : Frame.result; entered : F.activation; cells : Heap.cells}
let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (argument : V.value) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (fragment : Copy.fragment) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (pc : W.limb) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Copy.matches entry capacity fragment && Index.represents function_.C.start pc
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | Machine.invoke program heap (V.Closure_pointer closure) argument === Some out.entered
      && X.run (Frame.emit fragment pc object_local frame_local argument_tag argument_payload) state === X.Done {X.memory = out.frame.Frame.memory; machine = state.X.machine}
      && Image.related out.frame.Frame.memory heap && Hmc_linear_bounds.covers out.frame.Frame.memory limit
      && V.length out.frame.Frame.memory === V.length state.X.memory
      && out.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures
      && Codec.decode (Model.signature entry) function_.C.start out.cells === Some (out.entered, Heap.Empty)
      && Bytes.drop out.frame.Frame.memory base === Some out.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number pc), out.cells))) out.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number pc), out.cells), out.frame.Frame.suffix)
      && Bytes.drop out.frame.Frame.memory stop === Bytes.drop state.X.memory stop} @ immutable =
  fun program heap entry function_ id closure captures argument count fragment capacity pc state base stop limit object_local frame_local argument_tag argument_payload premise ->
    ghost_ (environment_size entry.K.captured captures ();
      Copy.matches_def entry capacity fragment;
      Hmc_wasm_relayout_geometry.size_represents (Heap.length captures) count ());
    let _read = Read.correct program.I.origin.C.origin.Hmc_closure_program.table heap state.X.memory closure id captures count () in
    let frame = Frame.correct captures count fragment pc argument state closure base limit object_local frame_local argument_tag argument_payload () in
    let entered = Model.activation entry function_.C.start (V.Closure_pointer closure) argument captures in
    let cells = Decode.cells entry closure argument captures in
    ghost_ (Decode.correct entry function_.C.start pc closure argument captures ();
      Hmc_heap_call_transition.invoke program heap closure id captures entry function_ argument ();
      Model.activation_def entry function_.C.start (V.Closure_pointer closure) argument captures;
      Hmc_wasm_call_capture_memory.capture_bytes_def count;
      Layout.width_def entry.K.recursive;
      S.add32_def (Layout.width entry.K.recursive) (16 * count); S.add32_def base (Layout.width entry.K.recursive + 16 * count);
      Above.preserve state.X.memory frame.Frame.memory heap stop ());
    {frame; entered; cells}
