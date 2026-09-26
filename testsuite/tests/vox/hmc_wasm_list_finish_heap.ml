module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module Model = Hmc_frame_list_branch
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Header = Hmc_wasm_header_update
module Capture = Hmc_wasm_list_capture
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module View = Hmc_frame_slices
module Cap = Hmc_cell_capacity
module Fits = Hmc_frame_capacity
module Store = Hmc_wasm_list_store
module Invariant = Hmc_wasm_list_finish_invariant
let (correct @ total) : (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (heap : Heap.heap) @ immutable -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && Hmc_wasm_list_relayout.matches signature next capacity max_pc fragment
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296 && base <= 4294967216
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals slots.Capture.head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals slots.Capture.head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals slots.Capture.tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals slots.Capture.tail_payload === Some (S.I64 (V.payload tail))
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)} ->
    {out : Invariant.result | Hmc_linear_preservation.equal_prefix base state.X.memory out.Invariant.memory
      && Hmc_heap_image.related out.Invariant.memory heap
      && Bytes.drop out.Invariant.memory frame_stop === Some suffix
      && X.run (Hmc_wasm_list_finish.emit fragment base_local slots) state === X.Done {X.memory = out.Invariant.memory; machine = state.X.machine}
      && Codec.decode (Model.successor signature element) next out.Invariant.cells === Some (Model.transition activation next head tail, out.Invariant.padding)
      && Heap.length out.Invariant.cells === Heap.length cells && Bytes.drop out.Invariant.memory base === Some out.Invariant.bytes
      && Wire.decode_cells (D.S (Heap.length out.Invariant.cells)) out.Invariant.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.Invariant.cells), suffix)} @ immutable =
  fun signature element activation next head tail fragment capacity max_pc old_pc cells old_padding heap frame_stop state base_local base slots before_frame suffix premise ->
    let out = Invariant.correct signature element activation next head tail fragment capacity max_pc old_pc cells old_padding state base_local base slots before_frame suffix () in
    let before_cells = Heap.Cell (V.Word (Header.number old_pc), cells) in
    let after_cells = Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.Invariant.cells) in
    ghost_ (Heap.length_def before_cells; Heap.length_def after_cells;
      Index.represents_def (D.S (Heap.length cells)) (capacity + 1);
      Hmc_wasm_frame_suffix.preserve_heap state.X.memory out.Invariant.memory heap base frame_stop (capacity + 1)
        before_cells after_cells before_frame out.Invariant.bytes suffix ();
      Hmc_wasm_frame_suffix.suffix out.Invariant.memory base frame_stop (capacity + 1) after_cells out.Invariant.bytes suffix ());
    out
