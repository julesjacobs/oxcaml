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
module Read = Hmc_wasm_list_memory
module Finish = Hmc_wasm_list_finish_heap
type result = {frame : Invariant.result; locals : S.stack}
let[@def] (emit @ total) (fragment : Lower.fragment @ immutable) (base_local : B.u32) (object_local : B.u32) (slots : Capture.slots @ immutable) =
  E.append (Capture.emit (Read.zero ()) (Read.eight ()) slots object_local) (Hmc_wasm_list_finish.emit fragment base_local slots)
let (correct @ total) : (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable -> (address : B.u32) -> (object_local : B.u32) -> (heap_local : B.u32) -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && Hmc_wasm_list_relayout.matches signature next capacity max_pc fragment
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296 && base <= 4294967216
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Heap.valid table heap && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Cons (head, tail))
      && L.get state.X.machine.E.locals object_local === Some (S.I32 address)
      && Capture.distinct slots && Capture.separate slots object_local && Capture.separate slots base_local && Capture.separate slots heap_local
      && Capture.writable slots state.X.machine.E.locals
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.frame.Invariant.memory
      && Hmc_heap_image.related out.frame.Invariant.memory heap
      && Bytes.drop out.frame.Invariant.memory frame_stop === Some suffix
      && X.run (emit fragment base_local object_local slots) state === X.Done {X.memory = out.frame.Invariant.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && Codec.decode (Model.successor signature element) next out.frame.Invariant.cells === Some (Model.transition activation next head tail, out.frame.Invariant.padding)
      && Heap.length out.frame.Invariant.cells === Heap.length cells && Bytes.drop out.frame.Invariant.memory base === Some out.frame.Invariant.bytes
      && Wire.decode_cells (D.S (Heap.length out.frame.Invariant.cells)) out.frame.Invariant.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.frame.Invariant.cells), suffix)
      && L.get out.locals base_local === Some (S.I32 base)
      && L.get out.locals heap_local === L.get state.X.machine.E.locals heap_local} @ immutable =
  fun signature element activation next head tail fragment capacity max_pc old_pc cells old_padding table heap address object_local heap_local frame_stop state base_local base slots before_frame suffix premise ->
    let locals = Capture.object_fields table heap address head tail state object_local base_local heap_local slots () in
    let captured = {X.memory = state.X.memory; machine = {E.locals; stack = state.X.machine.E.stack}} in
    let frame = Finish.correct signature element activation next head tail fragment capacity max_pc old_pc cells old_padding heap frame_stop captured base_local base slots before_frame suffix () in
    ghost_ (emit_def fragment base_local object_local slots;
      X.append_correct (Capture.emit (Read.zero ()) (Read.eight ()) slots object_local) (Hmc_wasm_list_finish.emit fragment base_local slots) state);
    {frame; locals}
