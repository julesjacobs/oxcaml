module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_value_pop
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_value_pop
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Invariant = Hmc_wasm_cons_finish_invariant
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_end : B.u32) -> (heap_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (heap : Heap.heap) @ immutable -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && Lower.matches signature next capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, head_type, schema)
      && activation.Frame.accumulator === right
      && (match activation.Frame.temporaries with Frame.Value (w, _, _) -> w === left | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next (V.Cons_pointer heap_base) === Some next_activation
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && heap_end = heap_base + 32 && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_end)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : Invariant.result | Hmc_linear_preservation.equal_prefix base state.X.memory out.Invariant.memory
      && Hmc_heap_image.related out.Invariant.memory heap
      && Bytes.drop out.Invariant.memory frame_stop === Some tail
      &&  X.run (Hmc_wasm_cons_finish.emit fragment base_local heap_local) state === X.Done {X.memory = out.Invariant.memory; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc out.Invariant.cells === Some (next_activation, out.Invariant.padding)
      && Heap.length out.Invariant.cells === Heap.length cells && Bytes.drop out.Invariant.memory base === Some out.Invariant.bytes
      && Wire.decode_cells (D.S (Heap.length out.Invariant.cells)) out.Invariant.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.Invariant.cells), tail)} @ immutable =
  fun signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_end heap_local cells old_padding heap frame_stop state base_local base before_frame tail premise ->
    let out = Invariant.correct signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_end heap_local cells old_padding state base_local base before_frame tail () in
    let before_cells = Heap.Cell (V.Word (Header.number old_pc), cells) in
    let after_cells = Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.Invariant.cells) in
    ghost_ (Heap.length_def before_cells; Heap.length_def after_cells;
      Index.represents_def (D.S (Heap.length cells)) (capacity + 1);
      Hmc_wasm_frame_suffix.preserve_heap state.X.memory out.Invariant.memory heap base frame_stop (capacity + 1)
        before_cells after_cells before_frame out.Invariant.bytes tail ();
      Hmc_wasm_frame_suffix.suffix out.Invariant.memory base frame_stop (capacity + 1) after_cells out.Invariant.bytes tail ());
    out
