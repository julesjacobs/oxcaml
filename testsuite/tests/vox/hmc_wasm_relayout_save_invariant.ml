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
module Model = Hmc_frame_relayout_model
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
type result = {memory : B.bytes; cells : Heap.cells; padding : Heap.cells; bytes : B.bytes}
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (next : D.index) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Geometry.matches signature (G.Save_environment next) capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature (G.Save_environment next) === Some next_signature
      && Simple.step (G.Save_environment next) (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.memory
      && X.run (Lower.emit fragment base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc out.cells === Some (next_activation, out.padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.cells), tail)} @ immutable =
  fun signature next_signature activation next_activation frames next fragment capacity max_pc old_pc cells old_padding state base_local base before_frame tail premise ->
    ghost_ (Geometry.matches_def signature (G.Save_environment next) capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required);
    let view = Hmc_frame_slices.decode signature activation cells old_padding () in
    let env_count = Hmc_wasm_schema_counts.encode (Codec.locals_size signature.G.locals) capacity () in
    let old_count = Hmc_wasm_schema_counts.encode (Codec.temporaries_size signature.G.temporaries) capacity () in
    let values = Seg.append view.Hmc_frame_slices.env view.Hmc_frame_slices.old in
    let prefix = Seg.append view.Hmc_frame_slices.env values in
    let target = D.S (D.S (Heap.length prefix)) in
    ghost_ (Hmc_wasm_range_four.length_append view.Hmc_frame_slices.env view.Hmc_frame_slices.old;
      Hmc_wasm_range_four.length_append view.Hmc_frame_slices.env values;
      Hmc_u32_index_sum.correct (Heap.length view.Hmc_frame_slices.env) (Heap.length view.Hmc_frame_slices.old) env_count old_count (env_count + old_count) ();
      Hmc_u32_index_sum.correct (Heap.length view.Hmc_frame_slices.env) (Heap.length values) env_count (env_count + old_count) (2 * env_count + old_count) ();
      Index.represents_def (D.S (Heap.length prefix)) (1 + 2 * env_count + old_count);
      Index.represents_def target (2 + 2 * env_count + old_count);
      Hmc_cell_capacity.numeric target (Heap.length cells) (2 + 2 * env_count + old_count) capacity ());
    let cut = Hmc_cell_capacity.split target cells () in
    let padding = cut.Hmc_cell_capacity.suffix in
    let after_body = Seg.append prefix padding in
    let after_cells = Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, after_body)) in
    let full = Heap.Cell (V.Word (Header.number fragment.Lower.pc), after_cells) in
    let after_frame = Wire.encode_cells full tail in
    let after = Wasm_memory_splice.replace state.X.memory base before_frame after_frame () in
    ghost_ (
      Seg.drop_def target cells;
      Seg.drop_def (D.S (Heap.length prefix)) (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
      Seg.drop_add (Heap.length view.Hmc_frame_slices.env) (Heap.length values) view.Hmc_frame_slices.body;
      Hmc_frame_relayout_patch.associate view.Hmc_frame_slices.env values padding;
      Hmc_frame_relayout_patch.associate view.Hmc_frame_slices.env view.Hmc_frame_slices.old padding;
      Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
      Heap.length_def full; Heap.length_def after_cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, after_body));
      Hmc_wasm_relayout_save_step.correct signature next_signature activation next_activation frames next fragment capacity max_pc env_count old_count old_pc
        view.Hmc_frame_slices.body view.Hmc_frame_slices.env view.Hmc_frame_slices.temporaries view.Hmc_frame_slices.old old_padding padding
        after_body state after base_local base before_frame after_frame tail ();
      Hmc_wasm_range_four.length_append prefix padding;
      D.add_def target (Heap.length padding); D.add_def (D.S (Heap.length prefix)) (Heap.length padding));
    {memory = after; cells = after_cells; padding; bytes = after_frame}
