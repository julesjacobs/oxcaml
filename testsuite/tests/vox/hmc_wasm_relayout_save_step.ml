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
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (next : D.index) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (env_count : Lower.count) -> (old_count : Lower.count) -> (old_pc : W.limb) ->
    (body : Heap.cells) @ immutable -> (env : Heap.cells) @ immutable -> (temporaries : Heap.cells) @ immutable ->
    (old : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (after_body : Heap.cells) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Geometry.matches signature (G.Save_environment next) capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && Index.represents activation.Frame.pc old_pc
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (Codec.temporaries_size signature.G.temporaries) old_count
      && Model.successor signature (G.Save_environment next) === Some next_signature
      && Simple.step (G.Save_environment next) (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, body))) === Some (activation, old_padding)
      && Seg.take (Codec.locals_size signature.G.locals) body === Some env
      && Seg.drop (Codec.locals_size signature.G.locals) body === Some temporaries
      && Seg.take (Codec.temporaries_size signature.G.temporaries) temporaries === Some old
      && Seg.drop (Heap.length (Seg.append env old)) temporaries === Some padding
      && after_body === Seg.append env (Seg.append env (Seg.append old padding))
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && P.equal_prefix base state.X.memory after && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) before_frame ===
        Some (Heap.Cell (V.Word (Header.number old_pc), Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, body))), tail)
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length after_body)))) after_frame ===
        Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, after_body))), tail)} ->
    {u : unit | X.run (Lower.emit fragment base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc
        (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, after_body))) === Some (next_activation, padding)} @ ghost =
  fun signature next_signature activation next_activation frames next fragment capacity max_pc env_count old_count old_pc body env temporaries old old_padding padding
      after_body state after base_local base before_frame after_frame tail premise -> ghost_ (
    let current = activation.Frame.current in let accumulator = activation.Frame.accumulator in
    let cells = Heap.Cell (current, Heap.Cell (accumulator, body)) in
    let after_cells = Heap.Cell (current, Heap.Cell (accumulator, after_body)) in
    let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
    let copied_cells = Heap.Cell (V.Word (Header.number old_pc), after_cells) in
    let _reshaped = Hmc_frame_relayout_source.correct signature (G.Save_environment next) activation frames cells old_padding padding next_signature next_activation () in
    Model.reshape_def signature (G.Save_environment next) cells padding;
    Hmc_frame_relayout_patch.save_environment signature next current accumulator body env temporaries old padding ();
    Hmc_frame_header_patch.shift (D.S (D.S (Codec.locals_size signature.G.locals))) (Seg.append env old) cells after_cells (V.Word (Header.number old_pc)) ();
    Hmc_cell_slice.take_length (Codec.locals_size signature.G.locals) body env ();
    Hmc_cell_slice.take_length (Codec.temporaries_size signature.G.temporaries) temporaries old ();
    Hmc_frame_header_patch.drop D.Z (V.Word (Header.number old_pc)) current accumulator body; Seg.drop_def D.Z body;
    Hmc_frame_header_patch.drop (Heap.length env) (V.Word (Header.number old_pc)) current accumulator body;
    Heap.length_def full; Heap.length_def cells; Heap.length_def (Heap.Cell (accumulator, body));
    let copied_frame = Wire.encode_cells copied_cells tail in
    let copied = Splice.replace state.X.memory base before_frame copied_frame () in
    Hmc_wasm_relayout_save.correct signature next fragment capacity max_pc env old env_count old_count body temporaries
      state.X.memory copied base before_frame copied_frame full copied_cells tail ();
    Heap.length_def copied_cells; Heap.length_def after_cells; Heap.length_def (Heap.Cell (accumulator, after_body));
    Splice.shared state.X.memory copied after base ();
    Hmc_wasm_relayout_finish.correct fragment state base_local base old_pc current accumulator after_body copied after copied_frame after_frame tail ())
