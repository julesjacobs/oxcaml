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
module Patch = Hmc_frame_relayout_patch
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
    (instruction : G.instruction) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (env_count : Lower.count) -> (saved_count : Lower.count) -> (old_count : Lower.count) -> (old_pc : W.limb) ->
    (body : Heap.cells) @ immutable -> (env : Heap.cells) @ immutable -> (temporaries : Heap.cells) @ immutable ->
    (saved : Heap.cells) @ immutable -> (more : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (values : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (after_body : Heap.cells) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Geometry.matches signature instruction capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && (instruction === G.Restore next || instruction === G.Save_value next || instruction === G.Bind next)
      && signature.G.temporaries === G.Environment (context, schema)
      && Index.represents activation.Frame.pc old_pc
      && Index.represents (Codec.locals_size signature.G.locals) env_count && Index.represents (Codec.locals_size context) saved_count
      && Index.represents (Codec.temporaries_size schema) old_count
      && Model.successor signature instruction === Some next_signature
      && Simple.step instruction (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, body))) === Some (activation, old_padding)
      && Seg.take (Codec.locals_size signature.G.locals) body === Some env && Seg.drop (Codec.locals_size signature.G.locals) body === Some temporaries
      && Seg.take (Codec.locals_size context) temporaries === Some saved && Seg.drop (Codec.locals_size context) temporaries === Some more
      && Seg.take (Codec.temporaries_size schema) more === Some old
      && Patch.replacement instruction saved activation.Frame.accumulator old === Some values
      && Seg.drop (Heap.length values) body === Some padding && after_body === Seg.append values padding
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
  fun signature next_signature activation next_activation frames instruction next context schema fragment capacity max_pc env_count saved_count old_count old_pc
      body env temporaries saved more old values old_padding padding after_body state after base_local base before_frame after_frame tail premise -> ghost_ (
    let current = activation.Frame.current in let accumulator = activation.Frame.accumulator in
    let cells = Heap.Cell (current, Heap.Cell (accumulator, body)) in
    let after_cells = Heap.Cell (current, Heap.Cell (accumulator, after_body)) in
    let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
    let copied_cells = Heap.Cell (V.Word (Header.number old_pc), after_cells) in
    let head = Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty)) in
    Heap.length_def head; Heap.length_def (Heap.Cell (accumulator, Heap.Empty)); Heap.length_def Heap.Empty;
    Seg.append_def head body; Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) body; Seg.append_def Heap.Empty body;
    Seg.append_def head after_body; Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) after_body; Seg.append_def Heap.Empty after_body;
    Patch.prefix head body values padding ();
    Patch.saved_environment signature instruction context schema current accumulator body env temporaries saved more old values padding ();
    let _reshaped = Hmc_frame_relayout_source.correct signature instruction activation frames cells old_padding padding next_signature next_activation () in
    Hmc_frame_header_patch.shift (D.S (D.S D.Z)) values cells after_cells (V.Word (Header.number old_pc)) ();
    Hmc_cell_slice.take_length (Codec.locals_size context) temporaries saved ();
    Hmc_cell_slice.take_length (Codec.temporaries_size schema) more old ();
    Hmc_frame_header_patch.drop (Codec.locals_size signature.G.locals) (V.Word (Header.number old_pc)) current accumulator body;
    Hmc_frame_header_patch.drop (D.add (Codec.locals_size signature.G.locals) (Heap.length saved)) (V.Word (Header.number old_pc)) current accumulator body;
    Seg.drop_add (Codec.locals_size signature.G.locals) (Heap.length saved) body;
    Heap.length_def full; Heap.length_def cells; Heap.length_def (Heap.Cell (accumulator, body));
    let copied_frame = Wire.encode_cells copied_cells tail in
    let copied = Splice.replace state.X.memory base before_frame copied_frame () in
    Patch.replacement_def instruction saved accumulator old;
    (match instruction with
    | G.Restore _ -> Hmc_wasm_relayout_restore.correct signature next context schema fragment capacity max_pc saved old env_count saved_count old_count
        temporaries more state.X.memory copied base before_frame copied_frame full copied_cells tail ()
    | G.Save_value _ | G.Bind _ ->
      Seg.drop_def (D.S (D.S D.Z)) full; Seg.drop_def (D.S D.Z) cells; Seg.drop_def D.Z (Heap.Cell (accumulator, body));
      Seg.take_def (D.S D.Z) (Heap.Cell (accumulator, body)); Seg.take_def D.Z body;
      let bind = match instruction with G.Bind _ -> true | _ -> false in
      Hmc_wasm_relayout_value.correct signature bind next context schema fragment capacity max_pc saved old accumulator (Heap.Cell (accumulator, body))
        env_count saved_count old_count temporaries more state.X.memory copied base before_frame copied_frame full copied_cells tail ()
    | _ -> ());
    Heap.length_def copied_cells; Heap.length_def after_cells; Heap.length_def (Heap.Cell (accumulator, after_body));
    Splice.shared state.X.memory copied after base ();
    Hmc_wasm_relayout_finish.correct fragment state base_local base old_pc current accumulator after_body copied after copied_frame after_frame tail ())
