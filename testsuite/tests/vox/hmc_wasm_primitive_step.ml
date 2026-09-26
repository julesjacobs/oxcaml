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
module Model = Hmc_frame_primitive_model
module Patch = Hmc_frame_relayout_patch
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_primitive_lower
module Relayout = Hmc_wasm_relayout
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module M = Wasm_memory
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
let rec (add_one @ total) : (n : D.index) @ immutable ->
    {u : unit | D.add n (D.S D.Z) === D.S n} @ ghost = fun n -> ghost_ (
    D.add_def n (D.S D.Z); match n with D.Z -> () | D.S rest -> add_one rest)
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (operation : D.word_operation) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (env_count : Relayout.count) -> (saved_count : Relayout.count) -> (old_count : Relayout.count) -> (old_pc : W.limb) ->
    (left : W.t) @ immutable -> (right : W.t) @ immutable ->
    (body : Heap.cells) @ immutable -> (saved_start : Heap.cells) @ immutable ->
    (saved : Heap.cells) @ immutable -> (more : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (after_body : Heap.cells) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.matches signature (G.Primitive (operation, next)) capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, D.Word64, schema)
      && activation.Frame.accumulator === V.Word right
      && Index.represents activation.Frame.pc old_pc
      && Index.represents (Codec.locals_size signature.G.locals) env_count && Index.represents (Codec.locals_size context) saved_count
      && Index.represents (Codec.temporaries_size schema) old_count
      && Model.successor signature operation === Some next_signature
      && Simple.step (G.Primitive (operation, next)) (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (V.Word right, body))) === Some (activation, old_padding)
      && Seg.drop (Codec.locals_size signature.G.locals) body === Some (Heap.Cell (V.Word left, saved_start))
      && Seg.take (Codec.locals_size context) saved_start === Some saved && Seg.drop (Codec.locals_size context) saved_start === Some more
      && Seg.take (Codec.temporaries_size schema) more === Some old
      && Seg.drop (Heap.length (Seg.append saved old)) body === Some padding && after_body === Seg.append (Seg.append saved old) padding
      && base + 16 + 16 * capacity <= 4294967296 && base <= 4294967248
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base fragment.Lower.left_offset M.W64 === Some (S.I64 left)
      && M.load state.X.memory base (Hmc_wasm_primitive_payload.offset ()) M.W64 === Some (S.I64 right)
      && P.equal_prefix base state.X.memory after && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) before_frame ===
        Some (Heap.Cell (V.Word (Header.number old_pc), Heap.Cell (activation.Frame.current, Heap.Cell (V.Word right, body))), tail)
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length after_body)))) after_frame ===
        Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), Heap.Cell (activation.Frame.current, Heap.Cell (Simple.primitive operation left right, after_body))), tail)} ->
    {u : unit | X.run (Lower.emit fragment base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc
        (Heap.Cell (activation.Frame.current, Heap.Cell (Simple.primitive operation left right, after_body))) === Some (next_activation, padding)} @ ghost =
  fun signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc env_count saved_count old_count old_pc
      left right body saved_start saved more old old_padding padding after_body state after base_local base before_frame after_frame tail premise -> ghost_ (
    Lower.matches_def signature (G.Primitive (operation, next)) capacity max_pc fragment;
    let current = activation.Frame.current in let value = Simple.primitive operation left right in
    let cells = Heap.Cell (current, Heap.Cell (V.Word right, body)) in
    let computed_cells = Heap.Cell (current, Heap.Cell (value, body)) in
    let after_cells = Heap.Cell (current, Heap.Cell (value, after_body)) in
    let full = Heap.Cell (V.Word (Header.number old_pc), computed_cells) in
    let copied_cells = Heap.Cell (V.Word (Header.number old_pc), after_cells) in
    let head = Heap.Cell (current, Heap.Cell (value, Heap.Empty)) in
    let values = Seg.append saved old in
    Heap.length_def head; Heap.length_def (Heap.Cell (value, Heap.Empty)); Heap.length_def Heap.Empty;
    Seg.append_def head body; Seg.append_def (Heap.Cell (value, Heap.Empty)) body; Seg.append_def Heap.Empty body;
    Seg.append_def head after_body; Seg.append_def (Heap.Cell (value, Heap.Empty)) after_body; Seg.append_def Heap.Empty after_body;
    Patch.prefix head body values padding (); Patch.associate saved old padding;
    Model.reshape_def signature operation cells padding;
    let _reshaped = Hmc_frame_primitive_source.correct signature operation next activation frames cells old_padding padding next_signature next_activation () in
    Hmc_frame_header_patch.shift (D.S (D.S D.Z)) values computed_cells after_cells (V.Word (Header.number old_pc)) ();
    Hmc_cell_slice.take_length (Codec.locals_size context) saved_start saved ();
    Hmc_cell_slice.take_length (Codec.temporaries_size schema) more old ();
    let env_size = Codec.locals_size signature.G.locals in
    Seg.drop_add env_size (D.S D.Z) body;
    Seg.drop_def (D.S D.Z) (Heap.Cell (V.Word left, saved_start)); Seg.drop_def D.Z saved_start;
    add_one env_size;
    Hmc_frame_header_patch.drop (D.S env_size) (V.Word (Header.number old_pc)) current value body;
    Seg.drop_add (D.S env_size) (Heap.length saved) body;
    D.add_def (D.S env_size) (Heap.length saved);
    Hmc_frame_header_patch.drop (D.S (D.add env_size (Heap.length saved))) (V.Word (Header.number old_pc)) current value body;
    Heap.length_def cells; Heap.length_def (Heap.Cell (V.Word right, body));
    Heap.length_def computed_cells; Heap.length_def (Heap.Cell (value, body)); Heap.length_def full;
    let computed_frame = Wire.encode_cells full tail in
    let computed = Splice.replace state.X.memory base before_frame computed_frame () in
    Hmc_wasm_relayout_finish.closure old_pc cells before_frame tail ();
    Hmc_wasm_relayout_finish.closure old_pc computed_cells computed_frame tail ();
    Hmc_wasm_primitive_update.correct operation fragment.Lower.left_offset left right old_pc current body (Heap.length body)
      state computed base_local base before_frame computed_frame tail ();
    let copied_frame = Wire.encode_cells copied_cells tail in
    let copied = Splice.replace computed base computed_frame copied_frame () in
    Hmc_wasm_primitive_restore.correct signature next operation context schema fragment capacity max_pc saved old env_count saved_count old_count
      saved_start more computed copied base computed_frame copied_frame full copied_cells tail ();
    Heap.length_def copied_cells; Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, after_body));
    Hmc_wasm_relayout_finish.closure old_pc after_cells copied_frame tail ();
    Hmc_wasm_relayout_finish.closure fragment.Lower.pc after_cells after_frame tail ();
    Splice.shared state.X.memory computed after base (); Splice.shared computed copied after base ();
    Hmc_wasm_pc_update.memory (Heap.length after_body) old_pc fragment.Lower.pc current value after_body copied after tail base copied_frame after_frame ();
    Relayout.finish_def copied base fragment.Lower.pc;
    Lower.correct fragment base_local state base computed copied after ())
