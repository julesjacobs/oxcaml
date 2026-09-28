module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Patch = Hmc_frame_relayout_patch
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Header = Hmc_wasm_header_update
module Model = Hmc_frame_list_branch
module Store = Hmc_wasm_list_store
module Lower = Hmc_wasm_relayout
module Capture = Hmc_wasm_list_capture
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Splice = Wasm_memory_splice
let (correct @ total) : (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (env_count : Lower.count) -> (old_count : Lower.count) -> (old_pc : W.limb) ->
    (old_head : V.value) @ immutable -> (old_tail : V.value) @ immutable -> (remaining : Heap.cells) @ immutable ->
    (env : Heap.cells) @ immutable -> (old_start : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Hmc_wasm_list_relayout.matches signature next capacity max_pc fragment
      && Index.represents activation.Frame.pc old_pc
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (Codec.temporaries_size signature.G.temporaries) old_count
      && Codec.decode signature activation.Frame.pc
        (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, remaining)))))
        === Some (activation, old_padding)
      && Seg.take (Codec.locals_size signature.G.locals) (Heap.Cell (old_head, Heap.Cell (old_tail, remaining))) === Some env
      && Seg.drop (Codec.locals_size signature.G.locals) (Heap.Cell (old_head, Heap.Cell (old_tail, remaining))) === Some old_start
      && Seg.take (Codec.temporaries_size signature.G.temporaries) old_start === Some old
      && Seg.drop (Heap.length (Seg.append (Seg.append env env) (Seg.append old Heap.Empty))) remaining === Some padding
      && base + 16 + 16 * capacity <= 4294967296 && base <= 4294967216
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals slots.Capture.head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals slots.Capture.head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals slots.Capture.tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals slots.Capture.tail_payload === Some (S.I64 (V.payload tail))
      && P.equal_prefix base state.X.memory after && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length (Store.cells (V.Word (Header.number old_pc)) activation.Frame.current activation.Frame.accumulator old_head old_tail remaining)) before_frame
        === Some (Store.cells (V.Word (Header.number old_pc)) activation.Frame.current activation.Frame.accumulator old_head old_tail remaining, suffix)
      && Wire.decode_cells (Heap.length (Store.cells (V.Word (Header.number fragment.Lower.pc)) activation.Frame.current activation.Frame.accumulator head tail
          (Seg.append env (Seg.append env (Seg.append old padding))))) after_frame
        === Some (Store.cells (V.Word (Header.number fragment.Lower.pc)) activation.Frame.current activation.Frame.accumulator head tail
          (Seg.append env (Seg.append env (Seg.append old padding))), suffix)} ->
    {u : unit | X.run (Hmc_wasm_list_finish.emit fragment base_local slots) state === X.Done {X.memory = after; machine = state.X.machine}
      && Codec.decode (Model.successor signature element) next
        (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, Heap.Cell (head, Heap.Cell (tail,
          Seg.append env (Seg.append env (Seg.append old padding)))))))
        === Some (Model.transition activation next head tail, padding)} @ ghost =
  fun signature element activation next head tail fragment capacity max_pc env_count old_count old_pc old_head old_tail remaining
      env old_start old old_padding padding state after base_local base slots before_frame after_frame suffix premise -> ghost_ (
    let current = activation.Frame.current in let accumulator = activation.Frame.accumulator in
    let pc = V.Word (Header.number old_pc) in
    let body = Heap.Cell (old_head, Heap.Cell (old_tail, remaining)) in
    let source_cells = Heap.Cell (current, Heap.Cell (accumulator, body)) in
    let before_cells = Store.cells pc current accumulator old_head old_tail remaining in
    let values = Seg.append (Seg.append env env) (Seg.append old Heap.Empty) in
    let rest = Seg.append env (Seg.append env (Seg.append old padding)) in
    let copied_cells = Store.cells pc current accumulator old_head old_tail rest in
    let copied_frame = Wire.encode_cells copied_cells suffix in
    let copied = Splice.replace state.X.memory base before_frame copied_frame () in
    let prefix = Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))))) in
    Heap.length_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))))));
    Heap.length_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)))));
    Heap.length_def (Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))));
    Heap.length_def (Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)));
    Heap.length_def (Heap.Cell (old_tail, Heap.Empty));
    Heap.length_def (Heap.Empty);
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)))))) (remaining);
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))))) (remaining);
    Seg.append_def (Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)))) (remaining);
    Seg.append_def (Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))) (remaining);
    Seg.append_def (Heap.Cell (old_tail, Heap.Empty)) (remaining);
    Seg.append_def (Heap.Empty) (remaining);
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)))))) (Seg.append values padding);
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))))) (Seg.append values padding);
    Seg.append_def (Heap.Cell (accumulator, Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty)))) (Seg.append values padding);
    Seg.append_def (Heap.Cell (old_head, Heap.Cell (old_tail, Heap.Empty))) (Seg.append values padding);
    Seg.append_def (Heap.Cell (old_tail, Heap.Empty)) (Seg.append values padding);
    Seg.append_def (Heap.Empty) (Seg.append values padding);
    Patch.prefix prefix remaining values padding ();
    Patch.associate (Seg.append env env) (Seg.append old Heap.Empty) padding;
    Patch.associate env env (Seg.append (Seg.append old Heap.Empty) padding);
    Patch.associate old Heap.Empty padding; Seg.append_def Heap.Empty padding;
    Store.cells_def pc current accumulator old_head old_tail remaining;
    Store.cells_def pc current accumulator old_head old_tail rest;
    Hmc_cell_slice.take_length (Codec.locals_size signature.G.locals) body env ();
    Hmc_cell_slice.take_length (Codec.temporaries_size signature.G.temporaries) old_start old ();
    Hmc_frame_header_patch.drop D.Z pc current accumulator body;
    Seg.drop_def D.Z body;
    Hmc_frame_header_patch.drop (Heap.length env) pc current accumulator body;
    Hmc_wasm_list_relayout_copy.correct signature next fragment capacity max_pc env old env_count old_count body old_start
      state.X.memory copied base before_frame copied_frame before_cells copied_cells suffix ();
    Splice.shared state.X.memory copied after base ();
    Hmc_wasm_list_finish_memory.correct fragment state base_local base slots old_pc current accumulator old_head old_tail head tail rest
      copied after copied_frame after_frame suffix ();
    Model.reshape_def signature head tail source_cells padding;
    let _result = Hmc_frame_list_branch_source.correct signature element activation next head tail source_cells old_padding padding () in ())
