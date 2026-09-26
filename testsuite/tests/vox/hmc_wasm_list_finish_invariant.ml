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
type result = {memory : B.bytes; cells : Heap.cells; padding : Heap.cells; bytes : B.bytes}
let (correct @ total) : (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Hmc_wasm_list_relayout.matches signature next capacity max_pc fragment
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
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.memory
      && X.run (Hmc_wasm_list_finish.emit fragment base_local slots) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Codec.decode (Model.successor signature element) next out.cells === Some (Model.transition activation next head tail, out.padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.cells), suffix)} @ immutable =
  fun signature element activation next head tail fragment capacity max_pc old_pc cells old_padding state base_local base slots before_frame suffix premise ->
    ghost_ (Hmc_wasm_list_relayout.matches_def signature next capacity max_pc fragment);
    let view = View.decode signature activation cells old_padding () in
    let env = view.View.env in let old = view.View.old in
    let env_count = Hmc_wasm_schema_counts.encode (Codec.locals_size signature.G.locals) capacity () in
    let old_count = Hmc_wasm_schema_counts.encode (Codec.temporaries_size signature.G.temporaries) capacity () in
    let values = Seg.append (Seg.append env env) (Seg.append old Heap.Empty) in
    let target = D.S (D.S (D.S (D.S (Heap.length values)))) in
    ghost_ (
      Hmc_wasm_range_four.length_append env env;
      Hmc_wasm_range_four.length_append old Heap.Empty;
      Hmc_wasm_range_four.length_append (Seg.append env env) (Seg.append old Heap.Empty);
      Heap.length_def Heap.Empty; Index.represents_def D.Z 0;
      Hmc_u32_index_sum.correct (Heap.length env) (Heap.length env) env_count env_count (2 * env_count) ();
      Hmc_u32_index_sum.correct (Heap.length old) D.Z old_count 0 old_count ();
      Hmc_u32_index_sum.correct (Heap.length (Seg.append env env)) (Heap.length (Seg.append old Heap.Empty)) (2 * env_count) old_count (2 * env_count + old_count) ();
      Index.represents_def (D.S (Heap.length values)) (1 + 2 * env_count + old_count);
      Index.represents_def (D.S (D.S (Heap.length values))) (2 + 2 * env_count + old_count);
      Index.represents_def (D.S (D.S (D.S (Heap.length values)))) (3 + 2 * env_count + old_count);
      Index.represents_def target (4 + 2 * env_count + old_count);
      Cap.numeric target (Heap.length cells) (4 + 2 * env_count + old_count) capacity ());
    let cut = Cap.split target cells () in
    let padding = cut.Cap.suffix in
    ghost_ (Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, view.View.body));
      Fits.le_def target (Heap.length cells);
      Fits.le_def (D.S (D.S (D.S (Heap.length values)))) (D.S (Heap.length view.View.body));
      Fits.le_def (D.S (D.S (Heap.length values))) (Heap.length view.View.body);
      Heap.length_def view.View.body);
    match view.View.body with
    | Heap.Empty -> unreachable_ ()
    | Heap.Cell (old_head, more) ->
      ghost_ (Heap.length_def more; Fits.le_def (D.S (Heap.length values)) (Heap.length more));
      (match more with
      | Heap.Empty -> unreachable_ ()
      | Heap.Cell (old_tail, remaining) ->
        let rest = Seg.append env (Seg.append env (Seg.append old padding)) in
        let after_cells = Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, Heap.Cell (head, Heap.Cell (tail, rest)))) in
        let full = Heap.Cell (V.Word (Header.number fragment.Lower.pc), after_cells) in
        let after_frame = Wire.encode_cells full suffix in
        let after = Wasm_memory_splice.replace state.X.memory base before_frame after_frame () in
        ghost_ (
          Seg.drop_def target cells;
          Seg.drop_def (D.S (D.S (D.S (Heap.length values)))) (Heap.Cell (activation.Frame.accumulator, view.View.body));
          Seg.drop_def (D.S (D.S (Heap.length values))) view.View.body;
          Seg.drop_def (D.S (Heap.length values)) more;
          Store.cells_def (V.Word (Header.number old_pc)) activation.Frame.current activation.Frame.accumulator old_head old_tail remaining;
          Store.cells_def (V.Word (Header.number fragment.Lower.pc)) activation.Frame.current activation.Frame.accumulator head tail rest;
          Heap.length_def (Heap.Cell (V.Word (Header.number old_pc), cells)); Heap.length_def full;
          Hmc_wasm_list_finish_step.correct signature element activation next head tail fragment capacity max_pc env_count old_count old_pc
            old_head old_tail remaining env view.View.temporaries old old_padding padding state after base_local base slots before_frame after_frame suffix ();
          Hmc_frame_relayout_patch.associate (Seg.append env env) (Seg.append old Heap.Empty) padding;
          Hmc_frame_relayout_patch.associate env env (Seg.append (Seg.append old Heap.Empty) padding);
          Hmc_frame_relayout_patch.associate old Heap.Empty padding; Seg.append_def Heap.Empty padding;
          Hmc_wasm_range_four.length_append values padding;
          Heap.length_def after_cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, Heap.Cell (head, Heap.Cell (tail, rest))));
          Heap.length_def (Heap.Cell (head, Heap.Cell (tail, rest))); Heap.length_def (Heap.Cell (tail, rest));
          D.add_def target (Heap.length padding);
          D.add_def (D.S (D.S (D.S (Heap.length values)))) (Heap.length padding);
          D.add_def (D.S (D.S (Heap.length values))) (Heap.length padding);
          D.add_def (D.S (Heap.length values)) (Heap.length padding));
        {memory = after; cells = after_cells; padding; bytes = after_frame})
