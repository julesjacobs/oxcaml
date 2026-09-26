module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module Raw = Wasm_words_read
module Split = Wasm_word_shape_split
module Cross = Wasm_cross_words
module Copy = Wasm_cross_copy
module Plan = Wasm_parallel_copy
module Lower = Hmc_wasm_call_captures
module Range = Hmc_wasm_range_copy
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (capture_bytes @ total) (count : Hmc_wasm_relayout.count) : B.u32 = 16 * count
type result = {memory : B.bytes; captures : B.bytes; suffix : B.bytes}
let (correct @ total) : (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) -> (fragment : Lower.fragment) @ immutable ->
    (prefix : Q.words) @ immutable -> (offset : B.u32) -> (state : X.state) @ immutable ->
    (object_base : B.u32) -> (frame_base : B.u32) -> (limit : B.u32) -> (object_local : B.u32) -> (frame_local : B.u32) ->
    {u : unit | Index.represents (Heap.length captures) count && Lower.position fragment.Lower.recursive + count <= 268435452
      && offset = 16 + 16 * Lower.position fragment.Lower.recursive && Q.size prefix offset
      && frame_base + offset + 16 * count <= limit
      && Hmc_wasm_relayout.range_is fragment.Lower.copies 0 (Lower.position fragment.Lower.recursive) (Heap.length captures) Plan.End
      && Range.reads state.X.memory object_base (Hmc_wasm_closure_read.position ()) captures
      && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 object_base)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)} ->
    {out : result | X.run (Lower.emit fragment object_local frame_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit
      && P.equal_prefix (S.add32 frame_base offset) state.X.memory out.memory
      && Bytes.drop out.memory (S.add32 frame_base offset) === Some out.captures
      && Wire.decode_cells (Heap.length captures) out.captures === Some (captures, out.suffix)
      && Bytes.drop state.X.memory (S.add32 frame_base (S.add32 offset (capture_bytes count))) === Some out.suffix
      && Bytes.drop out.memory (S.add32 frame_base (S.add32 offset (capture_bytes count))) === Some out.suffix} @ immutable =
  fun captures count fragment prefix offset state object_base frame_base limit object_local frame_local premise ->
    ghost_ (capture_bytes_def count);
    let width : B.u32 = offset + 16 * count in
    let stop : B.u32 = frame_base + width in
    let _end = Bounds.suffix state.X.memory limit stop () in
    ghost_ (Bounds.covers_def state.X.memory stop);
    let before = Bounds.suffix state.X.memory stop frame_base () in
    let values = Words.words captures in
    let layout = Q.append prefix values in
    ghost_ (Bounds.distance_def frame_base stop; Words.size captures count (16 * count) ();
      Wasm_word_sequence_algebra.size prefix values offset (16 * count) width ());
    let raw = Raw.read layout before width () in
    let split = Split.split prefix values raw.Raw.words offset () in
    let after_words = Q.append split.Split.prefix values in
    let bytes = Q.encode after_words raw.Raw.suffix in
    let memory = Wasm_memory_splice.replace state.X.memory frame_base before bytes () in
    ghost_ (Hmc_wasm_closure_read.position_def ();
      Hmc_wasm_cross_range_words.correct captures fragment.Lower.copies Plan.End 0 (Lower.position fragment.Lower.recursive) count state.X.memory object_base ();
      Range.tag_def 0; Range.tag_def (Lower.position fragment.Lower.recursive);
      Copy.apply_def Plan.End state.X.memory object_base frame_base;
      Cross.correct split.Split.rest values split.Split.prefix fragment.Lower.copies Plan.End 16 offset state.X.memory state.X.memory memory object_base frame_base before bytes raw.Raw.suffix (16 * count) ();
      Copy.correct fragment.Lower.copies object_local frame_local state object_base frame_base memory ();
      Lower.emit_def fragment object_local frame_local;
      Wasm_word_sequence_algebra.size split.Split.prefix values offset (16 * count) width ();
      Q.prefix raw.Raw.words before before raw.Raw.suffix raw.Raw.suffix width ();
      Q.prefix after_words bytes bytes raw.Raw.suffix raw.Raw.suffix width ();
      Wasm_cell.shift state.X.memory frame_base width stop before ();
      Wasm_cell.shift memory frame_base width stop bytes ());
    let old_captured = Q.split split.Split.prefix split.Split.rest before raw.Raw.suffix () in
    let captured = Q.split split.Split.prefix values bytes raw.Raw.suffix () in
    ghost_ (Q.prefix split.Split.prefix before bytes old_captured captured offset ();
      Hmc_linear_prefix_join.correct state.X.memory memory frame_base offset (frame_base + offset) before bytes ();
      Wasm_cell.shift memory frame_base offset (frame_base + offset) bytes ();
      Words.recover captures captured raw.Raw.suffix ();
      S.add32_def frame_base offset; S.add32_def offset (16 * count); S.add32_def frame_base width;
      Bounds.same_length state.X.memory memory limit ());
    {memory; captures = captured; suffix = raw.Raw.suffix}
