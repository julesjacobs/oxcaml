module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module A = Wasm_word_sequence_algebra
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
module Splice = Wasm_memory_splice
module Copy = Wasm_parallel_copy
let (correct @ total) : (prefix : Heap.cells) @ immutable -> (rest : Heap.cells) @ immutable ->
    (old : V.value) @ immutable -> (value : V.value) @ immutable -> (count : W.limb) ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) -> (offset : B.u32) -> (payload_offset : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (source : B.bytes) @ immutable -> (source_tag : B.u32) -> (source_payload : B.u32) -> (remaining : Copy.plan) @ immutable ->
    {u : unit | Copy.apply remaining source base === Some before
      && M.load source base source_tag M.W64 === Some (S.I64 (V.tag value))
      && M.load source base source_payload M.W64 === Some (S.I64 (V.payload value))
      && Hmc_u32_index.represents (Heap.length prefix) count && offset = 16 * count && payload_offset = offset + 8
      && base + offset <= 4294967280 && P.equal_prefix base before after
      && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length (Seg.append prefix (Heap.Cell (old, rest)))) before_frame
        === Some (Seg.append prefix (Heap.Cell (old, rest)), tail)
      && Wire.decode_cells (Heap.length (Seg.append prefix (Heap.Cell (value, rest)))) after_frame
        === Some (Seg.append prefix (Heap.Cell (value, rest)), tail)} ->
    {middle : B.bytes | M.store before base payload_offset (S.I64 (V.payload value)) === Some middle
      && M.store middle base offset (S.I64 (V.tag value)) === Some after
      && Copy.apply (Copy.Copy (source_tag, offset, Copy.Copy (source_payload, payload_offset, remaining))) source base === Some after} @ immutable =
  fun prefix rest old value count before after base offset payload_offset before_frame after_frame tail source source_tag source_payload remaining premise ->
    let p = Words.words prefix in let r = Words.words rest in
    let old_tag = V.tag old in let new_tag = V.tag value in
    let old_payload = V.payload old in let new_payload = V.payload value in
    let middle_words = Q.append p (Q.Word (old_tag, Q.Word (new_payload, r))) in
    let middle_frame = Q.encode middle_words tail in
    let middle = Splice.replace before base before_frame middle_frame () in
    ghost_ (
      Words.decode (Heap.length (Seg.append prefix (Heap.Cell (old, rest)))) before_frame (Seg.append prefix (Heap.Cell (old, rest))) tail ();
      Words.decode (Heap.length (Seg.append prefix (Heap.Cell (value, rest)))) after_frame (Seg.append prefix (Heap.Cell (value, rest))) tail ();
      Words.append prefix (Heap.Cell (old, rest)); Words.append prefix (Heap.Cell (value, rest));
      Words.words_def (Heap.Cell (old, rest)); Words.words_def (Heap.Cell (value, rest));
      Words.size prefix count offset ();
      let tag_prefix = Q.Word (old_tag, Q.End) in
      let payload_prefix = Q.append p tag_prefix in
      Q.size_def tag_prefix 8; Q.size_def Q.End 0; A.size p tag_prefix offset 8 payload_offset ();
      A.associative p tag_prefix (Q.Word (old_payload, r)); A.associative p tag_prefix (Q.Word (new_payload, r));
      Q.append_def tag_prefix (Q.Word (old_payload, r)); Q.append_def Q.End (Q.Word (old_payload, r));
      Q.append_def tag_prefix (Q.Word (new_payload, r)); Q.append_def Q.End (Q.Word (new_payload, r));
      Wasm_sequence_update.at payload_prefix r before middle before_frame middle_frame tail old_payload new_payload base payload_offset (base + payload_offset) ();
      Splice.shared before middle after base ();
      Wasm_sequence_update.at p (Q.Word (new_payload, r)) middle after middle_frame after_frame tail old_tag new_tag base offset (base + offset) ();
      Copy.apply_def (Copy.Copy (source_tag, offset, Copy.Copy (source_payload, payload_offset, remaining))) source base;
      Copy.apply_def (Copy.Copy (source_payload, payload_offset, remaining)) source base);
    middle
