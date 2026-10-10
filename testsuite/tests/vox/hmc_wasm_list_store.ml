module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Index = Hmc_u32_index
module Capture = Hmc_wasm_list_capture
module Finish = Hmc_wasm_list_finish
module Write = Wasm_frame_write
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Splice = Wasm_memory_splice
let[@def] (cells @ total) (pc : V.value @ immutable) (current : V.value @ immutable) (accumulator : V.value @ immutable)
    (head : V.value @ immutable) (tail : V.value @ immutable) (rest : Heap.cells @ immutable) =
  Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Cell (tail, rest)))))
let (correct @ total) : (pc : V.value) @ immutable -> (current : V.value) @ immutable -> (accumulator : V.value) @ immutable ->
    (old_head : V.value) @ immutable -> (old_tail : V.value) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (rest : Heap.cells) @ immutable -> (locals : S.stack) @ immutable -> (slots : Capture.slots) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967216 && P.equal_prefix base before after
      && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length (cells pc current accumulator old_head old_tail rest)) before_frame
        === Some (cells pc current accumulator old_head old_tail rest, suffix)
      && Wire.decode_cells (Heap.length (cells pc current accumulator head tail rest)) after_frame
        === Some (cells pc current accumulator head tail rest, suffix)
      && L.get locals slots.Capture.head_tag === Some (S.I64 (V.tag head))
      && L.get locals slots.Capture.head_payload === Some (S.I64 (V.payload head))
      && L.get locals slots.Capture.tail_tag === Some (S.I64 (V.tag tail))
      && L.get locals slots.Capture.tail_payload === Some (S.I64 (V.payload tail))} ->
    {u : unit | Write.apply (Finish.writes slots) before base locals === Some after} @ ghost =
  fun pc current accumulator old_head old_tail head tail rest locals slots before after base before_frame after_frame suffix premise -> ghost_ (
    cells_def pc current accumulator old_head old_tail rest; cells_def pc current accumulator head tail rest;
    cells_def pc current accumulator head old_tail rest;
    let intermediate_cells = cells pc current accumulator head old_tail rest in
    let intermediate_frame = Wire.encode_cells intermediate_cells suffix in
    let intermediate = Splice.replace before base before_frame intermediate_frame () in
    let prefix = Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty))) in
    let prefix_tail = Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty)))) in
    Heap.length_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty))));
    Heap.length_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty)));
    Heap.length_def (Heap.Cell (accumulator, Heap.Empty));
    Heap.length_def (Heap.Empty);
    Index.represents_def (D.Z) 0;
    Index.represents_def (D.S (D.Z)) 1;
    Index.represents_def (D.S (D.S (D.Z))) 2;
    Index.represents_def (D.S (D.S (D.S (D.Z)))) 3;
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty)))) (Heap.Cell (old_head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty))) (Heap.Cell (old_head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) (Heap.Cell (old_head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Empty) (Heap.Cell (old_head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty)))) (Heap.Cell (head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Empty))) (Heap.Cell (head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Cell (accumulator, Heap.Empty)) (Heap.Cell (head, Heap.Cell (old_tail, rest)));
    Seg.append_def (Heap.Empty) (Heap.Cell (head, Heap.Cell (old_tail, rest)));
    Heap.length_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty)))));
    Heap.length_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty))));
    Heap.length_def (Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty)));
    Heap.length_def (Heap.Cell (head, Heap.Empty));
    Heap.length_def (Heap.Empty);
    Index.represents_def (D.Z) 0;
    Index.represents_def (D.S (D.Z)) 1;
    Index.represents_def (D.S (D.S (D.Z))) 2;
    Index.represents_def (D.S (D.S (D.S (D.Z)))) 3;
    Index.represents_def (D.S (D.S (D.S (D.S (D.Z))))) 4;
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty))))) (Heap.Cell (old_tail, rest));
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty)))) (Heap.Cell (old_tail, rest));
    Seg.append_def (Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty))) (Heap.Cell (old_tail, rest));
    Seg.append_def (Heap.Cell (head, Heap.Empty)) (Heap.Cell (old_tail, rest));
    Seg.append_def (Heap.Empty) (Heap.Cell (old_tail, rest));
    Seg.append_def (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty))))) (Heap.Cell (tail, rest));
    Seg.append_def (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty)))) (Heap.Cell (tail, rest));
    Seg.append_def (Heap.Cell (accumulator, Heap.Cell (head, Heap.Empty))) (Heap.Cell (tail, rest));
    Seg.append_def (Heap.Cell (head, Heap.Empty)) (Heap.Cell (tail, rest));
    Seg.append_def (Heap.Empty) (Heap.Cell (tail, rest));
    let first = Hmc_wasm_cell_store.correct prefix (Heap.Cell (old_tail, rest)) old_head head 3
      before intermediate base 48 56 before_frame intermediate_frame suffix () in
    Splice.shared before intermediate after base ();
    let second = Hmc_wasm_cell_store.correct prefix_tail rest old_tail tail 4
      intermediate after base 64 72 intermediate_frame after_frame suffix () in
    Finish.writes_def slots;
    Write.apply_def (Finish.writes slots) before base locals;
    Write.apply_def (Write.Write (56, slots.Capture.head_payload, Write.Write (64, slots.Capture.tail_tag, Write.Write (72, slots.Capture.tail_payload, Write.End)))) first base locals;
    Write.apply_def (Write.Write (64, slots.Capture.tail_tag, Write.Write (72, slots.Capture.tail_payload, Write.End))) intermediate base locals;
    Write.apply_def (Write.Write (72, slots.Capture.tail_payload, Write.End)) second base locals;
    Write.apply_def Write.End after base locals)
