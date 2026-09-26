module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Store = Hmc_wasm_list_store
module Finish = Hmc_wasm_list_finish
module Capture = Hmc_wasm_list_capture
module Lower = Hmc_wasm_relayout
module Copy = Wasm_parallel_copy
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
let (correct @ total) : (fragment : Lower.fragment) @ immutable -> (state : X.state) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable -> (old_pc : W.limb) ->
    (current : V.value) @ immutable -> (accumulator : V.value) @ immutable ->
    (old_head : V.value) @ immutable -> (old_tail : V.value) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (rest : Heap.cells) @ immutable -> (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (copied_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967216 && Copy.apply fragment.Lower.copies state.X.memory base === Some copied
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && P.equal_prefix base copied after && Bytes.drop copied base === Some copied_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length (Store.cells (V.Word (Header.number old_pc)) current accumulator old_head old_tail rest)) copied_frame
        === Some (Store.cells (V.Word (Header.number old_pc)) current accumulator old_head old_tail rest, suffix)
      && Wire.decode_cells (Heap.length (Store.cells (V.Word (Header.number fragment.Lower.pc)) current accumulator head tail rest)) after_frame
        === Some (Store.cells (V.Word (Header.number fragment.Lower.pc)) current accumulator head tail rest, suffix)
      && L.get state.X.machine.E.locals slots.Capture.head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals slots.Capture.head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals slots.Capture.tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals slots.Capture.tail_payload === Some (S.I64 (V.payload tail))} ->
    {u : unit | X.run (Finish.emit fragment base_local slots) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun fragment state base_local base slots old_pc current accumulator old_head old_tail head tail rest copied after copied_frame after_frame suffix premise -> ghost_ (
    let pc = V.Word (Header.number old_pc) in
    let body = Heap.Cell (head, Heap.Cell (tail, rest)) in
    let cells = Heap.Cell (current, Heap.Cell (accumulator, body)) in
    let written_cells = Store.cells pc current accumulator head tail rest in
    let written_frame = Wire.encode_cells written_cells suffix in
    let written = Splice.replace copied base copied_frame written_frame () in
    Store.correct pc current accumulator old_head old_tail head tail rest state.X.machine.E.locals slots copied written base copied_frame written_frame suffix ();
    Store.cells_def pc current accumulator head tail rest;
    Store.cells_def (V.Word (Header.number fragment.Lower.pc)) current accumulator head tail rest;
    Heap.length_def written_cells;
    Heap.length_def (Store.cells (V.Word (Header.number fragment.Lower.pc)) current accumulator head tail rest);
    Heap.length_def cells; Heap.length_def (Heap.Cell (accumulator, body));
    Hmc_wasm_relayout_finish.closure old_pc cells written_frame suffix ();
    Hmc_wasm_relayout_finish.closure fragment.Lower.pc cells after_frame suffix ();
    Splice.shared copied written after base ();
    Hmc_wasm_pc_update.memory (Heap.length body) old_pc fragment.Lower.pc current accumulator body written after suffix base written_frame after_frame ();
    Lower.finish_def written base fragment.Lower.pc;
    Finish.correct fragment base_local slots state base copied written after ())
