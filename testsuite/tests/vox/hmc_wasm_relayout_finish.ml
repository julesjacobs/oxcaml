module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Lower = Hmc_wasm_relayout
module PC = Hmc_wasm_pc_update
module Header = Hmc_wasm_header_update
module Copy = Wasm_parallel_copy
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
let (closure @ total) : (pc : W.limb) -> (cells : Heap.cells) @ immutable ->
    (frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells (D.S (Heap.length cells)) frame === Some (Heap.Cell (V.Word (Header.number pc), cells), tail)} ->
    {u : unit | Wire.decode (Wire.Closure_schema (Heap.length cells)) frame === Some (Wire.Closure (pc, cells), tail)} @ ghost =
  fun pc cells frame tail premise -> ghost_ (
    Header.number_def pc; Wire.decode_cells_def (D.S (Heap.length cells)) frame;
    Wire.decode_def (Wire.Closure_schema (Heap.length cells)) frame)
let (correct @ total) : (fragment : Lower.fragment) @ immutable -> (state : X.state) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (old_pc : W.limb) ->
    (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (body : Heap.cells) @ immutable ->
    (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (copied_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base <= 4294967280 && Copy.apply fragment.Lower.copies state.X.memory base === Some copied
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && P.equal_prefix base copied after && Bytes.drop copied base === Some copied_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) copied_frame ===
        Some (Heap.Cell (V.Word (Header.number old_pc), Heap.Cell (current, Heap.Cell (accumulator, body))), tail)
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) after_frame ===
        Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), Heap.Cell (current, Heap.Cell (accumulator, body))), tail)} ->
    {u : unit | X.run (Lower.emit fragment base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun fragment state base_local base old_pc current accumulator body copied after copied_frame after_frame tail premise -> ghost_ (
    let cells = Heap.Cell (current, Heap.Cell (accumulator, body)) in
    Heap.length_def cells; Heap.length_def (Heap.Cell (accumulator, body));
    closure old_pc cells copied_frame tail (); closure fragment.Lower.pc cells after_frame tail ();
    PC.memory (Heap.length body) old_pc fragment.Lower.pc current accumulator body copied after tail base copied_frame after_frame ();
    Lower.finish_def copied base fragment.Lower.pc;
    Lower.correct fragment base_local state base copied after ())
