module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wasm_wire_words
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Lit = Hmc_wasm_literal_load
let (correct @ total) : (count : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (current : V.value) @ immutable -> (old : V.value) @ immutable -> (value : V.value) @ immutable ->
    (cells : Heap.cells) @ immutable -> (state : X.state) @ immutable -> (after : B.bytes) @ immutable ->
    (tail : B.bytes) @ immutable -> (base_local : B.u32) -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame
        === Some (Wire.Closure (old_pc, Heap.Cell (current, Heap.Cell (old, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame
        === Some (Wire.Closure (pc, Heap.Cell (current, Heap.Cell (value, cells))), tail)} ->
    {u : unit | Wasm_frame_literals.apply (Lit.writes pc value) state.X.memory base === Some after
      && X.run (Lit.emit pc value base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun count old_pc pc current old value cells state after tail base_local base before_frame after_frame premise -> ghost_ (
    let left = Words.header count before_frame old_pc current old cells tail () in
    let right = Words.header count after_frame pc current value cells tail () in
    Words.cells_unique count left right cells tail ();
    Header.correct (Header.number old_pc) pc current old value state after left base_local base before_frame after_frame ())
