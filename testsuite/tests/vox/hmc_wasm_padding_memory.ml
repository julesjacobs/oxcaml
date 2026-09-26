module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Pad = Hmc_wasm_frame_padding
module Write = Wasm_mixed_write
module Words = Hmc_wire_word_sequence
module Wire = Hmc_heap_wire
module Q = Wasm_word_sequence
module Memory = Wasm_mixed_range_memory
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
let[@def] (offset @ total) (position : R.count) : B.u32 = 16 * position
let (correct @ total) : (writes : Write.writes) @ immutable -> (position : R.count) -> (count : R.count) ->
    (n : D.index) @ immutable -> (prefix : Q.words) @ immutable -> (state : X.state) @ immutable ->
    (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    {u : unit | Index.represents n count && position + count <= 268435452 && Pad.matches writes position n
      && Q.size prefix (offset position) && base + 16 * position + 16 * count <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : Memory.result | X.run (Write.emit writes base_local) state === X.Done {X.memory = out.Memory.memory; machine = state.X.machine}
      && Wire.decode_cells n out.Memory.bytes === Some (Pad.cells n, out.Memory.suffix)
      && Bytes.drop out.Memory.memory (S.add32 base (offset position)) === Some out.Memory.bytes
      && P.equal_prefix (S.add32 base (offset position)) state.X.memory out.Memory.memory
      && V.length out.Memory.memory === V.length state.X.memory && Bounds.covers out.Memory.memory limit
      && Bytes.drop state.X.memory (S.add32 base (S.add32 (offset position) (offset count))) === Some out.Memory.suffix
      && Bytes.drop out.Memory.memory (S.add32 base (S.add32 (offset position) (offset count))) === Some out.Memory.suffix} @ immutable =
  fun writes position count n prefix state base limit base_local premise ->
    ghost_ (offset_def position; offset_def count; Pad.length n;
      Pad.layout writes position count n state.X.machine.E.locals (offset position) ();
      Words.size (Pad.cells n) count (offset count) ());
    let result = Memory.correct writes (Words.words (Pad.cells n)) (offset count) prefix (offset position) state base limit base_local () in
    ghost_ (Words.recover (Pad.cells n) result.Memory.bytes result.Memory.suffix ());
    result
