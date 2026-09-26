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
module Pad_memory = Hmc_wasm_padding_memory
module Memory = Wasm_mixed_range_memory
module Seg = Hmc_frame_segments
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (writes : Write.writes) @ immutable -> (prefix : H.cells) @ immutable -> (position : R.count) ->
    (count : R.count) -> (n : D.index) @ immutable -> (state : X.state) @ immutable ->
    (base : B.u32) -> (limit : B.u32) -> (before : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (base_local : B.u32) ->
    {u : unit | Index.represents (H.length prefix) position && Index.represents n count && position + count <= 268435452
      && Pad.matches writes position n && base + 16 * position + 16 * count <= limit && Bounds.covers state.X.memory limit
      && Bytes.drop state.X.memory base === Some before && Wire.decode_cells (H.length prefix) before === Some (prefix, tail)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : result | X.run (Write.emit writes base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (H.length (Seg.append prefix (Pad.cells n))) out.bytes === Some (Seg.append prefix (Pad.cells n), out.suffix)
      && P.equal_prefix base state.X.memory out.memory && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit
      && Bytes.drop state.X.memory (S.add32 base (S.add32 (Pad_memory.offset position) (Pad_memory.offset count))) === Some out.suffix
      && Bytes.drop out.memory (S.add32 base (S.add32 (Pad_memory.offset position) (Pad_memory.offset count))) === Some out.suffix} @ immutable =
  fun writes prefix position count n state base limit before tail base_local premise ->
    ghost_ (Pad_memory.offset_def position; Pad_memory.offset_def count; Words.size prefix position (16 * position) ());
    let padded = Pad_memory.correct writes position count n (Words.words prefix) state base limit base_local () in
    let bytes = Bounds.suffix padded.Memory.memory limit base () in
    ghost_ (S.add32_def base (16 * position);
      P.seek state.X.memory padded.Memory.memory (base + 16 * position) base ();
      Bounds.distance_def base (base + 16 * position);
      Words.decode (H.length prefix) before prefix tail ());
    let middle = Wasm_word_transport.sequence (Words.words prefix) before bytes tail (16 * position) () in
    ghost_ (Q.prefix (Words.words prefix) bytes bytes middle middle (16 * position) ();
      Wasm_cell.shift padded.Memory.memory base (16 * position) (base + 16 * position) bytes ();
      Words.recover prefix bytes middle (); Pad.length n;
      Hmc_wire_cells_join.correct prefix (Pad.cells n) bytes middle padded.Memory.suffix ();
      P.shrink (base + 16 * position) base state.X.memory padded.Memory.memory ());
    {memory = padded.Memory.memory; bytes; suffix = padded.Memory.suffix}
