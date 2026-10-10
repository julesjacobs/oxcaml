module B = Wasm_u32
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
type result = {bytes : B.bytes; tail : B.bytes}
let (correct @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (stop : B.u32) -> (count : B.u32) -> (cells : Heap.cells) @ immutable ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | stop = base + 16 * count && Index.represents (Heap.length cells) count
      && P.equal_prefix stop before after && L.drop before base === Some bytes
      && Wire.decode_cells (Heap.length cells) bytes === Some (cells, tail)} ->
    {out : result | L.drop after base === Some out.bytes
      && Wire.decode_cells (Heap.length cells) out.bytes === Some (cells, out.tail)} @ immutable =
  fun before after base stop count cells bytes tail premise ->
    ghost_ (P.seek before after stop base (); Bounds.distance_def base stop);
    match L.drop after base with
    | None -> unreachable_ ()
    | Some after_bytes ->
      ghost_ (Words.decode (Heap.length cells) bytes cells tail (); Words.size cells count (16 * count) ());
      let after_tail = Wasm_word_transport.sequence (Words.words cells) bytes after_bytes tail (16 * count) () in
      ghost_ (Words.recover cells after_bytes after_tail ());
      {bytes = after_bytes; tail = after_tail}
