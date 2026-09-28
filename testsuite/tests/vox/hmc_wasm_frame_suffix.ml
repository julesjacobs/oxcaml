module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module L = Hmc_linear_bytes
module Index = Hmc_u32_index
let (suffix @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (stop : B.u32) ->
    (count : B.u32) -> (cells : Heap.cells) @ immutable -> (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | stop = base + 16 * count && Index.represents (Heap.length cells) count
      && L.drop memory base === Some bytes && Wire.decode_cells (Heap.length cells) bytes === Some (cells, tail)} ->
    {u : unit | L.drop memory stop === Some tail} @ ghost =
  fun memory base stop count cells bytes tail premise -> ghost_ (
    Words.decode (Heap.length cells) bytes cells tail ();
    Words.size cells count (16 * count) ();
    Q.prefix (Words.words cells) bytes bytes tail tail (16 * count) ();
    Wasm_cell.shift memory base (16 * count) stop bytes ())
let (preserve_heap @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (heap : Heap.heap) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (count : B.u32) ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable ->
    (before_bytes : B.bytes) @ immutable -> (after_bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | stop = base + 16 * count && Index.represents (Heap.length before_cells) count
      && Heap.length after_cells === Heap.length before_cells
      && L.drop before base === Some before_bytes && L.drop after base === Some after_bytes
      && Wire.decode_cells (Heap.length before_cells) before_bytes === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_bytes === Some (after_cells, tail)
      && Hmc_heap_image.related before heap && Hmc_heap_image_suffix.above heap stop} ->
    {u : unit | Hmc_heap_image.related after heap && L.drop before stop === L.drop after stop} @ ghost =
  fun before after heap base stop count before_cells after_cells before_bytes after_bytes tail premise -> ghost_ (
    suffix before base stop count before_cells before_bytes tail ();
    suffix after base stop count after_cells after_bytes tail ();
    Hmc_heap_image_suffix.preserve before after heap stop ())
