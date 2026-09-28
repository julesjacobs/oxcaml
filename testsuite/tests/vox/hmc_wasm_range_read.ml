module B = Wasm_u32
module D = Hm_declarative
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Slice = Hmc_cell_slice
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Range = Hmc_wasm_range_copy
let rec (correct @ total) : (values : Heap.cells) @ immutable -> (position : Lower.count) -> (count : Lower.count) ->
    (index : D.index) @ immutable -> (source_cells : Heap.cells) @ immutable -> (start : Heap.cells) @ immutable ->
    (memory : B.bytes) @ immutable -> (base : B.u32) -> (frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Index.represents index position && Index.represents (Heap.length values) count
      && position + count <= 268435452 && base + 16 + 16 * (position + count) <= 4294967296
      && Seg.drop (D.S index) source_cells === Some start && Seg.take (Heap.length values) start === Some values
      && Hmc_linear_bytes.drop memory base === Some frame
      && Wire.decode_cells (Heap.length source_cells) frame === Some (source_cells, tail)} ->
    {u : unit | Range.reads memory base position values} @ ghost =
  fun values position count index source_cells start memory base frame tail premise -> ghost_ (
    Heap.length_def values; Index.represents_def (Heap.length values) count;
    Range.reads_def memory base position values; Seg.take_def (Heap.length values) start;
    match values, start with
    | Heap.Cell (value, rest), Heap.Cell (_, more) ->
      Slice.lookup (D.S index) source_cells start ();
      Index.represents_def (D.S index) (position + 1);
      Range.tag_def position; Range.payload_def position;
      Hmc_wasm_cells_read.correct memory base frame (Heap.length source_cells) source_cells tail
        (D.S index) (position + 1) (Range.tag position) (Range.payload position) value ();
      Slice.next (D.S index) source_cells;
      correct rest (position + 1) (count - 1) (D.S index) source_cells more memory base frame tail ()
    | _ -> ())
