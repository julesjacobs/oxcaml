module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
let rec (correct @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    (bytes : B.bytes) @ immutable -> (middle : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells (Heap.length first) bytes === Some (first, middle)
      && Wire.decode_cells (Heap.length second) middle === Some (second, tail)} ->
    {u : unit | Wire.decode_cells (Heap.length (Seg.append first second)) bytes === Some (Seg.append first second, tail)} @ ghost =
  fun first second bytes middle tail premise -> ghost_ (
    Heap.length_def first; Seg.append_def first second; Heap.length_def (Seg.append first second);
    Wire.decode_cells_def (Heap.length first) bytes;
    Wire.decode_cells_def (Heap.length (Seg.append first second)) bytes;
    match first with
    | Heap.Empty -> ()
    | Heap.Cell (_, rest) -> (match V.decode bytes with
      | None -> () | Some (_, remaining) -> correct rest second remaining middle tail ()))
