module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
let (shift @ total) : (index : D.index) @ immutable -> (values : Heap.cells) @ immutable ->
    (cells : Heap.cells) @ immutable -> (after : Heap.cells) @ immutable -> (head : V.value) @ immutable ->
    {u : unit | Patch.write index values cells === Some after} ->
    {u : unit | Patch.write (D.S index) values (Heap.Cell (head, cells)) === Some (Heap.Cell (head, after))} @ ghost =
  fun index values cells after head premise -> ghost_ (
    Patch.write_def index values cells; Patch.write_def (D.S index) values (Heap.Cell (head, cells));
    match values with Heap.Empty -> () | _ -> ())
let (drop @ total) : (index : D.index) @ immutable -> (pc : V.value) @ immutable ->
    (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (body : Heap.cells) @ immutable ->
    {u : unit | Seg.drop (D.S (D.S (D.S index))) (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, body)))) === Seg.drop index body} @ ghost =
  fun index pc current accumulator body -> ghost_ (
    Seg.drop_def (D.S (D.S (D.S index))) (Heap.Cell (pc, Heap.Cell (current, Heap.Cell (accumulator, body))));
    Seg.drop_def (D.S (D.S index)) (Heap.Cell (current, Heap.Cell (accumulator, body)));
    Seg.drop_def (D.S index) (Heap.Cell (accumulator, body)))
