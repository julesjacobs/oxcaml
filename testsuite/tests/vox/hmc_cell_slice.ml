module D = Hm_declarative
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Simple = Hmc_heap_simple
let rec (next @ total) : (index : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | Seg.drop (D.S index) cells === (match Seg.drop index cells with Some (Heap.Cell (_, rest)) -> Some rest | _ -> None)} @ ghost =
  fun index cells -> ghost_ (
    Seg.drop_def index cells; Seg.drop_def (D.S index) cells;
    match index, cells with
    | D.Z, Heap.Cell (_, rest) -> Seg.drop_def D.Z rest
    | D.S n, Heap.Cell (_, rest) -> next n rest
    | _ -> ())
let rec (lookup @ total) : (index : D.index) @ immutable -> (cells : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.drop index cells === Some tail} ->
    {u : unit | Simple.lookup cells index === (match tail with Heap.Empty -> None | Heap.Cell (value, _) -> Some value)} @ ghost =
  fun index cells tail premise -> ghost_ (
    Seg.drop_def index cells; Simple.lookup_def cells index;
    match index, cells with D.S n, Heap.Cell (_, rest) -> lookup n rest tail () | _ -> ())
let rec (take_length @ total) : (count : D.index) @ immutable -> (cells : Heap.cells) @ immutable -> (prefix : Heap.cells) @ immutable ->
    {u : unit | Seg.take count cells === Some prefix} -> {u : unit | Heap.length prefix === count} @ ghost =
  fun count cells prefix premise -> ghost_ (
    Seg.take_def count cells;
    match count, cells, prefix with
    | D.Z, _, _ -> Heap.length_def prefix
    | D.S n, Heap.Cell (_, rest), Heap.Cell (_, more) -> Heap.length_def prefix; take_length n rest more ()
    | _ -> ())
