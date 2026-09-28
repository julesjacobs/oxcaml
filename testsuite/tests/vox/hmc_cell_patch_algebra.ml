module D = Hm_declarative
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
let rec (zero @ total) : (index : D.index) @ immutable -> {u : unit | D.add index D.Z === index} @ ghost =
  fun index -> ghost_ (D.add_def index D.Z; match index with D.Z -> () | D.S n -> zero n)
let rec (successor @ total) : (index : D.index) @ immutable -> (count : D.index) @ immutable ->
    {u : unit | D.add index (D.S count) === D.S (D.add index count)} @ ghost = fun index count -> ghost_ (
    D.add_def index (D.S count); D.add_def index count; match index with D.Z -> () | D.S n -> successor n count)
let rec (append @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    (index : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | Patch.write index (Seg.append first second) cells ===
      (match Patch.write (D.add index (Heap.length first)) second cells with None -> None | Some middle -> Patch.write index first middle)} @ ghost =
  fun first second index cells -> ghost_ (
    Seg.append_def first second; Heap.length_def first;
    match first with
    | Heap.Empty ->
      zero index;
      (match Patch.write index second cells with None -> () | Some middle -> Patch.write_def index Heap.Empty middle)
    | Heap.Cell (value, rest) ->
      Patch.cons index value (Seg.append rest second) cells;
      append rest second (D.S index) cells;
      successor index (Heap.length rest); D.add_def (D.S index) (Heap.length rest);
      (match Patch.write (D.add index (Heap.length first)) second cells with
      | None -> () | Some middle -> Patch.cons index value rest middle))
let rec (join @ total) : (index : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    (prefix : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.take index cells === Some prefix && Seg.drop index cells === Some tail} ->
    {u : unit | Seg.append prefix tail === cells} @ ghost = fun index cells prefix tail premise -> ghost_ (
    Seg.take_def index cells; Seg.drop_def index cells; Seg.append_def prefix tail;
    match index, cells, prefix with
    | D.S n, Heap.Cell (_, more), Heap.Cell (_, rest) -> join n more rest tail ()
    | _ -> ())
let rec (overlay @ total) : (values : Heap.cells) @ immutable -> (cells : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.drop (Heap.length values) cells === Some tail} ->
    {u : unit | Patch.overlay values cells === Some (Seg.append values tail)} @ ghost = fun values cells tail premise -> ghost_ (
    Heap.length_def values; Seg.drop_def (Heap.length values) cells; Patch.overlay_def values cells; Seg.append_def values tail;
    match values, cells with Heap.Cell (_, rest), Heap.Cell (_, more) -> overlay rest more tail () | _ -> ())
let rec (block @ total) : (index : D.index) @ immutable -> (values : Heap.cells) @ immutable ->
    (cells : Heap.cells) @ immutable -> (prefix : Heap.cells) @ immutable -> (body : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.take index cells === Some prefix && Seg.drop index cells === Some body
      && Seg.drop (Heap.length values) body === Some tail} ->
    {u : unit | Patch.write index values cells === Some (Seg.append prefix (Seg.append values tail))} @ ghost =
  fun index values cells prefix body tail premise -> ghost_ (
    Patch.write_def index values cells;
    match values with
    | Heap.Empty -> Heap.length_def values; Seg.drop_def D.Z body; Seg.append_def values tail; join index cells prefix tail ()
    | _ ->
      Seg.take_def index cells; Seg.drop_def index cells; Seg.append_def prefix (Seg.append values tail);
      (match index, cells, prefix with
      | D.Z, _, _ -> overlay values cells tail ()
      | D.S n, Heap.Cell (_, more), Heap.Cell (_, rest) -> block n values more rest body tail ()
      | _ -> ()))
