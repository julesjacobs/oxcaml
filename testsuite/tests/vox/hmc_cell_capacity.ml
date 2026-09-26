module D = Hm_declarative
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module W = Hmc_word64
type slice = {prefix : Heap.cells; suffix : Heap.cells}
let rec (split @ total) : (count : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | Cap.le count (Heap.length cells)} ->
    {out : slice | Seg.take count cells === Some out.prefix && Seg.drop count cells === Some out.suffix
      && Heap.length out.prefix === count && Seg.append out.prefix out.suffix === cells
      && D.add count (Heap.length out.suffix) === Heap.length cells} @ immutable = fun count cells premise ->
  ghost_ (Heap.length_def cells; Cap.le_def count (Heap.length cells); Seg.take_def count cells; Seg.drop_def count cells);
  match count, cells with
  | D.Z, _ ->
    ghost_ (Heap.length_def Heap.Empty; Seg.append_def Heap.Empty cells; D.add_def D.Z (Heap.length cells));
    {prefix = Heap.Empty; suffix = cells}
  | D.S n, Heap.Cell (value, rest) ->
    let cut = split n rest () in
    let prefix = Heap.Cell (value, cut.prefix) in
    ghost_ (Heap.length_def prefix; Seg.append_def prefix cut.suffix; D.add_def count (Heap.length cut.suffix));
    {prefix; suffix = cut.suffix}
  | _ -> unreachable_ ()
let rec (numeric @ total) : (used : D.index) @ immutable -> (capacity : D.index) @ immutable ->
    (n : W.limb) -> (m : W.limb) ->
    {u : unit | Index.represents used n && Index.represents capacity m && n <= m} ->
    {u : unit | Cap.le used capacity} @ ghost = fun used capacity n m premise -> ghost_ (
    Index.represents_def used n; Index.represents_def capacity m; Cap.le_def used capacity;
    match used, capacity with D.S rest, D.S remaining -> numeric rest remaining (n - 1) (m - 1) () | _ -> ())
let rec (cut @ total) : (count : D.index) @ immutable -> (cells : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Seg.drop count cells === Some tail} ->
    {head : Heap.cells | Seg.take count cells === Some head && Seg.append head tail === cells && Heap.length head === count} @ immutable =
  fun count cells tail premise ->
    ghost_ (Seg.drop_def count cells; Seg.take_def count cells);
    match count, cells with
    | D.Z, _ -> ghost_ (Seg.append_def Heap.Empty tail; Heap.length_def Heap.Empty); Heap.Empty
    | D.S n, Heap.Cell (value, rest) ->
      let remaining = cut n rest tail () in
      let head = Heap.Cell (value, remaining) in
      ghost_ (Seg.append_def head tail; Heap.length_def head); head
    | _ -> unreachable_ ()
