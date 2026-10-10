module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
let[@def] rec (set @ total) (index : D.index @ immutable) (cells : Heap.cells @ immutable) (value : V.value @ immutable) =
  match index, cells with
  | D.Z, Heap.Cell (_, rest) -> Some (Heap.Cell (value, rest))
  | D.S n, Heap.Cell (head, rest) -> (match set n rest value with None -> None | Some after -> Some (Heap.Cell (head, after)))
  | _ -> None
let[@def] rec (overlay @ total) (values : Heap.cells @ immutable) (cells : Heap.cells @ immutable) = match values, cells with
  | Heap.Empty, _ -> Some cells
  | Heap.Cell (value, rest), Heap.Cell (_, more) -> (match overlay rest more with None -> None | Some after -> Some (Heap.Cell (value, after)))
  | _ -> None
let[@def] rec (write @ total) (index : D.index @ immutable) (values : Heap.cells @ immutable) (cells : Heap.cells @ immutable) =
  match values with
  | Heap.Empty -> Some cells
  | _ -> (match index, cells with
    | D.Z, _ -> overlay values cells
    | D.S n, Heap.Cell (head, rest) -> (match write n values rest with None -> None | Some after -> Some (Heap.Cell (head, after)))
    | _ -> None)
let rec (cons @ total) : (index : D.index) @ immutable -> (value : V.value) @ immutable ->
    (rest : Heap.cells) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | write index (Heap.Cell (value, rest)) cells ===
      (match write (D.S index) rest cells with None -> None | Some middle -> set index middle value)} @ ghost =
  fun index value rest cells -> ghost_ (
    write_def index (Heap.Cell (value, rest)) cells; write_def (D.S index) rest cells;
    match index, cells with
    | D.Z, _ ->
      overlay_def (Heap.Cell (value, rest)) cells;
      (match cells with
      | Heap.Empty -> (match rest with Heap.Empty -> set_def D.Z cells value | _ -> ())
      | Heap.Cell (head, more) ->
        write_def D.Z rest more;
        (match rest with Heap.Empty -> overlay_def rest more; set_def D.Z cells value
        | _ -> (match overlay rest more with None -> () | Some after -> set_def D.Z (Heap.Cell (head, after)) value)))
    | D.S n, Heap.Cell (head, more) ->
      cons n value rest more;
      (match rest with
      | Heap.Empty -> write_def (D.S n) rest more; set_def index cells value; write_def n (Heap.Cell (value, rest)) more;
        overlay_def (Heap.Cell (value, rest)) more
      | _ -> (match write (D.S n) rest more with None -> () | Some after -> set_def index (Heap.Cell (head, after)) value))
    | _, _ -> (match rest with Heap.Empty -> set_def index cells value | _ -> ()))
type split = {prefix : Heap.cells; old : V.value; rest : Heap.cells}
let rec (split @ total) : (index : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    (value : V.value) @ immutable -> (after : Heap.cells) @ immutable -> {u : unit | set index cells value === Some after} ->
    {out : split | Heap.length out.prefix === index && cells === Seg.append out.prefix (Heap.Cell (out.old, out.rest))
      && after === Seg.append out.prefix (Heap.Cell (value, out.rest))} @ immutable = fun index cells value after premise ->
  ghost_ (set_def index cells value);
  match index, cells with
  | D.Z, Heap.Cell (old, rest) ->
    ghost_ (Heap.length_def Heap.Empty; Seg.append_def Heap.Empty (Heap.Cell (old, rest)); Seg.append_def Heap.Empty (Heap.Cell (value, rest)));
    {prefix = Heap.Empty; old; rest}
  | D.S n, Heap.Cell (head, more) -> (match set n more value with
    | None -> unreachable_ ()
    | Some remaining ->
      let previous = split n more value remaining () in
      let prefix = Heap.Cell (head, previous.prefix) in
      ghost_ (Heap.length_def prefix; Seg.append_def prefix (Heap.Cell (previous.old, previous.rest));
        Seg.append_def prefix (Heap.Cell (value, previous.rest)));
      {prefix; old = previous.old; rest = previous.rest})
  | _ -> unreachable_ ()
