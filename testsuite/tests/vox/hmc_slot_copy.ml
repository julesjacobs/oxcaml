module D = Hm_declarative
module F = Hmc_frame_codec
module R = Hmc_closure_semantics

let[@def] rec (read @ total) (cells : F.cells @ immutable) (index : D.index @ immutable) = match cells, index with
  | F.Empty, _ -> None | F.Cell (value, _), D.Z -> Some value | F.Cell (_, rest), D.S n -> read rest n
let[@def] rec (write @ total) (cells : F.cells @ immutable) (index : D.index @ immutable) (value : R.V.value @ immutable) =
  match cells, index with
  | F.Empty, _ -> None
  | F.Cell (_, rest), D.Z -> Some (F.Cell (value, rest))
  | F.Cell (head, rest), D.S n -> (match write rest n value with None -> None | Some tail -> Some (F.Cell (head, tail)))
let rec (get @ total) : (cells : F.cells) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | D.present (F.length cells) index} -> {value : R.V.value | read cells index === Some value} @ immutable =
  fun cells index premise ->
    ghost_ (F.length_def cells; D.present_def (F.length cells) index; read_def cells index);
    match cells, index with F.Cell (head, _), D.Z -> head | F.Cell (_, rest), D.S n -> get rest n () | _ -> unreachable_ ()
let rec (put @ total) : (cells : F.cells) @ immutable -> (index : D.index) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | D.present (F.length cells) index} ->
    {out : F.cells | write cells index value === Some out && F.length out === F.length cells && read out index === Some value} @ immutable =
  fun cells index value premise ->
    ghost_ (F.length_def cells; D.present_def (F.length cells) index; write_def cells index value);
    match cells, index with
    | F.Cell (_, rest), D.Z ->
      let out = F.Cell (value, rest) in ghost_ (F.length_def out; read_def out index); out
    | F.Cell (head, rest), D.S n ->
      let tail = put rest n value () in let out = F.Cell (head, tail) in
      ghost_ (F.length_def out; read_def out index); out
    | _ -> unreachable_ ()

type moves = End | Move of D.index * D.index * moves [@@inductive]
type snapshot = Empty | Saved of D.index * R.V.value * snapshot [@@inductive]
let[@def] rec (moves_length @ total) (moves : moves @ immutable) = match moves with End -> D.Z | Move (_, _, rest) -> D.S (moves_length rest)
let[@def] rec (snapshot_length @ total) (snapshot : snapshot @ immutable) = match snapshot with Empty -> D.Z | Saved (_, _, rest) -> D.S (snapshot_length rest)
let[@def] rec (valid @ total) (source_size : D.index @ immutable) (target_size : D.index @ immutable) (moves : moves @ immutable) =
  match moves with End -> true | Move (src, dst, rest) -> D.present source_size src && D.present target_size dst && valid source_size target_size rest
let[@def] rec (captured @ total) (source : F.cells @ immutable) (moves : moves @ immutable) (snapshot : snapshot @ immutable) = ghost_ (
  match moves, snapshot with End, Empty -> true
  | Move (src, dst, rest), Saved (target, value, tail) -> target === dst && read source src === Some value && captured source rest tail
  | _ -> false)
let rec (capture @ total) : (source : F.cells) @ immutable -> (target_size : D.index) @ immutable -> (moves : moves) @ immutable ->
    {u : unit | valid (F.length source) target_size moves} ->
    {out : snapshot | captured source moves out && snapshot_length out === moves_length moves} @ immutable = fun source target_size moves premise ->
  ghost_ (valid_def (F.length source) target_size moves; moves_length_def moves);
  match moves with
  | End -> ghost_ (captured_def source moves Empty; snapshot_length_def Empty); Empty
  | Move (src, dst, rest) ->
    let value = get source src () in let tail = capture source target_size rest () in
    let out = Saved (dst, value, tail) in ghost_ (captured_def source moves out; snapshot_length_def out); out
let[@def] rec (parallel @ total) (source : F.cells @ immutable) (target : F.cells @ immutable) (moves : moves @ immutable) =
  match moves with
  | End -> Some target
  | Move (src, dst, rest) -> (match read source src with None -> None | Some value ->
    match write target dst value with None -> None | Some next -> parallel source next rest)
let rec (apply @ total) : (source : F.cells) @ immutable -> (target : F.cells) @ immutable -> (moves : moves) @ immutable ->
    (snapshot : snapshot) @ immutable ->
    {u : unit | valid (F.length source) (F.length target) moves && captured source moves snapshot} ->
    {out : F.cells | parallel source target moves === Some out && F.length out === F.length target} @ immutable =
  fun source target moves snapshot premise ->
    ghost_ (valid_def (F.length source) (F.length target) moves; captured_def source moves snapshot; parallel_def source target moves);
    match moves, snapshot with
    | End, Empty -> target
    | Move (_, _, rest), Saved (dst, value, tail) ->
      let next = put target dst value () in apply source next rest tail ()
    | _ -> unreachable_ ()
let (copy @ total) : (cells : F.cells) @ immutable -> (moves : moves) @ immutable ->
    {u : unit | valid (F.length cells) (F.length cells) moves} ->
    {out : F.cells | parallel cells cells moves === Some out && F.length out === F.length cells} @ immutable = fun cells moves premise ->
  let snapshot = capture cells (F.length cells) moves () in apply cells cells moves snapshot ()
