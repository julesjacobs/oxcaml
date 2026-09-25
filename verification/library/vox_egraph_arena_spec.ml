module I = Vox_iarray
module K = Vox_egraph_key

let[@def] rec (allocated @ total)
    (values : K.t option iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else
    (match I.at values (count - 1) with
     | Some (Some _) -> true
     | _ -> false) &&
    allocated values (count - 1))
  [@@decreases if count > 0 then count else 0]

let rec (append_frame @ total) :
    (values : K.t option iarray) @ immutable ->
    (count : int) -> (index : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count <= index &&
      index < Iarray.length values && allocated values count} ->
    {u : unit | allocated (I.updated values index (Some key)) count}
    @ ghost = fun values count index key premise -> ghost_ (
  allocated_def values count;
  allocated_def (I.updated values index (Some key)) count;
  if count > 0 then (
    I.updated_read values index (Some key) (count - 1);
    append_frame values (count - 1) index key ());
  ())
  [@@decreases if count > 0 then count else 0]

let (append_preserves @ total) :
    (values : K.t option iarray) @ immutable ->
    (count : int) -> (key : K.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length values &&
      allocated values count} ->
    {u : unit | allocated (I.updated values count (Some key)) (count + 1)}
    @ ghost = fun values count key premise -> ghost_ (
  I.updated_length values count (Some key);
  I.updated_read values count (Some key) count;
  append_frame values count count key ();
  allocated_def (I.updated values count (Some key)) (count + 1);
  ())
