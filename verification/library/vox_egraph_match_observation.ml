module I = Vox_iarray
module M = Vox_egraph_union_spec
module Q = Vox_egraph_match_spec

let[@def] rec (labels @ total) (parents : int iarray @ immutable)
    (count : int) = ghost_ (
  if count <= 0 then parents
  else I.updated (labels parents (count - 1)) (count - 1)
    (M.root parents (count - 1)))
  [@@decreases if count > 0 then count else 0]

let rec (labels_length @ total) :
    (parents : int iarray) @ immutable -> (count : int) ->
    {u : unit | Iarray.length (labels parents count) = Iarray.length parents}
    @ ghost = fun parents count -> ghost_ (
  labels_def parents count;
  if count > 0 then (
    labels_length parents (count - 1);
    I.updated_length (labels parents (count - 1)) (count - 1)
      (M.root parents (count - 1)));
  ())
  [@@decreases if count > 0 then count else 0]

let rec (labels_at @ total) :
    (parents : int iarray) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | 0 <= id && id < count && count <= Iarray.length parents} ->
    {u : unit | I.at (labels parents count) id === Some (M.root parents id)}
    @ ghost = fun parents count id premise -> ghost_ (
  labels_def parents count;
  labels_length parents (count - 1);
  I.updated_read (labels parents (count - 1)) (count - 1)
    (M.root parents (count - 1)) id;
  if id < count - 1 then labels_at parents (count - 1) id ();
  ())
  [@@decreases if count > 0 then count else 0]

let[@def] (observe @ total) (nodes : Q.node option iarray @ immutable)
    (parents : int iarray @ immutable) (count : int) = ghost_ (
  {Q.count; nodes; classes = labels parents count})

let (class_at @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | 0 <= id && id < count && count <= Iarray.length parents} ->
    {u : unit | Q.class_id (observe nodes parents count) id ===
      Some (M.root parents id)} @ ghost =
  fun nodes parents count id premise -> ghost_ (
    observe_def nodes parents count;
    labels_length parents count;
    labels_at parents count id ();
    Q.class_id_def (observe nodes parents count) id;
    let values = labels parents count in
    let _ : {u : unit | 0 <= id && id < Iarray.length values} = () in
    I.at_get values (id);
    ())

let (same @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable -> (count : int) ->
    (left : int) -> (right : int) ->
    {u : unit | 0 <= left && left < count && 0 <= right && right < count &&
      count <= Iarray.length parents} ->
    {u : unit | Q.same (observe nodes parents count) left right =
      (M.root parents left = M.root parents right)} @ ghost =
  fun nodes parents count left right premise -> ghost_ (
    class_at nodes parents count left ();
    class_at nodes parents count right ();
    Q.same_def (observe nodes parents count) left right;
    ())

let (node_at @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | 0 <= id && id < count && count <= Iarray.length nodes} ->
    {u : unit | I.at nodes id === Some (Q.node (observe nodes parents count) id)}
    @ ghost = fun nodes parents count id premise -> ghost_ (
  observe_def nodes parents count;
  Q.node_def (observe nodes parents count) id;
  I.at_get nodes (id);
  ())
