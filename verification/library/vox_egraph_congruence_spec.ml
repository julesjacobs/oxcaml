module Q = Vox_egraph_match_spec

let[@def] (same_node @ total) (graph : Q.graph @ immutable)
    (left : Q.node @ immutable) (right : Q.node @ immutable) =
  match left, right with
  | Q.Int_lit a, Q.Int_lit b -> a = b
  | Q.Bool_lit a, Q.Bool_lit b -> a = b
  | Q.Int_input, Q.Int_input | Q.Bool_input, Q.Bool_input -> true
  | Q.Add (a, b), Q.Add (x, y) | Q.Eq_int (a, b), Q.Eq_int (x, y) ->
    Q.same graph a x && Q.same graph b y
  | Q.Int_if (c, a, b), Q.Int_if (z, x, y)
  | Q.Bool_if (c, a, b), Q.Bool_if (z, x, y) ->
    Q.same graph c z && Q.same graph a x && Q.same graph b y
  | _ -> false

let[@def] (pair @ total) (graph : Q.graph @ immutable) (left : int) (right : int) =
  match Q.node graph left, Q.node graph right with
  | Some a, Some b -> not (same_node graph a b) || Q.same graph left right
  | _ -> true

let[@def] rec (row @ total) (graph : Q.graph @ immutable) (left : int) (count : int) =
  if count <= 0 then true
  else pair graph left (count - 1) && row graph left (count - 1)
  [@@decreases if count > 0 then count else 0]

let[@def] rec (rows @ total) (graph : Q.graph @ immutable) (count : int) =
  if count <= 0 then true
  else row graph (count - 1) graph.count && rows graph (count - 1)
  [@@decreases if count > 0 then count else 0]

let[@def] (closed @ total) (graph : Q.graph @ immutable) = rows graph graph.count
