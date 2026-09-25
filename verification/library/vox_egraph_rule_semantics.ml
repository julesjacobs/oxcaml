module I = Vox_iarray
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules
module U = Vox_egraph_union_spec

let[@def] (origin @ total) (origins : L.expr iarray @ immutable)
    (id : int) =
  match I.at origins id with Some expr -> expr | None -> L.Int_input

let[@def] (edge_ok @ total) (rules : R.t @ immutable) (parents : int iarray @ immutable)
    (origins : L.expr iarray @ immutable)
    (edges : E.evidence option iarray @ immutable) (id : int) = ghost_ (
  let next = U.parent parents id in
  if next = id then I.at edges id === Some None
  else
    (match I.at edges id with
     | Some (Some proof) ->
       0 <= next && next < id && E.valid rules proof &&
       E.left proof === origin origins id &&
       E.right proof === origin origins next
     | _ -> false))

let[@def] rec (valid_edges @ total) (rules : R.t @ immutable) (parents : int iarray @ immutable)
    (origins : L.expr iarray @ immutable)
    (edges : E.evidence option iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else edge_ok rules parents origins edges (count - 1) &&
    valid_edges rules parents origins edges (count - 1))
  [@@decreases if count > 0 then count else 0]

let rec (edge_at @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | valid_edges rules parents origins edges count &&
      0 <= id && id < count} ->
    {u : unit | edge_ok rules parents origins edges id} @ ghost =
  fun rules parents origins edges count id premise -> ghost_ (
    valid_edges_def rules parents origins edges count;
    if id < count - 1 then
      edge_at rules parents origins edges (count - 1) id ();
    ())
  [@@decreases if count > 0 then count else 0]

let rec (explain @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | U.ordered parents count &&
      valid_edges rules parents origins edges count &&
      0 <= id && id < count &&
      not (L.sort (origin origins id) === None)} ->
    {proof : E.evidence | E.valid rules proof &&
      E.left proof === origin origins id &&
      E.right proof === origin origins (U.root parents id)}
      @ ghost =
  fun rules parents origins edges count id premise -> ghost_ (
    U.ordered_index parents count id ();
    edge_at rules parents origins edges count id ();
    edge_ok_def rules parents origins edges id;
    U.root_def parents id;
    let next = U.parent parents id in
    if next = id then (
      let proof = E.Refl (origin origins id) in
      E.valid_def rules proof;
      E.left_def proof;
      E.right_def proof;
      E.endpoints_def proof;
      proof)
    else
      match I.at edges id with
      | Some (Some edge) ->
        EP.sort_sound rules edge ();
        EP.well_sorted_def edge;
        E.left_def edge;
        E.right_def edge;
        E.endpoints_def edge;
        let tail = explain rules parents origins edges count next () in
        E.left_def tail;
        E.right_def tail;
        let proof = E.Trans (edge, tail) in
        E.valid_def rules proof;
        E.left_def proof;
        E.right_def proof;
        E.endpoints_def proof;
        let _ : {u : unit | E.valid rules proof} = () in
        let _ : {u : unit | E.left proof === origin origins id} =
          () in
        let _ : {u : unit | E.right proof ===
          origin origins (U.root parents id)} = () in
        proof
      | _ -> (E.Refl (origin origins id)))
  [@@decreases if id > 0 then id else 0]

let (same_class_evidence @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (a : int) -> (b : int) ->
    {u : unit | U.ordered parents count &&
      valid_edges rules parents origins edges count &&
      0 <= a && a < count && 0 <= b && b < count &&
      U.root parents a = U.root parents b &&
      not (L.sort (origin origins a) === None) &&
      not (L.sort (origin origins b) === None)} ->
    {proof : E.evidence | E.valid rules proof &&
      E.left proof === origin origins a &&
      E.right proof === origin origins b} @ ghost =
  fun rules parents origins edges count a b premise -> ghost_ (
    let first = explain rules parents origins edges count a () in
    let second = explain rules parents origins edges count b () in
    let reversed = EP.symmetric rules second in
    EP.transitive rules first reversed ())

let rec (frame_below @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (loser : int) -> (winner : int) ->
    (proof : E.evidence) @ immutable ->
    {u : unit | 0 <= count && count <= loser &&
      loser < Iarray.length parents && loser < Iarray.length edges &&
      valid_edges rules parents origins edges count} ->
    {u : unit | valid_edges rules
      (I.updated parents loser winner) origins
      (I.updated edges loser (Some proof)) count} @ ghost =
  fun rules parents origins edges count loser winner proof premise -> ghost_ (
    let changed_parents = I.updated parents loser winner in
    let changed_edges = I.updated edges loser (Some proof) in
    valid_edges_def rules parents origins edges count;
    valid_edges_def rules changed_parents origins changed_edges count;
    if count > 0 then (
      let id = count - 1 in
      edge_ok_def rules parents origins edges id;
      edge_ok_def rules changed_parents origins changed_edges id;
      I.updated_read parents loser winner id;
      I.updated_read edges loser (Some proof) id;
      U.parent_def parents id;
      U.parent_def changed_parents id;
      frame_below rules parents origins edges (count - 1) loser winner proof ());
    ())
  [@@decreases if count > 0 then count else 0]

let rec (append_frame @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (index : int) -> (expr : L.expr) @ immutable ->
    {u : unit | 0 <= count && count <= index &&
      index < Iarray.length parents && index < Iarray.length origins &&
      index < Iarray.length edges && U.ordered parents count &&
      valid_edges rules parents origins edges count} ->
    {u : unit | valid_edges rules
      (I.updated parents index index)
      (I.updated origins index expr)
      (I.updated edges index None) count} @ ghost =
  fun rules parents origins edges count index expr premise -> ghost_ (
    let p = I.updated parents index index in
    let o = I.updated origins index expr in
    let e = I.updated edges index None in
    valid_edges_def rules parents origins edges count;
    valid_edges_def rules p o e count;
    U.ordered_def parents count;
    if count > 0 then (
      let id = count - 1 in
      U.ordered_index parents count id ();
      let next = U.parent parents id in
      I.updated_read parents index index id;
      I.updated_read origins index expr id;
      I.updated_read origins index expr next;
      I.updated_read edges index None id;
      U.parent_def parents id;
      U.parent_def p id;
      edge_ok_def rules parents origins edges id;
      edge_ok_def rules p o e id;
      origin_def origins id;
      origin_def o id;
      origin_def origins next;
      origin_def o next;
      append_frame rules parents origins edges (count - 1) index expr ());
    ())
  [@@decreases if count > 0 then count else 0]

let (append_preserves @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (expr : L.expr) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length parents &&
      count < Iarray.length origins && count < Iarray.length edges &&
      U.ordered parents count && valid_edges rules parents origins edges count} ->
    {u : unit | valid_edges rules
      (I.updated parents count count)
      (I.updated origins count expr)
      (I.updated edges count None) (count + 1)} @ ghost =
  fun rules parents origins edges count expr premise -> ghost_ (
    let p = I.updated parents count count in
    let o = I.updated origins count expr in
    let e = I.updated edges count None in
    append_frame rules parents origins edges count count expr ();
    I.updated_read parents count count count;
    I.updated_read edges count None count;
    U.parent_def p count;
    edge_ok_def rules p o e count;
    valid_edges_def rules p o e (count + 1);
    ())

let rec (link_preserves @ total) :
    (rules : R.t) @ immutable ->
    (parents : int iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (edges : E.evidence option iarray) @ immutable ->
    (count : int) -> (loser : int) -> (winner : int) ->
    (proof : E.evidence) @ immutable ->
    {u : unit | 0 <= winner && winner < loser && loser < count &&
      count <= Iarray.length parents && count <= Iarray.length edges &&
      U.parent parents loser = loser &&
      valid_edges rules parents origins edges count && E.valid rules proof &&
      E.left proof === origin origins loser &&
      E.right proof === origin origins winner} ->
    {u : unit | valid_edges rules
      (I.updated parents loser winner) origins
      (I.updated edges loser (Some proof)) count} @ ghost =
  fun rules parents origins edges count loser winner proof premise -> ghost_ (
    let changed_parents = I.updated parents loser winner in
    let changed_edges = I.updated edges loser (Some proof) in
    valid_edges_def rules parents origins edges count;
    valid_edges_def rules changed_parents origins changed_edges count;
    if count - 1 = loser then (
      I.updated_read parents loser winner loser;
      I.updated_read edges loser (Some proof) loser;
      U.parent_def changed_parents loser;
      edge_ok_def rules changed_parents origins changed_edges loser;
      frame_below rules parents origins edges loser loser winner proof ())
    else (
      let id = count - 1 in
      edge_ok_def rules parents origins edges id;
      edge_ok_def rules changed_parents origins changed_edges id;
      I.updated_read parents loser winner id;
      I.updated_read edges loser (Some proof) id;
      U.parent_def parents id;
      U.parent_def changed_parents id;
      link_preserves rules parents origins edges (count - 1)
        loser winner proof ());
    ())
  [@@decreases if count > 0 then count else 0]
