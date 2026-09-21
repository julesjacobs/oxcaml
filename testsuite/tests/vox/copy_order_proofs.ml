open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_copy_proofs
open Pooled_allocation_proofs

let rec (target_below_at @ total) : (saved : node Pref.heap) @ immutable ->
    (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable -> (final : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && valid saved epoch depth final && extends d final && target_for saved d p q} ->
    {u : unit | below (heap saved epoch depth final) q depth} @ ghost = fun saved depth bounds epoch d final p q premise -> ghost_ (
  let refine_ premise = premise in target_for_def saved d p q; valid_def saved epoch depth d; mapping_def d p;
  let after = heap saved epoch depth final in let u = () in
  copy_depth saved epoch depth d (refine_ u); bounds p; finite_node_def saved p; below_def saved p depth; history_at saved epoch depth final p (refine_ u);
  at_level_def saved p; below_def after q depth; at_level_def after q;
  match H.at saved p with None -> refine_ u | Some v ->
  match v.level with Finite _ -> refine_ u | Generic ->
    match d with Start -> refine_ u
    | Fresh (rest, x, _, old, desc) ->
      if p === x then (
        history_at saved epoch depth rest p (refine_ u); ready_def saved rest old.desc desc;
        copied_fresh saved epoch depth d final p q (refine_ u); refine_ u)
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        target_for_def saved rest p q;
        target_below_at saved depth bounds epoch rest final p q (refine_ u); refine_ u)
    | Alias (rest, x, _, old) ->
      extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
      if p === x then (match old.desc with Link child ->
        target_below_at saved depth bounds epoch rest final child q (refine_ u); refine_ u | _ -> refine_ u)
      else (target_for_def saved rest p q;
        target_below_at saved depth bounds epoch rest final p q (refine_ u); refine_ u))

let (mark_below @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | H.mem h p && H.at h p === Some old} ->
    {u : unit | below (H.put h p (mark old epoch q)) x bound === below h x bound} @ ghost =
  fun h p old epoch q x bound premise -> ghost_ (
    let refine_ premise = premise in let v = mark old epoch q in mark_def old epoch q;
    let after = H.put h p v in put_frame h p v x;
    below_def h x bound; below_def after x bound; at_level_def h x; at_level_def after x;
    let u = () in refine_ u)

let (mark_ordered @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && ordered h x} ->
    {u : unit | ordered (H.put h p (mark old epoch q)) x} @ ghost =
  fun h p old epoch q x premise -> ghost_ (
    let refine_ premise = premise in let v = mark old epoch q in mark_def old epoch q;
    let after = H.put h p v in put_frame h p v x;
    ordered_def h x; ordered_def after x; let u = () in
    match H.at h x with None -> refine_ u | Some v ->
    match v.level with Generic -> refine_ u | Finite n ->
    children_below_def h v.desc n; children_below_def after v.desc n;
    match v.desc with Var | Bool -> refine_ u
    | Link y -> mark_below h p old epoch q y n (refine_ u); refine_ u
    | Arrow (a, b) -> mark_below h p old epoch q a n (refine_ u);
      mark_below h p old epoch q b n (refine_ u); refine_ u)

let rec (copy_ordered @ total) : (saved : node Pref.heap) @ immutable ->
    (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered saved x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | ordered (heap saved epoch depth d) x} @ ghost =
  fun saved depth bounds order epoch d x premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
    let u = () in
    match d with
    | Start ->
      let desc : desc = Bool in children_below_def saved desc depth; order x;
      allocation_ordered saved epoch desc depth x (refine_ u); refine_ u
    | Fresh (rest, p, q, old, desc) ->
      let mid = heap saved epoch depth rest in
      copy_ordered saved depth bounds order epoch rest x (refine_ u);
      copy_depth saved epoch depth rest (refine_ u);
      ready_def saved rest old.desc desc; children_below_def mid desc depth;
      extends_def rest rest;
      (match old.desc, desc with Arrow (a, b), Arrow (c, e) ->
        target_below_at saved depth bounds epoch rest rest a c (refine_ u);
        target_below_at saved depth bounds epoch rest rest b e (refine_ u); () | _ -> ());
      allocation_ordered mid q desc depth x (refine_ u);
      let v = cell desc depth in let h1 = H.put mid q v in
      history_grows saved epoch depth rest p (refine_ u); put_frame mid q v p;
      mark_ordered h1 p old epoch q x (refine_ u); refine_ u
    | Alias (rest, p, q, old) ->
      let mid = heap saved epoch depth rest in
      copy_ordered saved depth bounds order epoch rest x (refine_ u);
      history_grows saved epoch depth rest p (refine_ u);
      mark_ordered mid p old epoch q x (refine_ u); refine_ u)

let rec (copy_bounds @ total) : (saved : node Pref.heap) @ immutable ->
    (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem (heap saved epoch depth d) x)
      || not (finite_node (heap saved epoch depth d) x)
      || below (heap saved epoch depth d) x depth} @ ghost =
  fun saved depth bounds epoch d x premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
    let after = heap saved epoch depth d in
    finite_node_def after x; below_def after x depth; at_level_def after x;
    let u = () in match d with
    | Start -> bounds x; finite_node_def saved x; below_def saved x depth; at_level_def saved x;
      let desc : desc = Bool in let v = cell desc depth in cell_def desc depth;
      put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      let mid = heap saved epoch depth rest in copy_bounds saved depth bounds epoch rest x (refine_ u);
      copy_depth saved epoch depth rest (refine_ u);
      finite_node_def mid x; below_def mid x depth; at_level_def mid x;
      let v = cell desc depth in cell_def desc depth; let h1 = H.put mid q v in
      let w = mark old epoch q in mark_def old epoch q;
      put_frame mid q v x; put_frame h1 p w x; refine_ u
    | Alias (rest, p, q, old) ->
      let mid = heap saved epoch depth rest in copy_bounds saved depth bounds epoch rest x (refine_ u);
      finite_node_def mid x; below_def mid x depth; at_level_def mid x;
      let w = mark old epoch q in mark_def old epoch q; put_frame mid p w x; refine_ u)
