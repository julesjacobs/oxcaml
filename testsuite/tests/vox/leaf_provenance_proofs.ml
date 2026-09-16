open Leaf_provenance_spec
open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Provenance_proofs
open Copy_heap_proofs
open Level_proofs
open Lower_locality_spec
open Lower_locality_proofs
open Marked_occurs_proofs
let (lower_bind_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (tree : bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && Level_unifier_spec.observe h p === Some Var &&
      at_level h p === Finite bound && lower_valid h bound edits &&
      confined edits tree && bound_root tree === q &&
      bounded (lower_heap h bound edits) bound tree} ->
    {o : origin | let mid = lower_heap h bound edits in
      let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
      not (low_var after x cut) || originates saved after cut x o}
      @ immutable ghost = fun saved h cut prior p q bound edits tree x premise -> ghost_ (
    let refine_ premise = premise in let mid = lower_heap h bound edits in
    let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
    lower_valid_def h bound edits;
    let u = () in Level_unifier_proofs.lower_observe h bound edits p (refine_ u);
    lowering_at h bound edits p (refine_ u); lower_frame_def h mid p;
    link_below mid p q cut x (refine_ u);
    low_var_def after x cut; low_var_def h x cut; low_var_def h p cut;
    Level_unifier_proofs.lower_observe h bound edits x (refine_ u);
    let v = Level_unifier_spec.redirect mid p q in
    Level_unifier_proofs.observe_write mid p v x;
    Level_unifier_proofs.redirect_desc mid p q;
    if not (low_var after x cut) then (let o = Origin (x, Stop) in refine_ o)
    else if below h x cut then (
      let refine_ o = prior x in lower_origin saved h cut bound edits x o (refine_ u);
      link_origin saved mid cut p q x o (refine_ u); refine_ o)
    else (
      if bound > cut then (lower_floor h bound edits cut x (refine_ u); ());
      below_def h p cut;
      let refine_ source = prior p in
      lower_origin saved h cut bound edits p source (refine_ u);
      link_origin saved mid cut p q p source (refine_ u);
      originates_def saved after cut p source;
      if not (contains tree x) then (
        confined_frame h bound edits tree x (refine_ u);
        lowering_at h bound edits x (refine_ u); lower_frame_def h mid x;
        below_def h x cut; below_def mid x cut;
        at_level_def h x; at_level_def mid x; ());
      let refine_ tail = bounded_path mid bound tree x (refine_ u) in
      link_path mid p q q x tail (refine_ u);
      let next = Step (q, tail) in reaches_def after p x next;
      edge_def after p q; Level_unifier_spec.redirect_def mid p q;
      Level_unifier_spec.observe_def mid p;
      let v = Level_unifier_spec.redirect mid p q in put_frame mid p v p;
      match source with Origin (root, path) ->
        append_reaches after root p x path next (refine_ u);
        let out = Origin (root, append path next) in
        originates_def saved after cut x out; refine_ out))


let rec (unified_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Level_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Level_unifier_spec.unified h p q ok after d} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o}
      @ immutable ghost = fun saved h cut prior p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in
    Level_unifier_spec.unified_def h p q ok after d;
    let u = () in
    if not (low_var after x cut) then (let o = Origin (x, Stop) in refine_ o)
    else match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
      let refine_ o = prior x in refine_ o
    | Bind_left _ -> link_below h p q cut x (refine_ u);
      low_var_def h x cut; low_var_def after x cut;
      let v = Level_unifier_spec.redirect h p q in
      Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q;
      let refine_ o = prior x in
      link_origin saved h cut p q x o (refine_ u); refine_ o
    | Bind_right _ -> link_below h q p cut x (refine_ u);
      low_var_def h x cut; low_var_def after x cut;
      let v = Level_unifier_spec.redirect h q p in
      Level_unifier_proofs.observe_write h q v x; Level_unifier_proofs.redirect_desc h q p;
      let refine_ o = prior x in
      link_origin saved h cut q p x o (refine_ u); refine_ o
    | Swap rest ->
      let refine_ o = unified_leaf_origin saved h cut prior q p ok after rest x (refine_ u) in refine_ o
    | Resolve (r, s, _, _, rest) ->
      let refine_ o = unified_leaf_origin saved h cut prior r s ok after rest x (refine_ u) in refine_ o
    | Scanned (needle, marks, rest) ->
      let mid = Level_unifier_spec.scan_heap h marks in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid y cut) || originates saved mid cut y o} @ immutable) @ total = fun y ->
        let u = () in scan_below h needle marks y cut (refine_ u);
        scan_observe h needle marks y (refine_ u); low_var_def h y cut; low_var_def mid y cut;
        let refine_ o = prior y in originates_def saved h cut y o;
        originates_def saved mid cut y o;
        if low_var mid y cut then (match o with Origin (root, path) ->
          scan_path h needle marks root y path (refine_ u); refine_ o) else refine_ o in
      let refine_ o = unified_leaf_origin saved mid cut middle_prior p q ok after rest x (refine_ u) in refine_ o
    | Lowering (bound, edits, tree, rest) ->
      let mid = lower_heap h bound edits in
      Level_unifier_spec.unified_def mid p q ok after rest;
      (match rest with Bind_left _ ->
        let refine_ o = lower_bind_leaf_origin saved h cut prior p q bound edits tree x (refine_ u) in refine_ o
      | _ -> let o = Origin (x, Stop) in refine_ o)
    | Children (a, b, c, e, mid, left_ok, left, right) ->
      if left_ok then (
        let prior1 : ((x : node Pref.t) @ immutable ->
          {o : origin | not (low_var mid x cut) || originates saved mid cut x o}
          @ immutable) @ total = fun x ->
          let u = () in
          let refine_ o = unified_leaf_origin saved h cut prior a c left_ok mid left x (refine_ u) in refine_ o in
        let refine_ o = unified_leaf_origin saved mid cut prior1 b e ok after right x (refine_ u) in refine_ o)
      else (
        let refine_ o = unified_leaf_origin saved h cut prior a c left_ok mid left x (refine_ u) in refine_ o))


let (allocation_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.at h p === None &&
      (match v.level with Generic -> true | Finite n -> n > cut)} ->
    {o : origin | not (low_var (H.put h p v) x cut) ||
      originates saved (H.put h p v) cut x o} @ immutable ghost =
  fun saved h cut prior p v x premise -> ghost_ (
    let refine_ premise = premise in let after = H.put h p v in
    put_frame h p v x; below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    low_var_def h x cut; low_var_def after x cut;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    let refine_ o = prior x in
    if low_var after x cut then (
      originates_def saved h cut x o; originates_def saved after cut x o;
      let u = () in match o with Origin (root, path) ->
        allocation_path h p v root x path (refine_ u); refine_ o)
    else refine_ o)


let (mark_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old &&
      (not (low_var h x cut) || originates saved h cut x origin)} ->
    {u : unit | not (low_var (H.put h p (mark old epoch target)) x cut) ||
      originates saved (H.put h p (mark old epoch target)) cut x origin}
      @ ghost = fun saved h cut p old epoch target x origin premise -> ghost_ (
    let refine_ premise = premise in let v = mark old epoch target in
    let after = H.put h p v in mark_def old epoch target; put_frame h p v x;
    below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    low_var_def h x cut; low_var_def after x cut;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    let u = () in if low_var after x cut then (
      originates_def saved h cut x origin;
      originates_def saved after cut x origin;
      match origin with Origin (root, path) ->
        mark_path h p old epoch target root x path (refine_ u); refine_ u)
    else refine_ u)


let rec (copy_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && depth > cut} ->
    {o : origin | not (low_var (heap h epoch depth d) x cut) ||
      originates saved (heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h cut scope prior epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in valid_def h epoch depth d;
    heap_def h epoch depth d; let u = () in match d with
    | Start -> scope epoch; let desc : desc = Bool in
      let v = cell desc depth in cell_def desc depth;
      let refine_ o = allocation_leaf_origin saved h cut prior epoch v x (refine_ u) in refine_ o
    | Fresh (rest, p, q, old, desc) -> let mid = heap h epoch depth rest in
      let prior1 : ((x : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid x cut) || originates saved mid cut x o}
        @ immutable) @ total = fun x -> let u = () in
        let refine_ o = copy_leaf_origin saved h cut scope prior epoch depth rest x (refine_ u) in refine_ o in
      Copy_model_proofs.history_scope h scope epoch depth rest q (refine_ u);
      let v = cell desc depth in cell_def desc depth;
      let refine_ o = allocation_leaf_origin saved mid cut prior1 q v x (refine_ u) in
      let h1 = H.put mid q v in
      history_grows h epoch depth rest p (refine_ u); put_frame mid q v p;
      mark_leaf_origin saved h1 cut p old epoch q x o (refine_ u); refine_ o
    | Alias (rest, p, q, old) -> let mid = heap h epoch depth rest in
      let refine_ o = copy_leaf_origin saved h cut scope prior epoch depth rest x (refine_ u) in
      history_grows h epoch depth rest p (refine_ u);
      mark_leaf_origin saved mid cut p old epoch q x o (refine_ u); refine_ o)


let (closed_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (origin_cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | pool_scoped h pool &&
      (not (low_var h x origin_cut) || originates saved h origin_cut x origin)} ->
    {u : unit | not (low_var (closed_heap h cut pool) x origin_cut) ||
      originates saved (closed_heap h cut pool) origin_cut x origin} @ ghost =
  fun saved h cut origin_cut pool x origin premise -> ghost_ (
    let refine_ premise = premise in let after = closed_heap h cut pool in
    let u = () in Generalize_proofs.closed_observe h cut pool x (refine_ u);
    closed_at_def h after cut pool x;
    below_def h x origin_cut; below_def after x origin_cut;
    low_var_def h x origin_cut; low_var_def after x origin_cut;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    at_level_def h x; at_level_def after x;
    low_var_def h x cut; low_var_def after x cut;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ());
    if low_var after x origin_cut then (
      originates_def saved h origin_cut x origin;
      originates_def saved after origin_cut x origin;
      match origin with Origin (root, path) ->
        closed_path h cut pool root x path (refine_ u); refine_ u)
    else refine_ u)
