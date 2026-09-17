open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
module E = Effective_level
module R = Representative_level
module M = Effective_unifier_metadata

let[@def] (safe @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (x : node Pref.t @ immutable) = ghost_ (
  E.valid_head h heads x
  && (if H.mem h x then source_ok h x else H.at h x === None)
  && E.effective_ordered h heads x
  && match H.at h x with
     | None -> true | Some v -> not v.visited && v.memo === Empty_memo)

let[@def] (depth_bound @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (depth : int)
    (x : node Pref.t @ immutable) = ghost_ (
  not (H.mem h x) || E.level h heads x === Generic
  || E.effective_below h heads x depth)

let[@def] (runtime_at @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (depth : int) (pool : pool @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  safe h heads x && depth_bound h heads depth x
  && R.representative_covered h (depth - 1) pool x)

let (unify_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h a depth pool x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (valid : ((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head after b x})) @ total ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x
        && (if H.mem h x then Level_finite_spec.finite h t
            else observe h x === None)} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d} ->
    {u : unit | runtime_at after b depth pool x} @ ghost =
  fun h a b depth pool facts p q ok after d valid trees x premise -> ghost_ (
    let refine_ premise = premise in
    let before_valid : ((y : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h a y}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      let u = () in refine_ u in
    let order : ((y : node Pref.t) @ immutable ->
      {u : unit | E.effective_ordered h a y}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y;
      let u = () in refine_ u in
    facts x; valid x; runtime_at_def h a depth pool x;
    safe_def h a x; depth_bound_def h a depth x;
    let u = () in
    M.source h p q ok after d trees x (refine_ u);
    M.cells h p q ok after d x (refine_ u);
    M.cell_frame_def h after x;
    Effective_unifier_order.ordered h p q ok after d a b
      before_valid valid order x (refine_ u);
    M.levels h p q ok after d a b x (refine_ u);
    let old = E.level h a x in let next = E.level after b x in
    decreases_def old next;
    if H.mem h x && not (old === Generic) then
      (M.below h p q ok after d a b x depth (refine_ u); ()) else ();
    let cut = depth - 1 in
    Effective_unifier_pool.coverage h p q ok after d cut pool x (refine_ u);
    runtime_at_def after b depth pool x; safe_def after b x;
    depth_bound_def after b depth x; refine_ u)

let (enter_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (depth : int) ->
    (pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && depth + 1 >= 0
      && runtime_at h heads depth pool x} ->
    {u : unit | runtime_at h heads (depth + 1) Empty x} @ ghost =
  fun h heads depth pool x premise -> ghost_ (
    let refine_ premise = premise in
    runtime_at_def h heads depth pool x; safe_def h heads x;
    depth_bound_def h heads depth x;
    let child_depth = depth + 1 in let empty : pool = Empty in
    runtime_at_def h heads child_depth empty x;
    depth_bound_def h heads child_depth x;
    E.effective_below_def h heads x depth;
    E.effective_below_def h heads x child_depth;
    let cut = child_depth - 1 in
    R.representative_covered_def h cut empty x;
    covered_def h cut empty x; finite_node_def h x;
    let u = () in
    if H.mem h x && terminal h x then (
      E.terminal_level h heads x (refine_ u); at_level_def h x; ())
    else ();
    refine_ u)
