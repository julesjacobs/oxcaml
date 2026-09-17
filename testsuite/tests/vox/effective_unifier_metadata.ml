open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module M = Level_unifier_metadata
module E = Effective_level
module R = Representative_level
module P = Effective_unifier_heads

let[@def] (cell_frame @ total) (h : Pref.heap @ immutable) (after : Pref.heap @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true
  | Some old, Some next -> old.memo === next.memo && old.visited === next.visited
    && decreases old.level next.level
  | _ -> false)

let (frame_trans @ total) : (h : Pref.heap) @ immutable -> (middle : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | cell_frame h middle x && cell_frame middle after x} ->
    {u : unit | cell_frame h after x} @ ghost = fun h middle after x premise -> ghost_ (
      let refine_ premise = premise in cell_frame_def h middle x; cell_frame_def middle after x; cell_frame_def h after x;
      (match H.at h x, H.at middle x, H.at after x with
      | Some a, Some b, Some c -> decreases_def a.level b.level; decreases_def b.level c.level; decreases_def a.level c.level; ()
      | _ -> ()); let u = () in refine_ u)

let rec (cells @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} -> {u : unit | cell_frame h after x} @ ghost =
  fun h p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d;
    let u = () in Effective_unifier_frame.unified_frame h p q ok after d x (refine_ u); match d with
    | Base old -> M.unified_scratch h p q ok after old x (refine_ u);
      M.scratch_frame_def h after x; cell_frame_def h after x; refine_ u
    | Swap rest -> cells h q p ok after rest x (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) -> cells h r s ok after rest x (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      cells h a c left_ok middle left x (refine_ u);
      if left_ok then (cells middle b e ok after right x (refine_ u); frame_trans h middle after x (refine_ u); ()) else ();
      refine_ u
    | Post_link (middle, rest, source, target) ->
      cells h p q true middle rest x (refine_ u);
      Structure_spec.linkable_def middle source target;
      let s = tree_root source in let t = tree_root target in M.redirect_scratch middle s t x (refine_ u);
      M.scratch_frame_def middle after x; cell_frame_def h middle x; cell_frame_def middle after x;
      frame_trans h middle after x (refine_ u); refine_ u
    | Pre_compress (middle, edits, rest) ->
      Effective_compression_proofs.frame h middle edits x (refine_ u);
      cell_frame_def h middle x; at_level_def h x; at_level_def middle x;
      (match H.at h x, H.at middle x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
      cells middle p q ok after rest x (refine_ u); frame_trans h middle after x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->
      let middle = scan_heap h marks in Marked_occurs_proofs.scan_frame h needle marks x (refine_ u);
      lower_frame_def h middle x; cell_frame_def h middle x;
      cells middle p q ok after rest x (refine_ u); frame_trans h middle after x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) ->
      let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      Terminal_lower_proofs.lowering_at h bound edits x (refine_ u);
      lower_frame_def h middle x; cell_frame_def h middle x;
      cells middle p q ok after rest x (refine_ u); frame_trans h middle after x (refine_ u); refine_ u)

let (head @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (heads : E.heads) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && E.valid_head h heads x} ->
    {r : R.representative | not (H.mem after x) || resolves after x r.root r.path} @ immutable ghost =
  fun h p q ok after d heads x premise -> ghost_ (
    let refine_ premise = premise in let u = () in cells h p q ok after d x (refine_ u);
    cell_frame_def h after x; E.valid_head_def h heads x; let old = heads x in
    if H.mem h x then (let refine_ out = P.unified_head h p q ok after d x old.root old.path (refine_ u) in refine_ out)
    else refine_ old)

let (levels @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && E.valid_head h before_heads x && E.valid_head after after_heads x} ->
    {u : unit | decreases (E.level h before_heads x) (E.level after after_heads x)} @ ghost =
  fun h p q ok after d before_heads after_heads x premise -> ghost_ (
    let refine_ premise = premise in let u = () in cells h p q ok after d x (refine_ u);
    cell_frame_def h after x; E.valid_head_def h before_heads x; E.valid_head_def after after_heads x;
    E.level_def h before_heads x; E.level_def after after_heads x;
    if H.mem h x then (
      let old = before_heads x in let current = after_heads x in
      let refine_ transported = P.unified_head h p q ok after d x old.root old.path (refine_ u) in
      R.unique after x current.root current.path transported.root transported.path (refine_ u);
      P.progress_def h old.root after transported.root; ())
    else (let generic = Generic in decreases_def generic generic; ()); refine_ u)

let (below @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | unified h p q ok after d && E.valid_head h before_heads x && E.valid_head after after_heads x
      && E.effective_below h before_heads x bound} ->
    {u : unit | E.effective_below after after_heads x bound} @ ghost =
  fun h p q ok after d before_heads after_heads x bound premise -> ghost_ (
    let refine_ premise = premise in let u = () in levels h p q ok after d before_heads after_heads x (refine_ u);
    cells h p q ok after d x (refine_ u); cell_frame_def h after x;
    E.effective_below_def h before_heads x bound; E.effective_below_def after after_heads x bound;
    let a = E.level h before_heads x in let b = E.level after after_heads x in decreases_def a b; refine_ u)

let (active @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && E.valid_head h before_heads x && E.valid_head after after_heads x} ->
    {u : unit | E.effective_active h before_heads x === E.effective_active after after_heads x} @ ghost =
  fun h p q ok after d before_heads after_heads x premise -> ghost_ (
    let refine_ premise = premise in let u = () in levels h p q ok after d before_heads after_heads x (refine_ u);
    cells h p q ok after d x (refine_ u); cell_frame_def h after x;
    E.effective_active_def h before_heads x; E.effective_active_def after after_heads x;
    let a = E.level h before_heads x in let b = E.level after after_heads x in decreases_def a b; refine_ u)

let (source @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && (not (H.mem h x) || source_ok h x)} ->
    {u : unit | not (H.mem after x) || source_ok after x} @ ghost =
  fun h p q ok after d trees x premise -> ghost_ (
    let refine_ premise = premise in let u = () in cells h p q ok after d x (refine_ u);
    cell_frame_def h after x; source_ok_def h x; source_ok_def after x;
    if H.mem h x then (
      let refine_ t = Effective_unifier_finite.unified_finite_at h trees p q ok after d x (refine_ u) in
      Level_finite_proofs.finite_scope_at after t (refine_ u); scoped_def after x; observe_def after x;
      (match H.at h x with None -> () | Some v -> match v.memo with Empty_memo | Forward _ -> ()
        | Memo (stamp, _) -> cells h p q ok after d stamp (refine_ u); cell_frame_def h after stamp; ()); ()) else ();
    refine_ u)
