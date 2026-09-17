open Copy_spec
open Level_spec
open Level_finite_spec
open Level_unifier_spec
let (lower_observe @ total) : (h : Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound d} ->
    {u : unit | observe h x === observe (lower_heap h bound d) x
      && H.mem h x === H.mem (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound d in
  let u = () in Terminal_lower_proofs.lowering_at h bound d x (refine_ u); lower_frame_def h after x;
  observe_def h x; observe_def after x; refine_ u)
let rec (lower_finite @ total) : (h : Pref.heap) @ immutable -> (bound : int) ->
    (edits : lowering) @ immutable -> (t : tree) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && finite h t} ->
    {u : unit | finite (lower_heap h bound edits) t} @ ghost = fun h bound edits t premise -> ghost_ (
  let refine_ premise = premise in finite_def h t; tree_root_def t;
  let after = lower_heap h bound edits in finite_def after t; let x = tree_root t in
  let u = () in lower_observe h bound edits x (refine_ u); match t with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, c) -> lower_finite h bound edits c (refine_ u); refine_ u
  | Branch (_, a, b) -> lower_finite h bound edits a (refine_ u); lower_finite h bound edits b (refine_ u); refine_ u)

open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_unifier_spec

let rec (unified_finite_at @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else U.observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else U.observe after x === None)} @ immutable ghost =
  fun h trees p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
    | Swap rest -> let refine_ t = unified_finite_at h trees q p ok after rest x (refine_ u) in refine_ t
    | Scanned (needle, marks, rest) -> let middle = U.scan_heap h marks in 
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ old = trees y in let u = () in Marked_occurs_proofs.scan_observe h needle marks y (refine_ u);
        if H.mem h y then (Level_finite_proofs.scan_finite h needle marks old (refine_ u); refine_ old) else refine_ old in
      let refine_ t = unified_finite_at middle next p q ok after rest x (refine_ u) in refine_ t
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ old = trees y in let u = () in lower_observe h bound edits y (refine_ u);
        if H.mem h y then (lower_finite h bound edits old (refine_ u); refine_ old) else refine_ old in
      let refine_ t = unified_finite_at middle next p q ok after rest x (refine_ u) in refine_ t
    | Base old -> let refine_ t = Level_finite_proofs.unified_finite_at h trees p q ok after old x (refine_ u) in refine_ t
    | Resolve (r, s, _, _, rest) -> let refine_ t = unified_finite_at h trees r s ok after rest x (refine_ u) in refine_ t
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let u = () in let refine_ t = unified_finite_at h trees a c left_ok middle left y (refine_ u) in refine_ t in
      if left_ok then (let refine_ t = unified_finite_at middle next b e ok after right x (refine_ u) in refine_ t)
      else (let refine_ t = next x in refine_ t)
    | Post_link (middle, rest, source, target) ->
      let refine_ old = unified_finite_at h trees p q true middle rest x (refine_ u) in
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let v = U.redirect middle s t in
      Level_unifier_proofs.observe_write middle s v x;
      if H.mem middle x then (let refine_ t = Structure_finite_proofs.redirect middle source target old (refine_ u) in refine_ t)
      else refine_ old
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else U.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ old = trees y in let u = () in Effective_compression_proofs.frame h middle edits y (refine_ u);
        if H.mem h y then (let refine_ t = Effective_compression_proofs.finite h middle edits old (refine_ u) in refine_ t)
        else (
           U.observe_def h y; U.observe_def middle y;
          refine_ old) in
      let refine_ t = unified_finite_at middle next p q ok after rest x (refine_ u) in refine_ t)
