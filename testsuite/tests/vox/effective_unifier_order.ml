open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module E = Effective_level
module R = Representative_level
module M = Effective_unifier_metadata

let (same_level @ total) : (h : Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | E.valid_head h a x && E.valid_head h b x} ->
    {u : unit | E.level h a x === E.level h b x} @ ghost = fun h a b x premise -> ghost_ (
      let refine_ premise = premise in E.valid_head_def h a x; E.valid_head_def h b x;
      E.level_def h a x; E.level_def h b x;
      if H.mem h x then (let r = a x in let s = b x in let u = () in
        R.unique h x r.root r.path s.root s.path (refine_ u); ()) else ();
      let u = () in refine_ u)

let (rebase @ total) : (h : Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (va : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h a x})) @ total ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h b x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h a x} ->
    {u : unit | E.effective_ordered h b x} @ ghost = fun h a b va vb x premise -> ghost_ (
      let refine_ premise = premise in E.effective_ordered_def h a x; E.effective_ordered_def h b x;
      (match H.at h x with Some {desc = Arrow (left, right); level = Finite n; _} ->
        va left; vb left; va right; vb right; let u = () in
        same_level h a b left (refine_ u); same_level h a b right (refine_ u);
        E.effective_below_def h a left n; E.effective_below_def h b left n;
        E.effective_below_def h a right n; E.effective_below_def h b right n; ()
      | _ -> ()); let u = () in refine_ u)

let (redirect_below @ total) : (h : Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | H.mem h source && terminal h source && H.mem h target && terminal h target
      && not (source === target)
      && (match at_level h source with Generic -> false | Finite n -> n >= 0 && Level_spec.below h target n)
      && E.valid_head h before_heads x
      && E.valid_head (H.put h source (U.redirect h source target)) after_heads x
      && E.effective_below h before_heads x bound} ->
    {u : unit | E.effective_below (H.put h source (U.redirect h source target)) after_heads x bound} @ ghost =
  fun h source target before_heads after_heads x bound premise -> ghost_ (
    let refine_ premise = premise in let v = U.redirect h source target in let after = H.put h source v in
    E.effective_below_def h before_heads x bound; E.effective_below_def after after_heads x bound;
    E.valid_head_def h before_heads x; E.valid_head_def after after_heads x;
    E.level_def h before_heads x; E.level_def after after_heads x;
    let old = before_heads x in let next = after_heads x in let u = () in
    Compression_path_proofs.resolution_terminal h x old.root old.path (refine_ u);
    Level_spec.below_def h old.root bound;
    let refine_ path = Representative_mutation.redirect_low h source target x old.root old.path bound (refine_ u) in
    let root = if old.root === source then target else old.root in
    R.unique after x root path next.root next.path (refine_ u);
    Level_spec.below_def after root bound;
    Copy_heap_proofs.put_frame h source v x; refine_ u)

let (redirect_order @ total) : (h : Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (va : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h a x})) @ total ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (H.put h source (U.redirect h source target)) b x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h source && terminal h source && H.mem h target && terminal h target
      && not (source === target)
      && (match at_level h source with Generic -> false | Finite n -> n >= 0 && Level_spec.below h target n)
      && E.effective_ordered h a x} ->
    {u : unit | E.effective_ordered (H.put h source (U.redirect h source target)) b x} @ ghost =
  fun h source target a b va vb x premise -> ghost_ (
    let refine_ premise = premise in let v = U.redirect h source target in let after = H.put h source v in
    U.redirect_def h source target; terminal_def h source; observe_def h source;
    Copy_heap_proofs.put_frame h source v x;
    E.effective_ordered_def h a x; E.effective_ordered_def after b x;
    (match H.at h x with Some {desc = Arrow (left, right); level = Finite n; _} ->
      va left; vb left; va right; vb right; let u = () in
      redirect_below h source target a b left n (refine_ u);
      redirect_below h source target a b right n (refine_ u); ()
    | _ -> ()); let u = () in refine_ u)

let rec (ordered @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (va : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h a x})) @ total ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head after b x})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h a x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | E.effective_ordered after b x} @ ghost =
  fun h p q ok after d a b va vb order x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d; let u = () in
    match d with
    | Base old -> U.unified_def h p q ok after old; order x;
      (match old with
      | U.Bind_left _ -> active_def h p; redirect_order h p q a b va (refine_ vb) x (refine_ u); refine_ u
      | U.Bind_right _ -> active_def h q; redirect_order h q p a b va (refine_ vb) x (refine_ u); refine_ u
      | U.Same | U.Constants | U.Occurs_left _ | U.Occurs_right _ | U.Clash -> rebase h a b va (refine_ vb) x (refine_ u); refine_ u
      | _ -> refine_ u)
    | Swap rest -> ordered h q p ok after rest a b va vb order x (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) -> ordered h r s ok after rest a b va vb order x (refine_ u); refine_ u
    | Children (left, right, other_left, other_right, middle, left_ok, first, second) ->
      let raw_heads : ((y : node Pref.t) @ immutable total -> {r : R.representative | not (H.mem middle y) || resolves middle y r.root r.path} @ immutable total) @ total = fun y -> va y; let u = () in
        let refine_ out = M.head h left other_left left_ok middle first a y (refine_ u) in refine_ out in
      let[@def] mid_heads : E.heads = fun y -> let refine_ r = raw_heads y in r in
      let mid_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle mid_heads y}) @ total = fun y ->
        mid_heads_def y; let refine_ r = raw_heads y in E.valid_head_def middle mid_heads y; let u = () in refine_ u in
      let mid_order : ((y : node Pref.t) @ immutable -> {u : unit | E.effective_ordered middle mid_heads y}) @ total = fun y ->
        let u = () in let refine_ out = ordered h left other_left left_ok middle first a mid_heads va mid_valid order y (refine_ u) in refine_ out in
      if left_ok then (ordered middle right other_right ok after second mid_heads b mid_valid vb mid_order x (refine_ u); refine_ u)
      else (mid_order x; rebase middle mid_heads b mid_valid (refine_ vb) x (refine_ u); refine_ u)
    | Post_link (middle, rest, source, target) ->
      let raw_heads : ((y : node Pref.t) @ immutable total -> {r : R.representative | not (H.mem middle y) || resolves middle y r.root r.path} @ immutable total) @ total = fun y -> va y; let u = () in
        let refine_ out = M.head h p q true middle rest a y (refine_ u) in refine_ out in
      let[@def] mid_heads : E.heads = fun y -> let refine_ r = raw_heads y in r in
      let mid_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle mid_heads y}) @ total = fun y ->
        mid_heads_def y; let refine_ r = raw_heads y in E.valid_head_def middle mid_heads y; let u = () in refine_ u in
      ordered h p q true middle rest a mid_heads va mid_valid order x (refine_ u);
      Structure_spec.linkable_def middle source target; finite_def middle source; finite_def middle target;
      let s = tree_root source in let t = tree_root target in active_def middle s;
      redirect_order middle s t mid_heads b mid_valid (refine_ vb) x (refine_ u); refine_ u
    | Pre_compress (middle, edits, rest) ->
      let raw_heads : ((y : node Pref.t) @ immutable total -> {r : R.representative | not (H.mem middle y) || resolves middle y r.root r.path} @ immutable total) @ total = fun y -> va y; let u = () in
        let refine_ out = Effective_compression_metadata.head h middle edits a y (refine_ u) in refine_ out in
      let[@def] mid_heads : E.heads = fun y -> let refine_ r = raw_heads y in r in
      let mid_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle mid_heads y}) @ total = fun y ->
        mid_heads_def y; let refine_ r = raw_heads y in E.valid_head_def middle mid_heads y; let u = () in refine_ u in
      let mid_order : ((y : node Pref.t) @ immutable -> {u : unit | E.effective_ordered middle mid_heads y}) @ total = fun y ->
        order y; let u = () in let refine_ out = Effective_compression_metadata.ordered h middle edits a mid_heads va mid_valid y (refine_ u) in refine_ out in
      ordered middle p q ok after rest mid_heads b mid_valid vb mid_order x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->
      let middle = scan_heap h marks in
      let mid_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle a y}) @ total = fun y ->
        va y; let u = () in let refine_ out = Effective_scan_proofs.head h a needle marks y (refine_ u) in refine_ out in
      let mid_order : ((y : node Pref.t) @ immutable -> {u : unit | E.effective_ordered middle a y}) @ total = fun y ->
        order y; let u = () in let refine_ out = Effective_scan_proofs.order h a needle marks y (refine_ u) in refine_ out in
      ordered middle p q ok after rest a b mid_valid vb mid_order x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) ->
      let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h middle y}) @ total = fun y ->
        let u = () in let refine_ out = Terminal_lower_proofs.lowering_at h bound edits y (refine_ u) in refine_ out in
      let mid_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle a y}) @ total = fun y ->
        va y; let u = () in let refine_ out = Effective_lower_proofs.frame_head h middle a frame y (refine_ u) in refine_ out in
      let mid_order : ((y : node Pref.t) @ immutable -> {u : unit | E.effective_ordered middle a y}) @ total = fun y ->
        order y; let u = () in let refine_ out = Terminal_lower_proofs.completed_ordered h a va bound q middle edits tree y (refine_ u) in refine_ out in
      ordered middle p q ok after rest a b mid_valid vb mid_order x (refine_ u); refine_ u)
