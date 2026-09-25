open Copy_spec
open Level_spec
open Level_unifier_spec
let rec (unified_terminal @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d && terminal after x} ->
    {u : unit | terminal h x} @ ghost = fun h p q ok after d x premise -> ghost_ (
      Effective_unifier_spec.unified_def h p q ok after d;
      match d with
      | Effective_unifier_spec.Base old -> Representative_mutation.base_terminal h p q ok after old x (); ()
      | Effective_unifier_spec.Resolve (r, s, _, _, rest) ->
        unified_terminal h r s ok after rest x (); ()
      | Effective_unifier_spec.Children (a, b, c, e, mid, left_ok, left, right) ->
        if left_ok then (
          unified_terminal mid b e ok after right x ();
          unified_terminal h a c left_ok mid left x (); ())
        else (unified_terminal h a c left_ok mid left x (); ())
      | Effective_unifier_spec.Post_link (mid, rest, source, target) ->
        let s = Level_finite_spec.tree_root source in
        let t = Level_finite_spec.tree_root target in
        Representative_mutation.redirect_terminal mid s t x ();
        unified_terminal h p q true mid rest x (); ()
      | Effective_unifier_spec.Pre_compress (mid, edits, rest) ->
        unified_terminal mid p q ok after rest x ();
        Effective_compression_proofs.frame h mid edits x ();
        terminal_def h x; terminal_def mid x; observe_def h x; observe_def mid x; ()
      | Effective_unifier_spec.Swap rest -> unified_terminal h q p ok after rest x (); ()
      | Effective_unifier_spec.Scanned (needle, marks, rest) ->
        let mid = scan_heap h marks in unified_terminal mid p q ok after rest x ();
        Marked_occurs_proofs.scan_observe h needle marks x ();
        terminal_def h x; terminal_def mid x; ()
      | Effective_unifier_spec.Terminal_lower (bound, edits, tree, rest) ->
        let mid = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q mid edits tree;
        unified_terminal mid p q ok after rest x ();
        Effective_unifier_finite.lower_observe h bound edits x ();
        terminal_def h x; terminal_def mid x; ())

let (coverage @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d
      && Representative_level.representative_covered h cut pool x} ->
    {u : unit | Representative_level.representative_covered after cut pool x} @ ghost =
  fun h p q ok after d cut pool x premise -> ghost_ (
    Representative_level.representative_covered_def h cut pool x;
    Representative_level.representative_covered_def after cut pool x;
    if terminal after x then (
      unified_terminal h p q ok after d x ();
      Effective_unifier_frame.unified_frame h p q ok after d x ();
      Effective_unifier_metadata.cells h p q ok after d x ();
      Effective_unifier_metadata.cell_frame_def h after x;
      Generalize_spec.covered_def h cut pool x;
      Generalize_spec.covered_def after cut pool x;
      at_level_def h x; at_level_def after x;
      (match H.at h x, H.at after x with
      | Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
      ())
    else ())

