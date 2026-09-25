open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_unifier_spec
module M = Level_unifier_metadata
module P = Level_unifier_proofs
module C = Effective_compression_proofs

let rec (unified_frame @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | H.mem h x === H.mem after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Swap rest -> unified_frame h q p ok after rest x (); ()
    | Scanned (needle, marks, rest) -> let middle = U.scan_heap h marks in
      Marked_occurs_proofs.scan_observe h needle marks x ();
      unified_frame middle p q ok after rest x (); ()
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in
      Terminal_lower_spec.completed_def h bound q middle edits tree;
      Effective_unifier_finite.lower_observe h bound edits x ();
      unified_frame middle p q ok after rest x (); ()
    | Base old -> P.unified_frame h p q ok after old x (); ()
    | Resolve (r, s, _, _, rest) -> unified_frame h r s ok after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      unified_frame h a c left_ok middle left x ();
      if left_ok then (unified_frame middle b e ok after right x (); ()) else ()
    | Post_link (middle, rest, source, target) -> unified_frame h p q true middle rest x ();
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let _ = U.redirect middle s t in
      ()
    | Pre_compress (middle, edits, rest) -> C.frame h middle edits x ();
      unified_frame middle p q ok after rest x (); ())

