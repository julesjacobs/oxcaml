open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_unifier_spec
module M = Level_unifier_metadata
module P = Level_unifier_proofs
module C = Effective_compression_proofs

let rec (unified_frame @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | H.mem h x === H.mem after x} @ ghost = fun h p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
    | Swap rest -> unified_frame h q p ok after rest x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) -> let middle = U.scan_heap h marks in
      Marked_occurs_proofs.scan_observe h needle marks x (refine_ u);
      unified_frame middle p q ok after rest x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in
      Terminal_lower_spec.completed_def h bound q middle edits tree;
      Effective_unifier_finite.lower_observe h bound edits x (refine_ u);
      unified_frame middle p q ok after rest x (refine_ u); refine_ u
    | Base old -> P.unified_frame h p q ok after old x (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) -> unified_frame h r s ok after rest x (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      unified_frame h a c left_ok middle left x (refine_ u);
      if left_ok then (unified_frame middle b e ok after right x (refine_ u); refine_ u) else refine_ u
    | Post_link (middle, rest, source, target) -> unified_frame h p q true middle rest x (refine_ u);
      Structure_spec.linkable_def middle source target; finite_def middle source;
      let s = tree_root source in let t = tree_root target in let v = U.redirect middle s t in
      Copy_heap_proofs.put_frame middle s v x; refine_ u
    | Pre_compress (middle, edits, rest) -> C.frame h middle edits x (refine_ u);
      unified_frame middle p q ok after rest x (refine_ u); refine_ u)

