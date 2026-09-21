open Copy_spec
open Level_spec
module U = Level_unifier_spec
open Level_finite_spec

type derivation =
  | Base of Level_unifier_spec.derivation
  | Resolve of node Pref.t * node Pref.t * U.resolution * U.resolution * derivation
  | Children of node Pref.t * node Pref.t * node Pref.t * node Pref.t
      * node Pref.heap * bool * derivation * derivation
  | Post_link of node Pref.heap * derivation * tree * tree
  | Pre_compress of node Pref.heap * Compression_spec.edits * derivation
  [@@inductive]

let[@def] rec (unified @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable)
    (ok : bool) (after : node Pref.heap @ immutable) (d : derivation @ immutable) = ghost_ (
  H.mem h p && H.mem h q && active h p && active h q && match d with
  | Base old -> Level_unifier_spec.unified h p q ok after old
  | Resolve (r, s, rp, sq, rest) -> U.resolves h p r rp && U.resolves h q s sq
    && unified h r s ok after rest
  | Children (a, b, c, e, middle, left_ok, left, right) ->
    U.observe h p === Some (Arrow (a, b)) && U.observe h q === Some (Arrow (c, e))
    && unified h a c left_ok middle left
    && (if left_ok then unified middle b e ok after right else not ok && after === middle)
  | Post_link (middle, rest, source, target) -> ok && unified h p q true middle rest
    && Structure_spec.linkable middle source target
    && after === H.put middle (tree_root source) (U.redirect middle (tree_root source) (tree_root target))
  | Pre_compress (middle, edits, rest) -> Compression_spec.rewritten h middle edits
    && unified middle p q ok after rest)

type result = #{ok : bool; state : node Pref.token; derivation : derivation @@ ghost}
