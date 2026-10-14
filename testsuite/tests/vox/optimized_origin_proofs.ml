open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec
open Optimized_unifier_spec

let rec (unified_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h cut prior p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d; match d with
    | Base old -> Leaf_provenance_proofs.unified_leaf_origin saved h cut prior p q ok after old x ()
    | Resolve (r, s, _, _, rest) | List_children (r, s, rest) -> unified_leaf_origin saved h cut prior r s ok after rest x ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        unified_leaf_origin saved h cut prior a c left_ok middle left y () in
      if left_ok then (unified_leaf_origin saved middle cut next b e ok after right x ())
      else (next x)
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        unified_leaf_origin saved h cut prior p q true middle rest y () in
      Structure_origin_proofs.rewrite_leaf_origin saved middle cut next source target x ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        Compression_origin_proofs.leaf_origin saved h middle edits cut prior y () in
      unified_leaf_origin saved middle cut next p q ok after rest x ())
