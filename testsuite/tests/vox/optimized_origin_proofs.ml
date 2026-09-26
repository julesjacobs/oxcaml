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
    let refine_ premise = premise in unified_def h p q ok after d; let u = () in match d with
    | Base old -> let refine_ o = Leaf_provenance_proofs.unified_leaf_origin saved h cut prior p q ok after old x (refine_ u) in refine_ o
    | Resolve (r, s, _, _, rest) | List_children (r, s, rest) -> let refine_ o = unified_leaf_origin saved h cut prior r s ok after rest x (refine_ u) in refine_ o
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = unified_leaf_origin saved h cut prior a c left_ok middle left y (refine_ u) in refine_ o in
      if left_ok then (let refine_ o = unified_leaf_origin saved middle cut next b e ok after right x (refine_ u) in refine_ o)
      else (let refine_ o = next x in refine_ o)
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = unified_leaf_origin saved h cut prior p q true middle rest y (refine_ u) in refine_ o in
      let refine_ o = Structure_origin_proofs.rewrite_leaf_origin saved middle cut next source target x (refine_ u) in refine_ o
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = Compression_origin_proofs.leaf_origin saved h middle edits cut prior y (refine_ u) in refine_ o in
      let refine_ o = unified_leaf_origin saved middle cut next p q ok after rest x (refine_ u) in refine_ o)
