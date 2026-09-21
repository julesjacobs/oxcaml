open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec
open Optimized_unifier_spec

let rec (unified_leaf_origin @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h cut prior p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; let u = () in match d with
    | Base old -> let o = Leaf_provenance_proofs.unified_leaf_origin saved h cut prior p q ok after old x (u) in o
    | Resolve (r, s, _, _, rest) -> let o = unified_leaf_origin saved h cut prior r s ok after rest x (u) in o
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let o = unified_leaf_origin saved h cut prior a c left_ok middle left y (u) in o in
      if left_ok then (let o = unified_leaf_origin saved middle cut next b e ok after right x (u) in o)
      else (let o = next x in o)
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let o = unified_leaf_origin saved h cut prior p q true middle rest y (u) in o in
      let o = Structure_origin_proofs.rewrite_leaf_origin saved middle cut next source target x (u) in o
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let o = Compression_origin_proofs.leaf_origin saved h middle edits cut prior y (u) in o in
      let o = unified_leaf_origin saved middle cut next p q ok after rest x (u) in o)
