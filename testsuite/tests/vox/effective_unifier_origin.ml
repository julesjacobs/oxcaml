open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Provenance_proofs
open Leaf_provenance_spec
open Effective_unifier_spec
open Marked_occurs_proofs

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
    | Swap rest -> let refine_ o = unified_leaf_origin saved h cut prior q p ok after rest x (refine_ u) in refine_ o
    | Scanned (needle, marks, rest) ->
      let mid = Level_unifier_spec.scan_heap h marks in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid y cut) || originates saved mid cut y o} @ immutable) @ total = fun y ->
        let u = () in scan_below h needle marks y cut (refine_ u);
        scan_observe h needle marks y (refine_ u); low_var_def h y cut; low_var_def mid y cut;
        let refine_ o = prior y in originates_def saved h cut y o;
        originates_def saved mid cut y o;
        if low_var mid y cut then (match o with Origin (root, path) ->
          scan_path h needle marks root y path (refine_ u); refine_ o) else refine_ o in
      let refine_ o = unified_leaf_origin saved mid cut middle_prior p q ok after rest x (refine_ u) in refine_ o
    | Terminal_lower (bound, edits, tree, rest) ->
      let mid = lower_heap h bound edits in
      Terminal_lower_spec.completed_def h bound q mid edits tree;
      unified_def mid p q ok after rest;
      (match rest with Base old -> Level_unifier_spec.unified_def mid p q ok after old;
        (match old with Level_unifier_spec.Bind_left _ ->
          let refine_ o = Terminal_lower_origin.lower_bind_leaf_origin saved h cut prior p q bound edits tree x (refine_ u) in refine_ o
        | _ -> let o = Origin (x, Stop) in refine_ o)
      | _ -> let o = Origin (x, Stop) in refine_ o)
    | Base old -> let refine_ o = Leaf_provenance_proofs.unified_leaf_origin saved h cut prior p q ok after old x (refine_ u) in refine_ o
    | Resolve (r, s, _, _, rest) -> let refine_ o = unified_leaf_origin saved h cut prior r s ok after rest x (refine_ u) in refine_ o
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
        let u = () in let refine_ o = Effective_compression_origin.leaf_origin saved h middle edits cut prior y (refine_ u) in refine_ o in
      let refine_ o = unified_leaf_origin saved middle cut next p q ok after rest x (refine_ u) in refine_ o)
