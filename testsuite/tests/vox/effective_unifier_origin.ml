open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Provenance_proofs
open Leaf_provenance_spec
open Effective_unifier_spec
open Marked_occurs_proofs

let rec (unified_leaf_origin @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h cut prior p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d; match d with
    | Swap rest -> let o = unified_leaf_origin saved h cut prior q p ok after rest x () in o
    | Scanned (needle, marks, rest) ->
      let mid = Level_unifier_spec.scan_heap h marks in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid y cut) || originates saved mid cut y o} @ immutable) @ total = fun y ->
        scan_below h needle marks y cut ();
        scan_observe h needle marks y (); low_var_def h y cut; low_var_def mid y cut;
        let o = prior y in originates_def saved h cut y o;
        originates_def saved mid cut y o;
        if low_var mid y cut then (match o with Origin (root, path) ->
          scan_path h needle marks root y path (); o) else o in
      let o = unified_leaf_origin saved mid cut middle_prior p q ok after rest x () in o
    | Terminal_lower (bound, edits, tree, rest) ->
      let mid = lower_heap h bound edits in
      Terminal_lower_spec.completed_def h bound q mid edits tree;
      unified_def mid p q ok after rest;
      (match rest with Base old -> Level_unifier_spec.unified_def mid p q ok after old;
        (match old with Level_unifier_spec.Bind_left _ ->
          let o = Terminal_lower_origin.lower_bind_leaf_origin saved h cut prior p q bound edits tree x () in o
        | _ -> let o = Origin (x, Stop) in o)
      | _ -> let o = Origin (x, Stop) in o)
    | Base old -> let o = Leaf_provenance_proofs.unified_leaf_origin saved h cut prior p q ok after old x () in o
    | Resolve (r, s, _, _, rest) -> let o = unified_leaf_origin saved h cut prior r s ok after rest x () in o
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let o = unified_leaf_origin saved h cut prior a c left_ok middle left y () in o in
      if left_ok then (let o = unified_leaf_origin saved middle cut next b e ok after right x () in o)
      else (let o = next x in o)
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let o = unified_leaf_origin saved h cut prior p q true middle rest y () in o in
      let o = Structure_origin_proofs.rewrite_leaf_origin saved middle cut next source target x () in o
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let o = Effective_compression_origin.leaf_origin saved h middle edits cut prior y () in o in
      let o = unified_leaf_origin saved middle cut next p q ok after rest x () in o)
