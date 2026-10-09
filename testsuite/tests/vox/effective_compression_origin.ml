open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec
open Compression_origin_proofs
let rec (leaf_origin @ total) : (saved : node Pref.heap) @ immutable -> (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : Compression_spec.edits) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | Effective_compression_spec.effective_rewritten h after edits} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h after edits cut prior x premise -> ghost_ (
    Effective_compression_spec.effective_rewritten_def h after edits; match edits with Done -> let o = prior x in o
    | Write (p, q, r, d, rest) -> let v = redirect h p r in let middle = H.put h p v in
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        low_var_def h y cut; low_var_def middle y cut;
        below_def h y cut; below_def middle y cut; at_level_def h y; at_level_def middle y;
        Level_unifier_proofs.observe_write h p v y; Level_unifier_proofs.redirect_desc h p r;
        redirect_def h p r; if not (low_var middle y cut) then (let o = Origin (y, Stop) in o) else (
          let o = prior y in originates_def saved h cut y o;
          match o with Origin (root, path) ->
            let path = shortcut_path h p q r d root y path () in
            let o = Origin (root, path) in originates_def saved middle cut y o; o) in
      let o = leaf_origin saved middle after rest cut next x () in o)
