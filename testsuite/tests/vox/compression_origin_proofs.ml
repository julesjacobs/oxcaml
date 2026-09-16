open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec

let rec (suffix @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable -> (d : resolution) @ immutable ->
    (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | resolves h p r d && reaches h p x path && observe h x === Some Var} ->
    {out : path | reaches h r x out} @ immutable ghost = fun h p r d x path premise -> ghost_ (
    let refine_ premise = premise in resolves_def h p r d; let u = () in match d with Here -> refine_ path
    | Via (q, rest) -> reaches_def h p x path; observe_def h p;
      match path with Stop -> refine_ path | Step (next, tail) -> edge_def h p next;
        let refine_ out = suffix h q r rest x tail (refine_ u) in refine_ out)

let rec (shortcut_path @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (d : resolution) @ immutable ->
    (a : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | observe h p === Some (Link q) && resolves h p r d
      && reaches h a x path && observe h x === Some Var} ->
    {out : path | reaches (H.put h p (redirect h p r)) a x out} @ immutable ghost =
  fun h p q r d a x path premise -> ghost_ (
    let refine_ premise = premise in let v = redirect h p r in let after = H.put h p v in
    reaches_def h a x path; let u = () in
    Compression_path_proofs.resolution_terminal h p r d (refine_ u); terminal_def h r;
    resolves_def h p r d;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p r;
    match path with Stop -> reaches_def after a x path; refine_ path
    | Step (next, rest) ->
      let refine_ tail = shortcut_path h p q r d next x rest (refine_ u) in
      if a === p then (
        edge_def h p next; observe_def h p;
        let old_tail = Compression_path_proofs.tail d in
        Compression_path_proofs.tail_resolves h p q r d (refine_ u);
        let refine_ next_d = Compression_path_proofs.redirect_resolution h p r q old_tail (refine_ u) in
        let refine_ last = suffix after q r next_d x tail (refine_ u) in
        let out = Step (r, last) in reaches_def after a x out; edge_def after p r;
        redirect_def h p r; observe_def after p; Copy_heap_proofs.put_frame h p v p; refine_ out)
      else (
        let out = Step (next, tail) in reaches_def after a x out;
        edge_def h a next; edge_def after a next; Copy_heap_proofs.put_frame h p v a; refine_ out))

let rec (leaf_origin @ total) : (saved : Pref.heap) @ immutable -> (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (edits : Compression_spec.edits) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | Compression_spec.rewritten h after edits} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h after edits cut prior x premise -> ghost_ (
    let refine_ premise = premise in Compression_spec.rewritten_def h after edits; let u = () in
    match edits with Done -> let refine_ o = prior x in refine_ o
    | Write (p, q, r, d, rest) -> let v = redirect h p r in let middle = H.put h p v in
      let next : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in low_var_def h y cut; low_var_def middle y cut;
        below_def h y cut; below_def middle y cut; at_level_def h y; at_level_def middle y;
        Level_unifier_proofs.observe_write h p v y; Level_unifier_proofs.redirect_desc h p r;
        redirect_def h p r; Copy_heap_proofs.put_frame h p v y;
        if not (low_var middle y cut) then (let o = Origin (y, Stop) in refine_ o) else (
          let refine_ o = prior y in originates_def saved h cut y o;
          match o with Origin (root, path) ->
            let refine_ path = shortcut_path h p q r d root y path (refine_ u) in
            let o = Origin (root, path) in originates_def saved middle cut y o; refine_ o) in
      let refine_ o = leaf_origin saved middle after rest cut next x (refine_ u) in refine_ o)
