open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec

let[@def] rec (mentions @ total) (t : ty @ immutable) (x : node Pref.t @ immutable) =
  ghost_ (match t with Variable y -> x === y | Boolean -> false
    | Function (a, b) -> mentions a x || mentions b x)

let rec (path_mentions @ total) : (h : Pref.heap) @ immutable -> (t : tree) @ immutable ->
    (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | finite h t && reaches h (tree_root t) x path && observe h x === Some Var} ->
    {u : unit | mentions (readback t) x} @ ghost = fun h t x path premise -> ghost_ (
    let refine_ premise = premise in finite_def h t; tree_root_def t; readback_def t;
    let p = tree_root t in let ty = readback t in mentions_def ty x;
    reaches_def h p x path; observe_def h p; let u = () in
    match path with Stop -> (match t with Free _ | Constant_tree _ | Alias_tree _ | Branch _ -> refine_ u)
    | Step (next, rest) -> edge_def h p next;
      match t with Free _ | Constant_tree _ -> refine_ u
      | Alias_tree (_, child) -> path_mentions h child x rest (refine_ u); refine_ u
      | Branch (_, a, b) -> if next === tree_root a then (path_mentions h a x rest (refine_ u); refine_ u)
        else (path_mentions h b x rest (refine_ u); refine_ u))

let rec (path_of_mention @ total) : (h : Pref.heap) @ immutable -> (t : tree) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | finite h t && mentions (readback t) x} ->
    {path : path | reaches h (tree_root t) x path} @ immutable ghost = fun h t x premise -> ghost_ (
    let refine_ premise = premise in finite_def h t; tree_root_def t; readback_def t;
    let p = tree_root t in let ty = readback t in mentions_def ty x; observe_def h p;
    let u = () in match t with Free _ | Constant_tree _ ->
      let path = Stop in reaches_def h p x path; refine_ path
    | Alias_tree (_, child) -> let refine_ tail = path_of_mention h child x (refine_ u) in
      let q = tree_root child in let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; refine_ path
    | Branch (_, a, b) ->
      if mentions (readback a) x then (
        let refine_ tail = path_of_mention h a x (refine_ u) in let q = tree_root a in
        let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; refine_ path)
      else (
        let refine_ tail = path_of_mention h b x (refine_ u) in let q = tree_root b in
        let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; refine_ path))

let rec (rewrite_var_path @ total) : (h : Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable -> (a : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root target) && readback source === readback target
      && reaches h a x path && observe h x === Some Var} ->
    {out : path | reaches (H.put h (tree_root source) (redirect h (tree_root source) (tree_root target))) a x out} @ immutable ghost =
  fun h source target a x path premise -> ghost_ (
    let refine_ premise = premise in let p = tree_root source in let q = tree_root target in
    let v = redirect h p q in let after = H.put h p v in let u = () in
    if a === p then (
      finite_def h source; tree_root_def source; observe_def h p; path_mentions h source x path (refine_ u);
      Structure_finite_proofs.equivalent_avoids h source target (refine_ u);
      Structure_finite_proofs.frame_avoids h p q target (refine_ u);
      let refine_ tail = path_of_mention after target x (refine_ u) in
      let out = Step (q, tail) in reaches_def after a x out; edge_def after p q;
      redirect_def h p q; observe_def after p; Copy_heap_proofs.put_frame h p v p; refine_ out)
    else (
      reaches_def h a x path; match path with Stop -> reaches_def after a x path; refine_ path
      | Step (next, rest) ->
        let refine_ tail = rewrite_var_path h source target next x rest (refine_ u) in
        let out = Step (next, tail) in reaches_def after a x out;
        edge_def h a next; edge_def after a next; Copy_heap_proofs.put_frame h p v a; refine_ out))

let (rewrite_leaf_origin @ total) : (saved : Pref.heap) @ immutable -> (h : Pref.heap) @ immutable ->
    (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (source : tree) @ immutable -> (target : tree) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root target) && readback source === readback target} ->
    {o : origin | let after = H.put h (tree_root source) (redirect h (tree_root source) (tree_root target)) in
      not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h cut prior source target x premise -> ghost_ (
    let refine_ premise = premise in let p = tree_root source in let q = tree_root target in
    let v = redirect h p q in let after = H.put h p v in
    low_var_def h x cut; low_var_def after x cut; below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q;
    redirect_def h p q; Copy_heap_proofs.put_frame h p v x;
    if not (low_var after x cut) then (let o = Origin (x, Stop) in refine_ o) else (
      let refine_ o = prior x in originates_def saved h cut x o;
      match o with Origin (root, path) -> let u = () in
        let refine_ path = rewrite_var_path h source target root x path (refine_ u) in
        let o = Origin (root, path) in originates_def saved after cut x o; refine_ o))
