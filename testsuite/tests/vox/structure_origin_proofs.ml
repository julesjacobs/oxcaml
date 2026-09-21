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

let rec (path_mentions @ total) : (h : node Pref.heap) @ immutable -> (t : tree) @ immutable ->
    (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | finite h t && reaches h (tree_root t) x path && observe h x === Some Var} ->
    {u : unit | mentions (readback t) x} @ ghost = fun h t x path premise -> ghost_ (
    finite_def h t; tree_root_def t; readback_def t;
    let p = tree_root t in let ty = readback t in mentions_def ty x;
    reaches_def h p x path; observe_def h p; match path with Stop -> (match t with Free _ | Constant_tree _ | Alias_tree _ | Branch _ -> ())
    | Step (next, rest) -> edge_def h p next;
      match t with Free _ | Constant_tree _ -> ()
      | Alias_tree (_, child) -> path_mentions h child x rest (); ()
      | Branch (_, a, b) -> if next === tree_root a then (path_mentions h a x rest (); ())
        else (path_mentions h b x rest (); ()))

let rec (path_of_mention @ total) : (h : node Pref.heap) @ immutable -> (t : tree) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | finite h t && mentions (readback t) x} ->
    {path : path | reaches h (tree_root t) x path} @ immutable ghost = fun h t x premise -> ghost_ (
    finite_def h t; tree_root_def t; readback_def t;
    let p = tree_root t in let ty = readback t in mentions_def ty x; observe_def h p;
    match t with Free _ | Constant_tree _ ->
      let path = Stop in reaches_def h p x path; path
    | Alias_tree (_, child) -> let tail = path_of_mention h child x () in
      let q = tree_root child in let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; path
    | Branch (_, a, b) ->
      if mentions (readback a) x then (
        let tail = path_of_mention h a x () in let q = tree_root a in
        let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; path)
      else (
        let tail = path_of_mention h b x () in let q = tree_root b in
        let path = Step (q, tail) in edge_def h p q; reaches_def h p x path; path))

let rec (rewrite_var_path @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable -> (a : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root target) && readback source === readback target
      && reaches h a x path && observe h x === Some Var} ->
    {out : path | reaches (H.put h (tree_root source) (redirect h (tree_root source) (tree_root target))) a x out} @ immutable ghost =
  fun h source target a x path premise -> ghost_ (
    let p = tree_root source in let q = tree_root target in
    let v = redirect h p q in let after = H.put h p v in if a === p then (
      finite_def h source; tree_root_def source; observe_def h p; path_mentions h source x path ();
      Structure_finite_proofs.equivalent_avoids h source target ();
      Structure_finite_proofs.frame_avoids h p q target ();
      let tail = path_of_mention after target x () in
      let out = Step (q, tail) in reaches_def after a x out; edge_def after p q;
      redirect_def h p q; observe_def after p; out)
    else (
      reaches_def h a x path; match path with Stop -> reaches_def after a x path; path
      | Step (next, rest) ->
        let tail = rewrite_var_path h source target next x rest () in
        let out = Step (next, tail) in reaches_def after a x out;
        edge_def h a next; edge_def after a next; out))

let (rewrite_leaf_origin @ total) : (saved : node Pref.heap) @ immutable -> (h : node Pref.heap) @ immutable ->
    (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (source : tree) @ immutable -> (target : tree) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root target) && readback source === readback target} ->
    {o : origin | let after = H.put h (tree_root source) (redirect h (tree_root source) (tree_root target)) in
      not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved h cut prior source target x premise -> ghost_ (
    let p = tree_root source in let q = tree_root target in
    let v = redirect h p q in let after = H.put h p v in
    low_var_def h x cut; low_var_def after x cut; below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q;
    redirect_def h p q; if not (low_var after x cut) then (let o = Origin (x, Stop) in o) else (
      let o = prior x in originates_def saved h cut x o;
      match o with Origin (root, path) -> let path = rewrite_var_path h source target root x path () in
        let o = Origin (root, path) in originates_def saved after cut x o; o))
