open Copy_spec
open Level_unifier_spec
open Level_finite_spec

let[@def] rec (avoids @ total) (p : node Pref.t @ immutable) (t : tree @ immutable) =
  ghost_ (not (tree_root t === p) && match t with
    | Free _ | Constant_tree _ -> true
    | Alias_tree (_, child) -> avoids p child
    | Branch (_, a, b) -> avoids p a && avoids p b)

let rec (smaller_avoids @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (t : tree) @ immutable ->
    {u : unit | finite h source && finite h t && weight (readback t) < weight (readback source)} ->
    {u : unit | avoids (tree_root source) t} @ ghost = fun h source t premise -> ghost_ (
    if tree_root t === tree_root source then (Level_finite_proofs.finite_unique h t source (); ());
    let p = tree_root source in avoids_def p t; finite_def h t; readback_def t;
    match t with Free _ | Constant_tree _ -> ()
    | Alias_tree (_, child) -> smaller_avoids h source child (); ()
    | Branch (_, a, b) -> let ta = readback a in let tb = readback b in let ty = Function (ta, tb) in
      weight_def ty; let _ = Level_unifier_proofs.weight_positive ta in
      let _ = Level_unifier_proofs.weight_positive tb in
      smaller_avoids h source a (); smaller_avoids h source b (); ())

let (equivalent_avoids @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root target) && readback source === readback target} ->
    {u : unit | avoids (tree_root source) target} @ ghost = fun h source target premise -> ghost_ (
    let p = tree_root source in
    avoids_def p target; finite_def h target; tree_root_def target;
    let q = tree_root target in terminal_def h q; readback_def target; match target with Free _ | Constant_tree _ | Alias_tree _ -> ()
    | Branch (_, a, b) -> let ta = readback a in let tb = readback b in let ty = Function (ta, tb) in
      weight_def ty; let _ = Level_unifier_proofs.weight_positive ta in
      let _ = Level_unifier_proofs.weight_positive tb in
      smaller_avoids h source a (); smaller_avoids h source b (); ())

let rec (frame_avoids @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (t : tree) @ immutable ->
    {u : unit | finite h t && avoids p t} ->
    {u : unit | finite (H.put h p (redirect h p q)) t} @ ghost = fun h p q t premise -> ghost_ (
    let after = H.put h p (redirect h p q) in
    finite_def h t; finite_def after t; avoids_def p t; tree_root_def t;
    let x = tree_root t in let v = redirect h p q in Level_unifier_proofs.observe_write h p v x;
    match t with Free _ | Constant_tree _ -> ()
    | Alias_tree (_, child) -> frame_avoids h p q child (); ()
    | Branch (_, a, b) -> frame_avoids h p q a (); frame_avoids h p q b (); ())

let rec (replace @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable -> (old : tree) @ immutable ->
    {u : unit | finite h source && finite h old && readback source === readback target
      && finite (H.put h (tree_root source) (redirect h (tree_root source) (tree_root target))) target} ->
    {t : tree | tree_root t === tree_root old && readback t === readback old
      && finite (H.put h (tree_root source) (redirect h (tree_root source) (tree_root target))) t} @ immutable ghost =
  fun h source target old premise -> ghost_ (
    let p = tree_root source in let q = tree_root target in
    let after = H.put h p (redirect h p q) in let x = tree_root old in if x === p then (
      Level_finite_proofs.finite_unique h old source ();
      let t = Alias_tree (p, target) in tree_root_def t; readback_def t; finite_def after t;
      let v = redirect h p q in Level_unifier_proofs.observe_write h p v p;
      Level_unifier_proofs.redirect_desc h p q; t)
    else (
      finite_def h old; tree_root_def old; readback_def old;
      let v = redirect h p q in Level_unifier_proofs.observe_write h p v x;
      match old with Free _ | Constant_tree _ -> finite_def after old; old
      | Alias_tree (x, child) -> let child = replace h source target child () in
        let t = Alias_tree (x, child) in tree_root_def t; readback_def t; finite_def after t; t
      | Branch (x, a, b) -> let a = replace h source target a () in
        let b = replace h source target b () in
        let t = Branch (x, a, b) in tree_root_def t; readback_def t; finite_def after t; t))

let (redirect @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable -> (old : tree) @ immutable ->
    {u : unit | finite h source && finite h target && finite h old
      && not (tree_root source === tree_root target) && terminal h (tree_root target)
      && readback source === readback target} ->
    {t : tree | tree_root t === tree_root old && readback t === readback old
      && finite (H.put h (tree_root source) (Level_unifier_spec.redirect h (tree_root source) (tree_root target))) t} @ immutable ghost =
  fun h source target old premise -> ghost_ (
    equivalent_avoids h source target ();
    let p = tree_root source in let q = tree_root target in frame_avoids h p q target ();
    let t = replace h source target old () in t)
