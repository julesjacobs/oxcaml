open Copy_spec
open Level_unifier_spec
open Level_finite_spec
open Structure_finite_proofs

let (forward @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && readback source === readback target} ->
    {u : unit | node_equation (H.put h (tree_root source) (Level_unifier_spec.redirect h (tree_root source) (tree_root target))) rho x} @ ghost =
  fun h source target rho model x premise -> ghost_ (
    let p = tree_root source in let q = tree_root target in
    Level_mgu_proofs.readback_factor h rho model source ();
    Level_mgu_proofs.readback_factor h rho model target ();
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in
    model x; node_equation_def h rho x; node_equation_def after rho x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q; ())

let (backward @ total) : (h : node Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h (tree_root source) (Level_unifier_spec.redirect h (tree_root source) (tree_root target))) rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root source) && terminal h (tree_root target) && readback source === readback target} ->
    {u : unit | node_equation h rho x} @ ghost = fun h source target rho model x premise -> ghost_ (
    let p = tree_root source in let q = tree_root target in
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in model x; node_equation_def after rho x; node_equation_def h rho x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q;
    if not (x === p) then () else (
      equivalent_avoids h source target (); frame_avoids h p q target ();
      Level_mgu_proofs.readback_factor after rho (refine_ model) target ();
      finite_def h source; tree_root_def source; readback_def source; terminal_def h p;
      let ty = readback source in Level_mgu_spec.substitute_def rho ty;
      match source with Free _ | Constant_tree _ | Word_tree _ | Alias_tree _ -> ()
      | List_tree (_, a) -> let ty = List_type (readback a) in
        weight_def ty; smaller_avoids h source a (); frame_avoids h p q a ();
        Level_mgu_proofs.readback_factor after rho (refine_ model) a (); ()
      | Branch (_, a, b) -> let ta = readback a in let tb = readback b in let ty = Function (ta, tb) in
        weight_def ty; let refine_ pa = Level_unifier_proofs.weight_positive ta in
        let refine_ pb = Level_unifier_proofs.weight_positive tb in
        smaller_avoids h source a (); smaller_avoids h source b ();
        frame_avoids h p q a (); frame_avoids h p q b ();
        Level_mgu_proofs.readback_factor after rho (refine_ model) a ();
        Level_mgu_proofs.readback_factor after rho (refine_ model) b (); ()))
