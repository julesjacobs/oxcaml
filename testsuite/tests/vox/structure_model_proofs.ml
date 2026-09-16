open Copy_spec
open Level_unifier_spec
open Level_finite_spec
open Structure_finite_proofs

let (forward @ total) : (h : Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && readback source === readback target} ->
    {u : unit | node_equation (H.put h (tree_root source) (Level_unifier_spec.redirect h (tree_root source) (tree_root target))) rho x} @ ghost =
  fun h source target rho model x premise -> ghost_ (
    let refine_ premise = premise in let p = tree_root source in let q = tree_root target in
    let u = () in Level_mgu_proofs.readback_factor h rho model source (refine_ u);
    Level_mgu_proofs.readback_factor h rho model target (refine_ u);
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in
    model x; node_equation_def h rho x; node_equation_def after rho x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q; refine_ u)

let (backward @ total) : (h : Pref.heap) @ immutable -> (source : tree) @ immutable ->
    (target : tree) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h (tree_root source) (Level_unifier_spec.redirect h (tree_root source) (tree_root target))) rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | finite h source && finite h target && not (tree_root source === tree_root target)
      && terminal h (tree_root source) && terminal h (tree_root target) && readback source === readback target} ->
    {u : unit | node_equation h rho x} @ ghost = fun h source target rho model x premise -> ghost_ (
    let refine_ premise = premise in let p = tree_root source in let q = tree_root target in
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in let u = () in
    model x; node_equation_def after rho x; node_equation_def h rho x;
    Level_unifier_proofs.observe_write h p v x; Level_unifier_proofs.redirect_desc h p q;
    if not (x === p) then refine_ u else (
      equivalent_avoids h source target (refine_ u); frame_avoids h p q target (refine_ u);
      Level_mgu_proofs.readback_factor after rho (refine_ model) target (refine_ u);
      finite_def h source; tree_root_def source; readback_def source; terminal_def h p;
      let ty = readback source in Level_mgu_spec.substitute_def rho ty;
      match source with Free _ | Constant_tree _ | Alias_tree _ -> refine_ u
      | Branch (_, a, b) -> let ta = readback a in let tb = readback b in let ty = Function (ta, tb) in
        weight_def ty; let refine_ pa = Level_unifier_proofs.weight_positive ta in
        let refine_ pb = Level_unifier_proofs.weight_positive tb in
        smaller_avoids h source a (refine_ u); smaller_avoids h source b (refine_ u);
        frame_avoids h p q a (refine_ u); frame_avoids h p q b (refine_ u);
        Level_mgu_proofs.readback_factor after rho (refine_ model) a (refine_ u);
        Level_mgu_proofs.readback_factor after rho (refine_ model) b (refine_ u); refine_ u))
