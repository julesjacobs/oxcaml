open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Optimized_unifier_spec

let (equal_roots @ total) : (before : Pref.heap) @ immutable ->
    (old_p : node Pref.t) @ immutable -> (old_q : node Pref.t) @ immutable ->
    (h : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (rp : resolution) @ immutable -> (rq : resolution) @ immutable ->
    (source : tree) @ immutable -> (target : tree) @ immutable ->
    {u : unit | unified before old_p old_q true h d && resolves h old_p p rp && resolves h old_q q rq
      && finite h source && finite h target && tree_root source === p && tree_root target === q} ->
    {u : unit | readback source === readback target} @ ghost =
  fun before old_p old_q h d trees p q rp rq source target premise -> ghost_ (
    let refine_ premise = premise in
    let[@def] sigma : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let refine_ t = trees x in if H.mem h x then readback t else Variable x in
    let normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in Level_mgu_spec.normalizes h sigma x t}) @ total = fun x ->
      sigma_def x; let refine_ t = trees x in Level_mgu_spec.normalizes_def h sigma x t; let u = () in refine_ u in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h sigma x}) @ total = fun x ->
      let refine_ u = Level_mgu_proofs.normal_model_at h trees sigma normal x in refine_ u in
    let u = () in Optimized_model_proofs.success_forward_at before sigma old_p old_q h d model old_p (refine_ u);
    Level_unifier_proofs.resolution_model h sigma model old_p p rp (refine_ u);
    Level_unifier_proofs.resolution_model h sigma model old_q q rq (refine_ u);
    finite_def h source; finite_def h target; sigma_def p; sigma_def q;
    let refine_ a = trees p in let refine_ b = trees q in
    Level_finite_proofs.finite_unique h a source (refine_ u);
    Level_finite_proofs.finite_unique h b target (refine_ u); refine_ u)
