open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec

let (equal_roots @ total) : (before : node Pref.heap) @ immutable ->
    (old_p : node Pref.t) @ immutable -> (old_q : node Pref.t) @ immutable ->
    (h : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (rp : resolution) @ immutable -> (rq : resolution) @ immutable ->
    (source : tree) @ immutable -> (target : tree) @ immutable ->
    {u : unit | unified before old_p old_q true h d && resolves h old_p p rp && resolves h old_q q rq
      && finite h source && finite h target && tree_root source === p && tree_root target === q} ->
    {u : unit | readback source === readback target} @ ghost =
  fun before old_p old_q h d trees p q rp rq source target premise -> ghost_ (
    let[@def] sigma : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let t = trees x in if H.mem h x then readback t else Variable x in
    let normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in Level_mgu_spec.normalizes h sigma x t}) @ total = fun x ->
      sigma_def x; let t = trees x in Level_mgu_spec.normalizes_def h sigma x t; () in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h sigma x}) @ total = fun x ->
      let () = Level_mgu_proofs.normal_model_at h trees sigma normal x in () in
    Effective_unifier_model.success_forward_at before sigma old_p old_q h d model old_p ();
    Level_unifier_proofs.resolution_model h sigma model old_p p rp ();
    Level_unifier_proofs.resolution_model h sigma model old_q q rq ();
    finite_def h source; finite_def h target; sigma_def p; sigma_def q;
    let a = trees p in let b = trees q in
    Level_finite_proofs.finite_unique h a source ();
    Level_finite_proofs.finite_unique h b target (); ())
