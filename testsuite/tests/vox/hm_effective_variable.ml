open Copy_spec
open Level_unifier_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_effective_environment
module E = Effective_level
module D = Hm_declarative
module T = Hm_type_proofs
module P = Hm_template_instance_proofs

let (variable_typing @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable -> (depth : int) ->
    (env : env) @ immutable -> (schemas : templates) @ immutable -> (i : D.index) @ immutable ->
    (original : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (history : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation (copy_heap h epoch depth history) rho x})) @ total ->
    {u : unit | effective_env h heads depth env schemas && lookup env i === Some original
      && Copy_certificate_spec.certifies h certificate epoch depth history original p} ->
    {d : D.typing | D.typed D.Z (P.context rho schemas) (D.Bound i) (D.embed (rho p)) d} @ immutable ghost =
  fun h heads witness certificate depth env schemas i original p epoch history rho model premise -> ghost_ (
    Copy_certificate_proofs.replay h certificate heads witness epoch depth history original p ();
    let schema = lookup_schema h heads depth env schemas i original () in
    copy_heap_def h epoch depth history; let after = copy_heap h epoch depth history in let raw = heap h epoch depth history in
    let trail = Pooled_spec.touched history in let clean_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x}) @ total = fun x -> model x; node_equation_def after rho x; observe_def after x; equation_def after rho x; () in
    let raw_model : ((x : node Pref.t) @ immutable -> {u : unit | equation raw rho x}) @ total = fun x ->
      clean_model x; Effective_copy_metadata.result_at h heads epoch depth history x ();
      Copy_cleanup_proofs.sweep_model raw after trail rho x (); () in
    let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun x -> Effective_copy_spec.effective_image h heads history rho x in
    let images : ((x : node Pref.t) @ immutable -> {u : unit | choices x === Effective_copy_spec.effective_image h heads history rho x}) @ total =
      fun x -> choices_def x; () in
    Effective_copy_sound.target_image h heads history rho original p (); choices_def original;
    Effective_copy_sound.template_sound h heads epoch depth history rho (refine_ raw_model) choices images schema ();
    let args = P.direct_instance rho choices schema in
    P.lookup_context rho schemas i schema (); P.context_wf rho schemas;
    let ty = rho p in let z = D.Z in T.embed_wf z ty;
    let g = P.context rho schemas in let term = D.Bound i in let target = D.embed ty in
    let d = D.Variable args in D.typed_def z g term target d; d)
