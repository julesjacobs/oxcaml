(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml hm_infer.ml leaf_agreement_proofs.ml hm_origin_proofs.ml hm_complete_proofs.ml hm_one_let_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_polymorphic_proofs.ml hm_polymorphic_fixtures.ml hm_polymorphic_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Hm_execution_spec
module D = Hm_declarative
module T = Hm_type_proofs
module P = Hm_polymorphic_proofs

let run : (e : D.term) @ immutable -> (target : ty) @ immutable ghost ->
    (d : D.typing) @ immutable ghost ->
    {u : unit | D.typed D.Z D.Empty_context e (T.embed target) d} @ ghost -> unit =
  fun e target d premise ->
  ghost_ (let refine_ premise = premise in let z = D.Z in let g = D.Empty_context in
    let t = T.embed target in let u = () in D.depth_def g;
    T.typing_scoped z g e t d (refine_ u); ());
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_infer.closed_hm input in let refine_ input = input in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (let refine_ premise = premise in let u = () in
    P.closed_completes out.#execution after out.#pool target d (refine_ u));
  match out.#value with None ->
    ghost_ (let _impossible : {u : unit | false} = refine_ () in ()); assert false
  | Some p ->
    ghost_ (let refine_ premise = premise in let u = () in
      let refine_ tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      Hm_execution_proofs.run_result h 0 pool env out.#execution after out.#pool p (refine_ u);
      let use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
        {u : unit | true}) @ total = fun _delta _factor -> let u = () in refine_ u in
      P.closed_factor out.#execution after out.#pool p tree target d (refine_ u) true use; ()); ()

let nested (b : ty @ immutable ghost) =
  let z = D.Z in let v = D.Bound z in let rhs = D.Lambda v in
  let app = D.Apply (v, v) in let inner = D.Let (v, app) in let e = D.Let (rhs, inner) in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (T.embed target) d} @ immutable ghost = ghost_ (let a = T.embed b in T.embed_wf z b; T.embed_def target;
    let u = () in let refine_ d = Hm_polymorphic_fixtures.nested_alias_typing a (refine_ u) in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let mixed (b : ty @ immutable ghost) =
  let z = D.Z in let one = D.S z in let v = D.Bound z in
  let outer = D.Bound one in let rhs = D.Lambda outer in
  let truth = D.Truth in let app = D.Apply (v, truth) in
  let body = D.Let (rhs, app) in let e = D.Lambda body in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (T.embed target) d} @ immutable ghost = ghost_ (let refine_ d = Hm_polymorphic_fixtures.mixed_typing b in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let () =
  nested (ghost_ Boolean);
  nested (ghost_ (Function (Boolean, Boolean)));
  mixed (ghost_ Boolean);
  mixed (ghost_ (Function (Boolean, Boolean)))
