(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml hm_infer.ml hm_complete_proofs.ml hm_complete_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Hm_execution_spec
open Hm_runtime_spec
module D = Hm_declarative
module T = Hm_type_proofs

let (identity_typing @ total) : (a : ty) @ immutable ->
    {d : D.typing | D.typed D.Z D.Empty_context (D.Lambda (D.Bound D.Z))
      (D.embed (Function (a, a))) d} @ immutable ghost = fun a -> ghost_ (
    let z = D.Z in let empty = D.Empty_context in let at = D.embed a in
    let scheme = D.Forall (z, at) in let env = D.Binding (scheme, empty) in
    let args = D.No_arguments in let variable = D.Variable args in let body = D.Bound z in
    let e = D.Lambda body in let target = Function (a, a) in D.embed_def target;
    let t = D.embed target in let d = D.Abstraction (at, variable) in
    T.embed_wf z a; T.embed_wf z target; D.context_wf_def z empty; D.context_wf_def z env;
    D.scheme_wf_def z scheme; D.add_def z z; D.lookup_def env z;
    D.length_def args; D.arity_def scheme; D.arguments_wf_def z args;
    D.open_scheme_def scheme args; T.open_empty at;
    D.typed_def z env body at variable; D.typed_def z empty e t d; refine_ d)

let run_identity (a : ty @ immutable ghost) =
  let z = D.Z in let body = D.Bound z in let e = D.Lambda body in
  ghost_ (let one = D.S z in D.scoped_term_def z e; D.scoped_term_def one body;
    D.present_def one z; term_let_free_def e; term_let_free_def body);
  let e : {e : D.term | D.scoped_term D.Z e && term_let_free e} = refine_ e in
  let refine_ out = Hm_infer.closed e in let refine_ e = e in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (let target = Function (a, a) in let refine_ d = identity_typing a in let u = () in
    Hm_complete_proofs.closed_completes out.#execution after out.#pool target d (refine_ u));
  match out.#value with None ->
    ghost_ (let _impossible : {u : unit | false} = refine_ () in ()); assert false
  | Some p ->
    ghost_ (let u = () in
      let refine_ tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let empty : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      Hm_execution_proofs.run_result h 0 empty env out.#execution after out.#pool p (refine_ u);
      let target = Function (a, a) in let refine_ d = identity_typing a in
      let use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
        {u : unit | true}) @ total = fun _delta _fit -> let u = () in refine_ u in
      Hm_complete_proofs.closed_factor out.#execution after out.#pool p tree target d (refine_ u) true use; ()); ()

let () =
  run_identity (ghost_ Boolean);
  run_identity (ghost_ (Function (Boolean, Boolean)))
