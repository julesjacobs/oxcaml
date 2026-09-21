(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml hm_infer.ml leaf_agreement_proofs.ml hm_origin_proofs.ml hm_complete_proofs.ml hm_one_let_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_polymorphic_proofs.ml hm_polymorphic_fixtures.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_scheme_transport_proofs.ml hm_generalized_scheme_proofs.ml hm_polymorphic_sound_proofs.ml fast_environment_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
module F = Fast_environment
module T = Fast_term
module D = Hm_declarative

let rec inspect count p q (f : {f : F.forest | F.valid_forest f} @ immutable) i =
  if i < 0 then () else (
    let proof : ({u : unit | F.valid_forest f && i >= 0}) Ghost.t =
      {Ghost.ghost = ghost_ (())} in
    let found = F.lookup f i proof in
    (if i = count then assert (found = None) else
      match found with None -> assert false | Some value ->
        let expected = if i mod 2 = 0 then q else p in
        let equal = Pref.equal value expected in assert equal);
    inspect count p q (f) (i - 1))

let rec build remaining count p q
    (f : {f : F.forest | F.valid_forest f} @ immutable) =
  if remaining <= 0 then inspect count p q f count else (
    let next = F.cons p f in
    build (remaining - 1) count q p (next))

let () =
  let state = Pref.empty () in
  let a = cell Var 0 in let b = cell Bool 0 in
  let first = Pref.alloc a state in
  let second = Pref.alloc b first.state in
  let empty = F.Nil in ghost_ (F.valid_forest_def empty);
  build 200000 200000 first.value second.value (empty);
  let zero : {n : int | n >= 0} = 0 in
  let variable = T.bound zero in let term = T.Lambda variable in
  ghost_ (T.valid_def term; T.source_def term;
    T.decode_def zero;
    let z = D.Z in let one = D.S z in
    let body = D.Bound z in let source = D.Lambda body in
    D.scoped_term_def z source; D.scoped_term_def one body;
    D.present_def one z; ());
  let input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)} = term in
  let out = Hm_infer.closed_compiled input in
  assert (out.#value <> None)
