(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_generalization_proofs.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_inference_model.ml hm_inference.mli hm_inference.ml hm_inference_public_client.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module I = Hm_inference
module M = Hm_inference_model

let (identity_typing @ total) :
    unit -> {d : D.typing | D.typed D.Z D.Empty_context (D.Lambda (D.Bound D.Z))
      (D.embed (Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean))) d} @ immutable ghost =
  fun () -> ghost_ (
    let z = D.Z in let a = D.Boolean in let scheme = D.Forall (z, a) in
    let context = D.Binding (scheme, D.Empty_context) in let args = D.No_arguments in
    let body = D.Variable args in let out = D.Abstraction (a, body) in
    D.add_def z z; D.mono_wf_def z a; D.scheme_wf_def z scheme;
    D.context_wf_def z D.Empty_context; D.context_wf_def z context;
    D.arguments_wf_def z args; D.length_def args; D.arity_def scheme;
    D.lookup_def context z; D.open_scheme_def scheme args; D.open_type_def args a;
    D.typed_def z context (D.Bound z) a body;
    D.mono_wf_def z (D.Function (a, a));
    D.typed_def z D.Empty_context (D.Lambda (D.Bound z)) (D.Function (a, a)) out;
    D.embed_def Copy_spec.Boolean; D.embed_def (Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean));
    out)

let () =
  let source = D.Lambda (D.Bound D.Z) in
  ghost_ (D.scoped_term_def D.Z source; D.scoped_term_def (D.S D.Z) (D.Bound D.Z);
    D.present_def (D.S D.Z) D.Z);
  let out = I.infer source in
  ghost_ (
    let target = Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean) in
    let typing = identity_typing () in
    let claim = not (I.inferred_type out === None) in
    let use : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match I.inferred_type out with None -> false
        | Some ty -> target === M.substitute delta ty} ->
      {u : unit | claim}) @ total = fun _delta _instance -> () in
    I.principal out target typing () claim use);
  let ty : {ty : Copy_spec.ty option | not (ty === None)} @ immutable = I.inferred_type out in
  (match ty with None -> failwith "proved typable identity rejected" | Some ty ->
    let _typing = ghost_ (I.sound out ty ()) in ());
  let self = D.Apply (D.Bound D.Z, D.Bound D.Z) in
  let invalid = D.Lambda self in
  ghost_ (D.scoped_term_def D.Z invalid; D.scoped_term_def (D.S D.Z) self;
    D.scoped_term_def (D.S D.Z) (D.Bound D.Z); D.present_def (D.S D.Z) D.Z);
  let rejected = I.infer invalid in
  match I.inferred_type rejected with None -> () | Some _ -> failwith "self-application accepted"

let (rejection_client @ total) : (out : I.result) @ immutable ->
    (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | I.inferred_type out === None &&
      D.typed D.Z D.Empty_context (I.source out) (D.embed target) typing} -> {u : unit | false} @ ghost =
  fun out target typing premise -> ghost_ (I.rejected out target typing premise)

let () =
  let word = D.Word {Hmc_word64.lo = 17; hi = 4294967295} in
  let list = D.Cons (word, D.Nil) in
  ghost_ (D.scoped_term_def D.Z word; D.scoped_term_def D.Z D.Nil; D.scoped_term_def D.Z list);
  let out = I.infer list in
  (match I.inferred_type out with
  | Some (Copy_spec.List_type Copy_spec.Word64 as ty) -> let _typing = ghost_ (I.sound out ty ()) in ()
  | _ -> failwith "word-list inference failed");
  let id = D.Lambda (D.Bound D.Z) in
  let first = D.Apply (D.Bound D.Z, D.Truth) in
  let second = D.Apply (D.Bound (D.S D.Z), word) in
  let body = D.Let (first, second) in
  let source = D.Let (id, body) in
  ghost_ (
    D.scoped_term_def D.Z id; D.scoped_term_def (D.S D.Z) (D.Bound D.Z); D.present_def (D.S D.Z) D.Z;
    D.scoped_term_def (D.S D.Z) D.Truth; D.scoped_term_def (D.S D.Z) first;
    D.scoped_term_def (D.S (D.S D.Z)) word; D.scoped_term_def (D.S (D.S D.Z)) (D.Bound (D.S D.Z));
    D.present_def (D.S (D.S D.Z)) (D.S D.Z); D.scoped_term_def (D.S (D.S D.Z)) second;
    D.scoped_term_def (D.S D.Z) body; D.scoped_term_def D.Z source);
  let out = I.infer source in
  match I.inferred_type out with
  | Some Copy_spec.Word64 -> let _typing = ghost_ (I.sound out Copy_spec.Word64 ()) in ()
  | _ -> failwith "polymorphic boolean-word inference failed"
