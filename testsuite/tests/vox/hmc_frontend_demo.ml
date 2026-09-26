(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_frontend_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module F = Hmc_frontend
module T = Hmc_templates
module A = Hmc_admission
module G = Hmc_grounding
module N = Hmc_no_free
module S = Hmc_source_semantics
module W = Hmc_word64

let prepared : (term : D.term) @ immutable ->
    {p : T.program | T.ready p && T.rebuild p.T.globals p.T.entry === term} @ immutable =
  fun term -> match F.prepare term with
  | F.Prepared p -> p
  | _ -> failwith "expected admitted entry"
let rec index n = if n <= 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {W.lo = n; hi = 0}
let budget = index 2000

let run (term : D.term @ immutable) input expected =
  let p = prepared term in
  let rebuilt = T.rebuild p.T.globals p.T.entry in
  let value = D.Apply (rebuilt, word input) in
  match S.observe budget (S.initial value) with
  | S.Finished (Hm_interpreter_typing.Word w) when W.equal w {W.lo = expected; hi = 0} -> ()
  | _ -> failwith "prepared source result"

let () =
  let id = D.Lambda (var 0) in
  run id 37 37;
  let p = prepared id in
  (match p.T.derivation with D.Abstraction (D.Word64, _) -> ()
  | _ -> failwith "identity entry was not instantiated to Word64");
  if not (N.typing p.T.derivation) then failwith "free entry annotation";
  let default = D.Lambda (D.Apply (D.Lambda (var 1), D.Nil)) in
  let p = prepared default in
  (match p.T.derivation with
  | D.Abstraction (D.Word64, D.Application (D.List_type D.Boolean, _, D.Empty_list D.Boolean)) -> ()
  | _ -> failwith "unconstrained free variables must default consistently to Bool");
  run default 17 17;
  let global_id = D.Let (id, D.Lambda (D.Apply (var 1, var 0))) in
  let p = prepared global_id in
  (match p.T.globals with
  | T.Declare ({scheme = D.Forall (D.S D.Z, D.Function (D.Parameter D.Z, D.Parameter D.Z)); _}, T.Empty) -> ()
  | _ -> failwith "outer scheme parameters were lost during grounding");
  run global_id 29 29;
  let two_globals = D.Let (id, D.Let (D.Lambda (D.Apply (var 1, var 0)),
    D.Lambda (D.Apply (var 1, D.Apply (var 2, var 0))))) in
  let p = prepared two_globals in
  ghost_ (T.ready_def p);
  (match T.select p.T.globals D.Z () with
  | Some s when Hm_elaboration_check.index_equal (T.rank s.T.earlier) (D.S D.Z) -> ()
  | _ -> failwith "newer declaration rank");
  (match T.select p.T.globals (D.S D.Z) () with
  | Some s when Hm_elaboration_check.index_equal (T.rank s.T.earlier) D.Z -> ()
  | _ -> failwith "earlier declaration rank");
  (match T.select p.T.globals (D.S (D.S D.Z)) () with
  | None -> () | Some _ -> failwith "dangling declaration lookup");
  run two_globals 41 41;
  let map = D.Lambda (D.Recursive (D.CaseList (var 0, D.Nil,
    D.Cons (D.Apply (var 4, var 0), D.Apply (var 3, var 1))))) in
  let mapped_entry = D.Let (map, D.Lambda (D.CaseList (
    D.Apply (D.Apply (var 1, D.Lambda (D.Primitive (D.Add, var 0, word 1))),
      D.Cons (var 0, D.Nil)), word 0, var 0))) in
  run mapped_entry 41 42;
  let sum = D.Recursive (D.CaseList (var 0, word 0,
    D.Primitive (D.Add, var 0, D.Apply (var 3, var 1)))) in
  run (D.Let (sum, D.Lambda (D.Apply (var 1, D.Cons (var 0, D.Cons (word 2, D.Nil)))))) 40 42;
  run (D.Lambda (D.Let (D.Primitive (D.Add, var 0, word 1), var 0))) 41 42;
  (match F.prepare (D.Apply (D.Truth, D.Truth)) with F.Type_error -> ()
  | _ -> failwith "type error classification");
  (match F.prepare (var 0) with F.Unbound_variable -> ()
  | _ -> failwith "scope error classification");
  (match F.prepare (D.Lambda D.Truth) with F.Entry_type_mismatch -> ()
  | _ -> failwith "entry interface mismatch classification");
  (match F.prepare (D.Lambda (D.If (var 0, word 0, word 1))) with F.Entry_type_mismatch -> ()
  | _ -> failwith "Boolean entry argument admitted as Word64");
  (match F.prepare (D.Lambda (D.Let (id, var 1))) with
  | F.Unsupported_fragment A.Unsupported_polymorphic_local_let -> ()
  | _ -> failwith "local polymorphism must remain a fragment rejection");
  (match F.prepare (D.Let (word 0, id)) with
  | F.Unsupported_fragment A.Non_callable_outer_binding -> ()
  | _ -> failwith "computational outer binding admitted");
  (match F.prepare (D.Apply (D.Lambda id, D.Truth)) with
  | F.Unsupported_fragment A.Non_callable_entry -> ()
  | _ -> failwith "computed entry closure admitted");
  (match F.prepare (D.Let (D.Nil, id)) with
  | F.Unsupported_fragment A.Non_callable_outer_binding -> ()
  | _ -> failwith "non-callable polymorphic global admitted")
