(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_source_safety.ml hmc_specialization.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_monomorphic_safety.ml hmc_simulation_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module C = Hmc_monomorphic
module T = Hmc_templates
module M = Hmc_manifest
module H = Hmc_monomorphic_states
module W = Hmc_monomorphic_values
module P = Hmc_monomorphic_simulation
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
module V = Hm_interpreter_typing
let rec index n = if n <= 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {Hmc_word64.lo = n; hi = 0}
let id = D.Lambda (var 0)
let build : (source : D.term) @ immutable ->
    {p : C.program | T.rebuild p.C.source.T.globals p.C.source.T.entry === source} @ immutable = fun source ->
  match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p -> p
  | _ -> raise (Failure "simulation fixture rejected")
let check : D.term @ immutable -> Hmc_word64.limb -> int option -> unit = fun source input expected ->
  let p = build source in
  ghost_ (C.ready_def p);
  let definitions : {d : C.definitions | C.origins d} = refine_ p.C.definitions in
  let input = {Hmc_word64.lo = input; hi = 0} in
  let start = P.initial p definitions input () in
  let source_start = S.advance (P.source_offset p) (S.initial (D.Apply (source, D.Word input))) in
  let target_start = Q.step definitions (Q.initial (C.Apply (p.C.entry, C.Word input))) in
  if H.source start <> source_start || H.target start <> target_start then failwith "initialization mismatch";
  let rec loop : int -> (state : H.state) @ immutable -> S.state @ immutable -> Q.state @ immutable ->
      {u : unit | H.valid (C.manifest definitions) state} -> H.state @ immutable =
    fun left state source_state target_state premise ->
      if H.source state <> source_state || H.target state <> target_state then failwith "prefix mismatch";
      if left = 0 then state else
        let next = Hmc_monomorphic_step.step p definitions state () in
        loop (left - 1) next (S.step source_state) (Q.step definitions target_state) () in
  let out = loop 400 start source_start target_start () in
  let fuel = index 400 in
  let run = P.run p definitions input fuel () in
  if H.source run <> H.source out || H.target run <> H.target out then failwith "iteration mismatch";
  ghost_ (Hmc_monomorphic_safety.safe p definitions input fuel ());
  (match expected, out with
  | Some expected, H.Done (W.Word actual) ->
    if actual.Hmc_word64.lo <> expected || actual.Hmc_word64.hi <> 0 then failwith "wrong result";
    ghost_ (P.normal_return_at_offsets p definitions input actual fuel ());
    let source_result = S.advance (D.add (P.source_offset p) fuel) (P.source_start p input) in
    let target_result = Q.advance definitions (D.S fuel) (P.target_start p input) in
    (match source_result, target_result with
    | S.Done (V.Word a), Q.Done (Q.V.Word b) ->
      ghost_ (P.normal_return_preservation p definitions input a (D.add (P.source_offset p) fuel) ());
      ghost_ (P.normal_return_reflection p definitions input b (D.S fuel) ());
      if not (Hmc_word64.equal a b) then failwith "normal-return mismatch"
    | _ -> failwith "normal result missing")
  | None, H.Running _ -> ()
  | _ -> failwith "unexpected termination")
let () =
  List.iter (fun input ->
    check (D.Lambda (var 0)) input (Some input);
    check (D.Let (id, D.Lambda (D.If (D.Apply (var 1, D.Truth),
      D.Apply (var 1, var 0), word 0)))) input (Some input);
    let wrapper = D.Lambda (D.Apply (var 1, var 0)) in
    check (D.Let (id, D.Let (wrapper, D.Lambda (D.Apply (var 1, var 0))))) input (Some input);
    let capture = D.Lambda (D.Lambda (var 1)) in
    check (D.Let (capture, D.Lambda (D.Apply (D.Apply (var 1, var 0), D.False)))) input (Some input);
    check (D.Let (id, D.Let (capture, D.Lambda (D.Primitive (D.Add,
      D.Apply (D.Apply (var 1, var 0), word 5), D.Apply (var 2, word 7)))))) input (Some (input + 7));
    check (D.Lambda (D.Let (var 0, D.Primitive (D.Add, var 0, var 1)))) input (Some (input * 2));
    check (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
      D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) input (Some 17);
    let map = D.Lambda (D.Recursive (D.CaseList (var 0, D.Nil,
      D.Cons (D.Apply (var 4, var 0), D.Apply (var 3, var 1))))) in
    let sum = D.Recursive (D.CaseList (var 0, word 0,
      D.Primitive (D.Add, var 0, D.Apply (var 3, var 1)))) in
    check (D.Let (map, D.Let (sum, D.Lambda (D.Apply (var 1,
      D.Apply (D.Apply (var 2, D.Lambda (D.Primitive (D.Add, var 0, word 1))),
        D.Cons (var 0, D.Cons (word 2, D.Nil)))))))) input (Some (input + 4));
    let head_id = D.Lambda (D.CaseList (var 0, D.Nil, D.Cons (D.Apply (var 3, var 0), var 1))) in
    check (D.Let (id, D.Let (head_id, D.Lambda (D.CaseList (
      D.Apply (var 1, D.Cons (var 0, D.Nil)), word 0, var 0))))) input (Some input);
    check (D.Lambda (D.If (D.False, word 55, D.If (D.Primitive (D.Unsigned_less, var 0, word 10),
      var 0, word 44)))) input (Some input);
    check (D.Recursive (D.Apply (var 1, var 0))) input None) [0; 1; 9];
  print_endline "source/monomorphic simulation, normal returns, and prefixes passed"
