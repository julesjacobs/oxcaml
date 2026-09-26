(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_source_safety.ml hmc_specialization.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_simple_proofs.ml hmc_heap_machine.ml hmc_heap_machine_proofs.ml hmc_heap_control.ml hmc_heap_allocating.ml hmc_heap_globals.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_runs.ml hmc_heap_extent_math.ml hmc_heap_static.ml hmc_heap_initialize.ml hmc_heap_execute.ml hmc_heap_source.ml hmc_heap_demand.ml hmc_heap_resource_step.ml hmc_heap_resources.ml hmc_heap_resource_execute.ml hmc_heap_resources_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module W = Hmc_word64
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module V = Hmc_closure_semantics
module X = Hmc_heap_machine
module Q = Hmc_heap_state
module M = Hmc_heap_objects
module R = Hmc_heap_execute
module A = Hmc_heap_resource_execute
module E = Hmc_heap_extent
module Cap = Hmc_frame_capacity
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {W.lo = n; hi = 0}
let build : D.term @ immutable -> I.program @ immutable = fun source ->
  match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p -> I.build (C.build (P.build p))
  | _ -> failwith "resource fixture rejected"
let check source input expected =
  let p = build source in
  let input = {W.lo = input; hi = 0} and fuel = index 1000 in
  let heap_cells = A.heap_cells p input fuel in
  let stack = A.stack_frames p input fuel in
  match E.reserve heap_cells 64 1048576 with
  | None -> failwith "resource plan too large"
  | Some limit ->
    ghost_ (E.ordered heap_cells 64 limit (); E.sufficient heap_cells 64 limit limit (); Cap.reflexive stack);
    (match U.advance p fuel (U.initial p input) with
    | S.Done (V.V.Word value) ->
      let out = A.normal p 64 limit stack input fuel value () in
      (match out with R.Execution (_, Hmc_heap_runs.Finished {X.state = Q.Done (Hmc_tagged_cell.Word actual); heap})
        when actual = value && actual.W.lo = expected && actual.W.hi = 0 && M.used heap = limit -> ()
      | _ -> failwith "sufficient-resource normal return");
      let ordinary = R.execute p 64 limit stack input fuel () in
      if ordinary <> out then failwith "sufficient executor differs from ordinary executor";
      if limit > 64 then (
        let short = R.execute p 64 (limit - 1) stack input fuel () in
        if not (R.exhausted short) then failwith "short heap plan did not exhaust") else failwith "empty static budget";
      (match stack with D.Z -> () | D.S smaller ->
        match R.execute p 64 limit smaller input fuel () with
        | R.Execution (_, Hmc_heap_runs.Blocked (_, X.Stack, _)) -> ()
        | _ -> failwith "short stack plan did not exhaust")
    | _ -> failwith "resource fixture did not return")
let () =
  check (D.Lambda (var 0)) 7 7;
  check (D.Lambda (D.CaseList (D.Cons (var 0, D.Cons (word 2, D.Nil)), word 0, var 0))) 9 9;
  check (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) 9 17;
  check (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Primitive (D.Add, word 1, D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1)))))) 9 26;
  let factory = D.Lambda (D.Lambda (D.Primitive (D.Add, var 1, var 0))) in
  check (D.Let (factory, D.Lambda (D.Apply (D.Apply (var 1, var 0), word 5)))) 7 12;
  print_endline "sufficient heap and stack plans, exact bounds, and normal returns passed"
