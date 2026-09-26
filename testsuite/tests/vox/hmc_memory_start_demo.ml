(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_source_safety.ml hmc_specialization.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_simple_proofs.ml hmc_heap_machine.ml hmc_heap_machine_proofs.ml hmc_heap_control.ml hmc_heap_allocating.ml hmc_heap_globals.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_runs.ml hmc_heap_extent_math.ml hmc_heap_static.ml hmc_heap_initialize.ml hmc_heap_execute.ml hmc_heap_source.ml hmc_linear_bytes.ml hmc_linear_bounds.ml hmc_linear_preservation.ml hmc_memory_extent.ml hmc_memory_object.ml hmc_heap_image.ml hmc_heap_code_bounds.ml hmc_linear_create.ml hmc_heap_image_extend.ml hmc_memory_prefix.ml hmc_memory_header.ml hmc_memory_lookup.ml hmc_memory_operations.ml hmc_memory_invoke.ml hmc_memory_read_step.ml hmc_memory_suffix.ml hmc_memory_allocate.ml hmc_memory_machine.ml hmc_memory_runs.ml hmc_memory_cells.ml hmc_pointer_frame_codec.ml hmc_pointer_frame_shape.ml hmc_memory_block_lookup.ml hmc_memory_saved_frame.ml hmc_memory_stack.ml hmc_memory_stack_capacity.ml hmc_memory_call_shape.ml hmc_memory_stack_machine.ml hmc_memory_stack_runs.ml hmc_memory_stack_source.ml hmc_memory_current_shape.ml hmc_memory_active_machine.ml hmc_memory_active_runs.ml hmc_memory_active_source.ml hmc_heap_demand.ml hmc_heap_resource_step.ml hmc_heap_resources.ml hmc_memory_active_resources.ml hmc_memory_static.ml hmc_memory_initialize.ml hmc_memory_start.ml hmc_memory_layout.ml hmc_memory_start_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module W = Hmc_word64
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module N = Hmc_monomorphic
module X = Hmc_heap_machine
module U = Hmc_tail_semantics
module Index = Hmc_u32_index
module Start = Hmc_memory_start
module Layout = Hmc_memory_layout
module Runs = Hmc_memory_active_runs
module Source = Hmc_memory_active_source
module Machine = Hmc_memory_active_machine
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {W.lo = n; hi = 0}
let build : D.term @ immutable -> I.program @ immutable = fun source ->
  match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p -> I.build (C.build (P.build p))
  | _ -> failwith "direct initialization fixture rejected"
type outcome = Normal of int | Initialization_full | Heap_full | Stack_full
let check source input frames (heap_limit : W.limb) expected =
  let program = build source in
  let size = index 4096 in
  match Index.encode 4096 size with
  | None -> failwith "memory size"
  | Some memory_limit ->
    let before = Hmc_linear_create.zeroed size memory_limit () in
    match Layout.make program 4294967295 64 heap_limit heap_limit frames memory_limit with
    | Layout.Rejected _ -> failwith "direct initialization layout"
    | Layout.Layout layout ->
      let input = {W.lo = input; hi = 0} in
      let initial = Start.initialize program layout before input () in
      ghost_ (Start.valid_def program layout; Start.correct_def program layout before input initial);
      let actual = match initial with
      | Start.Heap_exhausted _ -> Initialization_full
      | Start.Initialized start ->
        ghost_ (Hmc_heap_initialize.correct_def program layout.Start.heap_base layout.Start.heap_limit input
          (Hmc_heap_initialize.Initialized {Hmc_heap_initialize.globals = start.Start.globals; configuration = start.Start.model}));
        let fuel = index 5000 in
        let out = Runs.run program start.Start.globals layout.Start.code_capacity layout.Start.width layout.Start.heap_limit layout.Start.stack_base layout.Start.stack_limit
          layout.Start.active layout.Start.active_end layout.Start.memory_limit fuel start.Start.configuration (ghost_ start.Start.model)
          (ghost_ (U.initial program input)) frames () in
        ghost_ (N.ready_def program.I.origin.C.origin.P.origin;
          Source.safe program program.I.origin.C.origin.P.origin.N.definitions start.Start.globals layout.Start.width layout.Start.stack_base
            layout.Start.heap_limit layout.Start.stack_limit layout.Start.active layout.Start.memory_limit frames input fuel start.Start.model
            start.Start.configuration.Machine.memory out ());
        (match out with
        | Runs.Blocked (_, X.Heap, _) -> Heap_full
        | Runs.Blocked (_, X.Stack, _) -> Stack_full
        | Runs.Finished final ->
          match final.Machine.status with
          | Machine.Done (Hmc_tagged_cell.Word word) ->
            ghost_ (Source.returned_def out word);
            let source_steps = Source.reflection program program.I.origin.C.origin.P.origin.N.definitions start.Start.globals
              layout.Start.width layout.Start.stack_base layout.Start.heap_limit layout.Start.stack_limit layout.Start.active layout.Start.memory_limit
              frames input fuel (ghost_ start.Start.model) (ghost_ start.Start.configuration.Machine.memory) out word () in
            if Hmc_source_semantics.advance source_steps (Hmc_monomorphic_simulation.source_start program.I.origin.C.origin.P.origin input)
              <> Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) then failwith "direct initialization source reflection";
            if word.W.hi <> 0 then failwith "high result";
            Normal word.W.lo
          | _ -> failwith "direct initialization terminal state")
      in
      if actual <> expected then failwith "direct initialization outcome";
      (match Layout.make program 0 64 heap_limit heap_limit frames memory_limit with
      | Layout.Rejected Layout.Code_capacity -> () | _ -> failwith "unchecked code capacity")
let () =
  let identity = D.Lambda (var 0) in
  check identity 11 D.Z 64 Initialization_full;
  check identity 11 D.Z 79 Initialization_full;
  check identity 11 D.Z 80 (Normal 11);
  let list = D.Lambda (D.CaseList (D.Cons (var 0, D.Cons (word 2, D.Nil)), word 0, var 0)) in
  check list 9 D.Z 1024 (Normal 9);
  check list 9 D.Z 80 Heap_full;
  let closure_list = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Primitive (D.Add, var 1, var 0)), D.Nil), word 0, D.Apply (var 0, word 2))) in
  check closure_list 7 (index 4) 1024 (Normal 9);
  check closure_list 7 D.Z 1024 Stack_full;
  let factory = D.Lambda (D.Lambda (D.Primitive (D.Add, var 1, var 0))) in
  let globals = D.Let (factory, D.Lambda (D.Let (D.Apply (var 1, var 0),
    D.Let (D.Apply (var 2, word 2), D.Primitive (D.Add, D.Apply (var 1, word 5), D.Apply (var 0, word 7)))))) in
  check globals 10 (index 4) 95 Initialization_full;
  check globals 10 (index 4) 1024 (Normal 24);
  check (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) 100 D.Z 1024 (Normal 17);
  print_endline "direct byte initialization: globals, entry, exact capacity, source reflection, and execution exhaustion passed"
