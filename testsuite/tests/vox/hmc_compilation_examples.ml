(* TEST
 has-z3;
 timeout = "3600";
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_interpreter_typing.ml wasm_u32.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_memory.ml wasm_memory_execution.ml wasm_control.ml hmc_failed_guard_model.ml hmc_layout.ml hmc_source_semantics.ml wasm_framing.ml wasm_data_section.ml wasm_export_section.ml wasm_control_compose.ml wasm_execution_budget.ml wasm_nesting.ml wasm_control_codec.ml wasm_functions.ml wasm_instruction_stream.ml wasm_local_declarations.ml wasm_function_body.ml wasm_index_vector.ml wasm_code_section.ml wasm_signature_section.ml wasm_function_sections.ml wasm_global_entry.ml wasm_globals.ml wasm_global_section.ml wasm_limits_section.ml wasm_section.ml wasm_module.ml wasm_binary_module.ml wasm_global_execution.ml wasm_control_lift.ml wasm_instance_control.ml wasm_calls.ml wasm_binary_execution.ml hmc_compilation_model.ml wasm_static_types.ml wasm_static_control.ml wasm_static_typing.ml wasm_static_module.ml hmc_compilation.mli hm_checked_elaboration.mli hm_elaboration_check.ml hm_checked_elaboration.ml hm_type_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_ground_annotations.ml hm_abstraction.ml hm_abstraction_proofs.ml hmc_ground_arguments.ml hmc_templates.ml hmc_instance.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_parameter_closed.ml hmc_parameter_typing.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_closure_ir.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_cfg_program.ml hmc_closure_semantics.ml hmc_cfg_semantics.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_tail_sites.ml hmc_tail_continuation.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_execution.ml hmc_frame_reachable.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_extent_math.ml hmc_frame_codec.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_machine.ml hmc_heap_globals.ml hmc_heap_machine_proofs.ml hmc_heap_allocating.ml hmc_heap_simple_proofs.ml hmc_heap_control.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_static.ml hmc_heap_initialize.ml hmc_catalog_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_monomorphic_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hm_annotation_trace.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml level_mgu_spec.ml verified_hm.mli generalize_spec.ml copy_heap_proofs.ml level_proofs.ml marked_occurs_proofs.ml level_unifier_proofs.ml compression_finite_proofs.ml level_unifier_metadata.ml provenance_spec.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml generalize_proofs.ml generalize_scheme_proofs.ml lower_locality_proofs.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_path_proofs.ml compression_spec.ml effective_compression_spec.ml effective_compression_proofs.ml structure_spec.ml terminal_lower_spec.ml effective_unifier_spec.ml hm_environment_spec.ml compression_proofs.ml optimized_unifier_spec.ml level_finite_proofs.ml level_mgu_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml optimized_model_proofs.ml representative_level.ml nested_pool_spec.ml representative_pool_spec.ml effective_level.ml effective_lower_spec.ml forest_transport.ml effective_lower_proofs.ml terminal_lower_proofs.ml effective_unifier_model.ml representative_certificate.ml copy_certificate_spec.ml copy_cleanup_spec.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_primitive_constraints.ml pooled_spec.ml hm_effective_execution_spec.ml hm_annotation_trace_spec.ml hm_annotation_shape.ml copy_certificate_capture.ml effective_copy_spec.ml copy_certificate_proofs.ml copy_cleanup_proofs.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_finite.ml effective_unifier_finite.ml hm_effective_forest.ml forest_heads.ml hm_effective_model.ml hm_annotation_equations.ml effective_unifier_frame.ml optimized_metadata.ml representative_mutation.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_pool.ml pooled_proofs.ml nested_pool_proofs.ml representative_pool_proofs.ml hm_effective_registration.ml hm_effective_membership.ml hm_effective_bound.ml pooled_allocation_proofs.ml hm_effective_paths.ml effective_compression_metadata.ml effective_scan_proofs.ml effective_unifier_order.ml hm_effective_runtime.ml hm_effective_result.ml hm_execution_spec.ml hm_runtime_spec.ml hm_effective_driver_proofs.ml hm_annotation_owned.ml hm_readback_runtime.ml hm_annotation_snapshot.ml hm_effective_complete.mli effective_template.ml hm_effective_allocation.ml hm_effective_closing.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml hm_effective_environment.ml hm_environment_models.ml hm_effective_complete_helpers.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_environment_proofs.ml effective_copy_order.ml effective_copy_pool.ml hm_effective_copy_runtime.ml hm_effective_invariant.ml leaf_provenance_spec.ml compression_origin_proofs.ml effective_compression_origin.ml leaf_provenance_proofs.ml structure_origin_proofs.ml terminal_lower_origin.ml effective_unifier_origin.ml hm_freshness_proofs.ml hm_effective_freshness.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.ml hm_effective_sound.mli hm_template_instance_proofs.ml hm_effective_generalization.ml hm_effective_variable.ml hm_scheme_transport_proofs.ml hm_effective_sound.ml hm_generalization.ml hm_elaboration.ml hm_instantiation.ml hm_generalization_instances.ml hm_generalization_proofs.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_reconstruction_instances.ml hm_elaboration_freshness.ml hm_elaboration_binding.ml hm_elaboration_continuation.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml level_pool_routing_spec.ml pool_closing_equivalence.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_routed_infer.mli certified_copy.mli effective_copy_runtime.mli copy_cleanup.mli copy_cleanup.ml effective_copy_runtime.ml certified_copy.ml effective_allocator.mli effective_allocator.ml effective_hm_unify.mli effective_unifier_runtime.mli effective_bind.mli effective_bind_proofs.ml effective_lower_runtime.mli effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml level_lower.ml effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_bind.ml effective_compressed_representative.mli graph_representative.mli graph_representative.ml effective_compressed_representative.ml effective_link.mli effective_link_proofs.ml structure_link.mli structure_link.ml effective_link.ml effective_unifier_runtime.ml effective_hm_unify.ml hm_pool_capacity.mli hm_pool_capacity.ml level_pool_routing.mli representative_pool.mli representative_pool.ml level_pool_routing.ml hm_routed_infer.ml hm_elaboration_projection.ml hm_typed_elaboration.ml verified_hm.ml hmc_frontend.ml hmc_specialization.ml hmc_u32_index.ml hmc_linear_bounds.ml hmc_linear_preservation.ml hmc_frame_capacity.ml hmc_memory_prefix.ml wasm_cell.ml wasm_word_prefix.ml wasm_word_sequence.ml hmc_wasm_header_words.ml wasm_immediate_write.ml wasm_frame_literals.ml hmc_wasm_literal_load.ml wasm_memory_splice.ml wasm_memory_unique.ml wasm_word_overlay.ml wasm_word_update.ml wasm_sequence_update.ml hmc_wasm_header_update.ml hmc_wasm_reservation.ml hmc_wasm_allocation_guard.ml wasm_control_success.ml hmc_wasm_allocation_select.ml hmc_failed_guard_proof.ml hmc_heap_wire.ml hmc_memory_extent.ml hmc_memory_object.ml hmc_heap_image.ml hmc_heap_image_suffix.ml hmc_heap_code_bounds.ml hmc_memory_block_lookup.ml hmc_memory_suffix.ml hmc_pointer_frame_codec.ml hmc_pointer_frame_shape.ml hmc_memory_saved_frame.ml hmc_memory_stack.ml hmc_runtime_closures.ml hmc_heap_image_prefix.ml hmc_memory_cells.ml hmc_runtime_descriptor.ml hmc_runtime_descriptor_table.ml hmc_frame_segments.ml hmc_wasm_wire_words.ml hmc_wasm_pc_update.ml wasm_parallel_copy.ml hmc_wasm_relayout_geometry.ml hmc_wasm_relayout.ml wasm_cross_copy.ml hmc_wasm_call_save.ml hmc_cell_patch.ml hmc_wire_word_sequence.ml wasm_word_sequence_algebra.ml hmc_wasm_cell_update.ml hmc_wasm_range_copy.ml wasm_cross_words.ml hmc_wasm_cross_range_words.ml wasm_scatter_words.ml wasm_word_read.ml wasm_words_read.ml wasm_scatter_memory.ml wasm_scatter_range.ml hmc_wasm_call_save_memory.ml wasm_cell_suffix.ml hmc_wasm_environment_lookup.ml hmc_wasm_cells_read.ml hmc_cell_slice.ml hmc_wasm_range_read.ml hmc_wasm_frame_restore.ml hmc_wasm_cross_cell_update.ml hmc_wasm_return_result.ml hmc_wasm_stack_retreat.ml hmc_wasm_return_frame.ml hmc_heap_return_transition.ml hmc_wasm_return_frame_source.ml wasm_local_probe.ml hmc_wasm_return_select.ml wasm_control_branch_continue.ml hmc_wasm_caller_return.ml hmc_linear_take_prefix.ml wasm_memory_word_limbs.ml wasm_word_transport.ml hmc_wasm_descriptor_reads.ml wasm_limb_read.ml hmc_wasm_descriptor_load.ml hmc_heap_call_transition.ml hmc_frame_call_entry.ml wasm_memory_lowering.ml wasm_frame_write.ml wasm_pointer_store.ml wasm_mixed_write.ml hmc_wasm_call_header.ml wasm_mixed_words.ml wasm_mixed_memory.ml hmc_wasm_call_header_layout.ml hmc_frame_call_decode.ml hmc_runtime_descriptor_bound.ml hmc_wasm_call_captures.ml hmc_wasm_allocation_exit.ml hmc_wasm_global_lower.ml hmc_wasm_primitive_value.ml hmc_wasm_primitive_payload.ml hmc_wasm_primitive_write.ml hmc_wasm_value_pop.ml hmc_wasm_primitive_lower.ml hmc_wasm_branch.ml hmc_wasm_local_load.ml hmc_wasm_simple_lower.ml hmc_wasm_block_lower.ml hmc_wasm_closure_write.ml hmc_wasm_closure_lower.ml hmc_heap_closure_transition.ml hmc_wasm_allocation_advance.ml hmc_wasm_relayout_finish.ml hmc_wasm_closure_memory.ml wasm_four_words.ml hmc_wasm_cons_write.ml hmc_wasm_cons_stored.ml hmc_wasm_closure_stored.ml hmc_wasm_closure_allocate.ml hmc_frame_header_patch.ml hmc_cell_capacity.ml hmc_frame_slices.ml hmc_wasm_schema_counts.ml hmc_wasm_closure_allocate_source.ml hmc_wasm_closure_result.ml hmc_wasm_closure_result_memory.ml hmc_frame_value_pop.ml hmc_wasm_cons_memory.ml hmc_wasm_cons_allocate.ml hmc_wasm_cons_result.ml hmc_wasm_cons_finish.ml hmc_cell_patch_algebra.ml hmc_frame_relayout_model.ml hmc_frame_relayout_patch.ml hmc_frame_repad.ml hmc_frame_value_pop_source.ml hmc_wasm_cons_result_memory.ml hmc_u32_index_sum.ml hmc_wasm_range_pair.ml hmc_wasm_range_split.ml hmc_wasm_range_geometry.ml hmc_wasm_value_pop_restore.ml hmc_wasm_cons_finish_step.ml hmc_wasm_range_four.ml hmc_wasm_replacement_size.ml hmc_wasm_cons_finish_invariant.ml hmc_wasm_frame_suffix.ml hmc_wasm_cons_finish_heap.ml hmc_wasm_frame_preservation.ml hmc_wasm_frame_transport.ml hmc_wasm_cons_success.ml hmc_wasm_closure_finish.ml hmc_wasm_closure_success.ml wasm_local_unique.ml wasm_frame_snapshot.ml wasm_snapshot_values.ml hmc_wasm_cons_capture.ml hmc_heap_bounds.ml hmc_wasm_list_memory.ml hmc_wasm_list_capture.ml hmc_frame_list_branch.ml hmc_heap_list_branch.ml hmc_wasm_frame_header.ml hmc_wasm_control_step.ml hmc_wasm_list_finish.ml hmc_frame_list_branch_source.ml hmc_wasm_cell_store.ml hmc_wasm_list_store.ml hmc_wasm_list_finish_memory.ml hmc_wasm_list_relayout.ml hmc_wasm_list_relayout_copy.ml hmc_wasm_list_finish_step.ml hmc_wasm_list_finish_invariant.ml hmc_wasm_list_empty.ml hmc_wasm_list_finish_heap.ml hmc_wasm_list_full.ml hmc_wasm_list_full_source.ml wasm_pointer_read.ml hmc_wasm_list_pointer.ml hmc_wasm_list_full_entry.ml wasm_word_probe.ml hmc_wasm_list_probe.ml wasm_control_select.ml hmc_wasm_list_conditional.ml hmc_wasm_list_lower.ml hmc_wasm_structured_block.ml hmc_wasm_structured_table.ml hmc_wasm_dispatch_code.ml hmc_linear_prefix_join.ml hmc_wasm_closure_read.ml wasm_word_shape_split.ml hmc_wasm_call_capture_memory.ml hmc_wasm_dynamic_call_header.ml hmc_wasm_dynamic_call_header_memory.ml hmc_wire_cells_join.ml hmc_wasm_dynamic_call_frame.ml hmc_wasm_call_plan_table.ml wasm_empty_labels.ml wasm_local_select.ml wasm_local_select_resume.ml hmc_wasm_call_plan_select.ml hmc_wasm_dynamic_call_entry.ml hmc_wasm_call_plan_enter.ml wasm_local_double.ml wasm_local_replace_compose.ml hmc_wasm_descriptor_address.ml hmc_wasm_descriptor_source.ml hmc_wasm_descriptor_table_source.ml hmc_wasm_descriptor_select.ml wasm_local_preservation.ml hmc_wasm_descriptor_preserve.ml hmc_wasm_selected_call_entry.ml hmc_wasm_call_dispatch.ml hmc_wasm_cons_read.ml hmc_wasm_cons_operands.ml wasm_pointer_local.ml hmc_wasm_call_operands.ml hmc_wasm_closure_code.ml hmc_wasm_call_target.ml hmc_wasm_call_target_preserve.ml hmc_wasm_loaded_call.ml hmc_frame_call_save.ml hmc_frame_call_slices.ml hmc_memory_stack_capacity.ml hmc_wasm_frame_padding.ml wasm_mixed_range_memory.ml hmc_wasm_padding_memory.ml hmc_wasm_frame_pad_finish.ml hmc_wasm_call_save_padded.ml hmc_wasm_call_save_reads.ml hmc_frame_decode_suffix.ml hmc_wasm_call_save_layout.ml hmc_wasm_call_save_source.ml hmc_wasm_call_save_regions.ml hmc_wasm_saved_frame.ml hmc_wasm_call_save_loaded.ml hmc_wasm_stack_push.ml hmc_wasm_call_save_push.ml hmc_wasm_stack_guard.ml hmc_wasm_call_save_guard.ml hmc_wasm_loaded_call_stack.ml hmc_wasm_ordinary_call.ml hmc_wasm_program_block.ml hmc_wasm_program_table.ml hmc_wasm_program_lower.ml hmc_wasm_root_return.ml hmc_wasm_program_emit.ml hmc_wasm_program_status.ml hmc_failed_guard_reach.ml wasm_global_local_transfer.ml wasm_global_registers.ml wasm_instance_body.ml wasm_register_block.ml wasm_call_policy.ml wasm_shallow_calls.ml wasm_shallow_module.ml hmc_wasm_program_functions.ml wasm_control_branch_target.ml wasm_calls_body.ml wasm_indirect_block.ml hmc_wasm_program_roundtrip.ml hmc_wasm_program_runtime.ml hmc_wasm_program_dispatch.ml hmc_wasm_program_header.ml wasm_local_replace.ml wasm_register_load.ml wasm_register_store.ml hmc_wasm_program_registers.ml wasm_control_local_type.ml hmc_wasm_program_register_export.ml hmc_wasm_program_register_step.ml hmc_failed_guard_calls.ml hmc_heap_demand.ml hmc_heap_runs.ml hmc_wasm_heap_suffix.ml hmc_wasm_program_frame.ml hmc_wasm_program_resources.ml hmc_wasm_program_frame_store.ml hmc_wasm_program_descriptors.ml hmc_wasm_program_state.ml hmc_wasm_program_resource_step.ml hmc_wasm_program_case_shape.ml hmc_wasm_program_selection.ml hmc_wasm_branch_invariant.ml hmc_wasm_program_branch.ml hmc_wasm_program_cost.ml hmc_wasm_program_source_branch.ml hmc_failed_guard_blocks.ml wasm_control_branch_finish.ml hmc_wasm_closure_guarded.ml hmc_wasm_closure_continue.ml hmc_wasm_program_closure.ml hmc_wasm_program_source_closure.ml hmc_heap_cons_transition.ml hmc_wasm_cons_guarded.ml hmc_wasm_cons_continue.ml hmc_wasm_cons_entry.ml hmc_wasm_cons_locals.ml hmc_wasm_program_cons.ml hmc_wasm_program_source_cons.ml hmc_wasm_frame_update.ml hmc_wasm_global_step.ml hmc_wasm_global_invariant.ml hmc_wasm_program_global.ml hmc_wasm_program_source_global.ml hmc_wasm_jump_invariant.ml hmc_wasm_program_jump.ml hmc_wasm_program_source_jump.ml hmc_wasm_list_block_entry.ml hmc_wasm_program_list.ml hmc_wasm_program_source_list.ml hmc_wasm_literal_step.ml hmc_wasm_literal_invariant.ml hmc_wasm_program_literal.ml hmc_wasm_program_source_literal.ml hmc_wasm_environment_read.ml hmc_wasm_local_step.ml hmc_wasm_local_invariant.ml hmc_wasm_program_local.ml hmc_wasm_program_source_local.ml hmc_frame_primitive_model.ml hmc_wasm_primitive_read.ml hmc_frame_primitive_source.ml hmc_wasm_primitive_restore.ml hmc_wasm_primitive_update.ml hmc_wasm_primitive_step.ml hmc_wasm_primitive_invariant.ml hmc_wasm_program_primitive.ml hmc_wasm_program_source_primitive.ml hmc_frame_relayout_source.ml hmc_wasm_relayout_save.ml hmc_wasm_relayout_save_step.ml hmc_wasm_relayout_save_invariant.ml hmc_wasm_program_save_environment.ml hmc_wasm_program_source_save_environment.ml hmc_wasm_relayout_restore.ml hmc_wasm_relayout_value.ml hmc_wasm_relayout_saved_step.ml hmc_wasm_relayout_saved_invariant.ml hmc_wasm_program_saved_environment.ml hmc_wasm_program_source_saved_environment.ml hmc_heap_operand_shapes.ml hmc_cfg_execution.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hm_interpreter_proofs.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_simulation.ml hmc_cfg_start.ml hmc_cfg_simulation.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_heap_reachable_operands.ml hmc_wasm_program_branch_ready.ml hmc_wasm_program_step_branch.ml hmc_wasm_program_call_finish.ml hmc_wasm_program_call.ml hmc_wasm_program_call_ready.ml hmc_wasm_program_edges.ml hmc_wasm_program_frame_facts.ml wasm_control_local_preservation.ml hmc_wasm_call_locals.ml hmc_frame_decode_unique.ml hmc_wasm_frame_extend.ml hmc_wasm_program_source_call.ml hmc_wasm_program_caller.ml hmc_wasm_program_source_caller.ml hmc_wasm_program_root.ml hmc_wasm_program_source_root.ml hmc_wasm_program_step_root.ml hmc_wasm_saved_frame_read.ml hmc_wasm_program_step_caller.ml hmc_wasm_program_step_call.ml hmc_wasm_program_step_closure.ml hmc_wasm_program_step_cons.ml hmc_wasm_program_step_global.ml hmc_wasm_program_step_jump.ml hmc_wasm_program_step_list.ml hmc_wasm_program_step_literal.ml hmc_wasm_program_local_ready.ml hmc_wasm_program_step_local.ml hmc_wasm_program_step_primitive.ml hmc_wasm_program_step_result.ml hmc_wasm_program_step_save_environment.ml hmc_wasm_program_step_saved_environment.ml hmc_wasm_program_tail.ml hmc_wasm_program_source_tail.ml hmc_wasm_program_step_tail.ml hmc_wasm_program_step.ml hmc_wasm_program_execution.ml hmc_wasm_program_memory_initialize.ml hmc_wasm_program_initialize.ml hmc_heap_execute.ml hmc_heap_resource_step.ml hmc_heap_resources.ml hmc_wasm_program_source_execution.ml hmc_wasm_program_state_entry.ml wasm_calls_prefix.ml hmc_wasm_program_observe.ml hmc_wasm_program_run.ml hmc_wasm_program_binary.ml wasm_static_sequence.ml wasm_static_steps.ml hmc_wasm_static_allocation.ml wasm_static_copy.ml wasm_static_memory.ml hmc_wasm_static_simple.ml hmc_wasm_static_objects.ml hmc_wasm_static_cons.ml hmc_wasm_static_list.ml hmc_wasm_static_call_data.ml hmc_wasm_static_return.ml hmc_wasm_static_call.ml hmc_wasm_static_block.ml hmc_wasm_static_structured.ml hmc_wasm_static_emit.ml wasm_static_registers.ml hmc_wasm_static_config.ml hmc_wasm_static_functions.ml hmc_wasm_static_runtime.ml hmc_wasm_program_static.ml hmc_compiler.ml hmc_heap_bound.ml hmc_resource_numbers.ml hmc_compilation.ml hmc_compilation_examples.ml";
 { native; }
*)

(* Example programs compiled by the public [Hmc_compilation.compile] and run
   in the WebAssembly model. These are checked runs, not theorems: each run's
   result word is compared with the source interpreter's, and the output
   records the steps taken and the heap and stack the run used against the
   configured limits. *)
module B = Wasm_u32
module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_compilation_model
module C = Hmc_compilation
module Registers = Hmc_wasm_program_registers

(* Source programs with names; [term] converts them to de Bruijn indices. *)
type expr =
  | Var of string | Int of int | True | False | Nil
  | Fun of string * expr
  | Rec of string * string * expr  (* [Rec (f, x, body)]: [f] is the function itself *)
  | App of expr * expr
  | Let of string * expr * expr
  | Cons of expr * expr
  | Case of expr * expr * string * string * expr  (* [Case (l, nil, h, t, cons)] *)
  | If of expr * expr * expr
  | Add of expr * expr | Sub of expr * expr | Eq of expr * expr | Less of expr * expr

let word n = if n < 0 || n >= 4294967296 then failwith "word" else {W.lo = n; hi = 0}
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let rec position name = function
  | [] -> failwith ("unbound " ^ name)
  | head :: rest -> if head = name then 0 else 1 + position name rest
let rec term scope = function
  | Var x -> D.Bound (index (position x scope))
  | Int n -> D.Word (word n)
  | True -> D.Truth | False -> D.False | Nil -> D.Nil
  | Fun (x, body) -> D.Lambda (term (x :: scope) body)
  | Rec (f, x, body) -> D.Recursive (term (x :: f :: scope) body)
  | App (f, a) -> D.Apply (term scope f, term scope a)
  | Let (x, rhs, body) -> D.Let (term scope rhs, term (x :: scope) body)
  | Cons (h, t) -> D.Cons (term scope h, term scope t)
  | Case (l, nil, h, t, cons) -> D.CaseList (term scope l, term scope nil, term (h :: t :: scope) cons)
  | If (c, a, b) -> D.If (term scope c, term scope a, term scope b)
  | Add (a, b) -> D.Primitive (D.Add, term scope a, term scope b)
  | Sub (a, b) -> D.Primitive (D.Subtract, term scope a, term scope b)
  | Eq (a, b) -> D.Primitive (D.Equal_word, term scope a, term scope b)
  | Less (a, b) -> D.Primitive (D.Unsigned_less, term scope a, term scope b)

(* [program [(f, e); ...] entry] is [let f = e in ... in entry]. *)
let program definitions entry =
  List.fold_right (fun (name, rhs) body -> Let (name, rhs, body)) definitions entry
let apply f args = List.fold_left (fun f a -> App (f, a)) (Var f) args

let rec zeros n = if n = 0 then B.End else B.Byte (0, zeros (n - 1))
let rec count = function D.Z -> 0 | D.S n -> 1 + count n

(* The source interpreter's result and step count. *)
let source_result limit source input =
  let rec go steps state =
    if steps > limit then failwith "source step limit" else
    match state with
    | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) -> word, steps
    | Hmc_source_semantics.Running _ -> go (steps + 1) (Hmc_source_semantics.step state)
    | _ -> failwith "source did not return a word" in
  go 0 (Hmc_source_semantics.initial (D.Apply (source, D.Word input)))

(* Only [compile] and [execute] call the compiler and run its output.

   [compile] compiles [source] for [input] with the public interface. The
   layout: the closure table below [frame_base], the current frame from
   [frame_base] to 4 KiB, [frames] saved frames from 4 KiB, and the heap from
   [heap_base] to the end of [pages] pages of zeroed memory. *)
let compile ~pages ~frame_base ~frames ~heap_base name source input =
  if pages < 1 || pages > 16 || frame_base < 0 || frame_base > 4096 || heap_base < 4096
    || heap_base > 65536 * pages then failwith "layout" else
  let layout = {M.table_base = 0; frame_base; stack_base = 4096;
    heap_base; heap_limit = 65536 * pages; max_pc = 10000;
    stack_capacity = index frames; host_capacity = Wasm_code.Zero} in
  let memory = zeros (65536 * pages) in
  match Hmc_linear_bytes.drop memory layout.M.heap_limit with
  | None -> failwith "memory bounds"
  | Some _ ->
    ghost_ (Hmc_linear_bounds.covers_def memory layout.M.heap_limit; M.valid_layout_def layout memory);
    match C.compile source layout input memory pages () with
    | C.Rejected _ -> failwith (name ^ ": rejected")
    | C.Compiled artifact -> layout, C.bytes artifact

type run = {status : int; tag : W.t; payload : W.t; steps : int; heap_used : int; stack_used : int;
  width : int; memory : B.bytes}

(* [execute] runs an emitted module in the model, recording the deepest
   stack top and the final heap pointer. Each block of the program is a
   WebAssembly function that loads the registers from the globals on entry
   and stores them back on exit, so the globals show the stack top between
   blocks. *)
let execute limit (layout : M.layout) bytes =
  let image = match Wasm_binary_module.decode bytes with
    | Some (image, B.End) -> image | _ -> failwith "decode" in
  let module_ = image.Wasm_binary_module.module_ in
  let top_of (configuration : Wasm_calls.configuration) =
    match Registers.read configuration.Wasm_calls.current.Wasm_instance_control.globals with
    | Some registers -> registers.Registers.top | None -> 0 in
  let rec go steps top configuration =
    if steps > limit then failwith "target step limit" else
    match Wasm_calls.step module_ configuration with
    | Wasm_calls.Running next -> go (steps + 1) (max top (top_of next)) next
    | Wasm_calls.Finished after -> after, steps + 1, top
    | _ -> failwith "target execution failed" in
  let after, steps, top =
    match Wasm_calls.start module_ image.Wasm_binary_module.exports.Wasm_export_section.run
        image.Wasm_binary_module.data image.Wasm_binary_module.globals
        (Wasm_code.Succ layout.M.host_capacity) with
    | Wasm_calls.Running configuration -> go 0 layout.M.stack_base configuration
    | _ -> failwith "start" in
  let registers = match Registers.read after.Wasm_global_execution.globals with
    | Some registers -> registers | None -> failwith "registers" in
  (* [run] returns the status, which the globals also hold. *)
  (match after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack with
   | Wasm_scalar.Push (Wasm_scalar.I32 status, Wasm_scalar.Empty) when status = registers.Registers.status -> ()
   | _ -> failwith "run did not return the status");
  let frames = count layout.M.stack_capacity in
  let width = if frames = 0 then 0 else (registers.Registers.stack_limit - layout.M.stack_base) / frames in
  {status = registers.Registers.status; tag = registers.Registers.tag; payload = registers.Registers.payload; steps;
    heap_used = registers.Registers.heap - layout.M.heap_base;
    stack_used = (if width = 0 then 0 else (max top layout.M.stack_base - layout.M.stack_base) / width);
    width; memory = after.Wasm_global_execution.execution.Wasm_memory_execution.memory}

let status_name = function 2 -> "out of heap" | 3 -> "out of stack" | _ -> "unknown status"

let rec write channel = function B.End -> () | B.Byte (byte, rest) -> output_byte channel byte; write channel rest
(* With HMC_EXAMPLES_DIR set, write each module and its expected result for
   verification/catalogue/compiler-example/run-examples-node.js: the bytes,
   the final memory and the status and result. *)
let output_directory = Sys.getenv_opt "HMC_EXAMPLES_DIR"
let expected_channel = Option.map (fun dir -> open_out (Filename.concat dir "expected.txt")) output_directory

(* Compile [source], run it on [input] and compare the result with the
   source machine's. *)
let example ?(pages = 2) ?(frame_base = 2048) ?(frames = 128) ?(heap_base = 65536) ?(expect = `Returns)
    name source input =
  let source = term [] source in
  let input = word input in
  let expected, source_steps = source_result 10_000_000 source input in
  let layout, bytes = compile ~pages ~frame_base ~frames ~heap_base name source input in
  let run = execute 100_000_000 layout bytes in
  let agrees = match expect with
    | `Returns -> run.status = 1 && run.tag = {W.lo = 1; hi = 0} && run.payload = expected
    | `Exhausts status -> run.status = status in
  if not agrees then failwith (Printf.sprintf "%s: the model run ends with status %d, not the source's result"
    name run.status);
  (* [sufficient]: whether the premise of [normal] holds for this number of
     source steps. *)
  Printf.printf "%s (input %d): %s; %d source steps, %d WebAssembly steps; \
    heap %d of %d bytes; stack %d of %d frames (%d bytes each); sufficient: %s\n"
    name input.W.lo (if run.status = 1 then Printf.sprintf "returned %d" run.payload.W.lo
      else status_name run.status ^ Printf.sprintf " (the source returns %d)" expected.W.lo)
    source_steps run.steps
    run.heap_used (layout.M.heap_limit - layout.M.heap_base) run.stack_used (count layout.M.stack_capacity)
    run.width (if M.sufficient layout bytes (index source_steps) then "yes" else "no");
  match output_directory, expected_channel with
  | Some dir, Some channel ->
    let file = String.concat "-" (String.split_on_char ' ' (String.concat "" (String.split_on_char ',' name)))
      ^ Printf.sprintf "-%d" input.W.lo in
    let output = open_out_bin (Filename.concat dir (file ^ ".wasm")) in
    write output bytes; close_out output;
    let output = open_out_bin (Filename.concat dir (file ^ ".memory")) in
    write output run.memory; close_out output;
    Printf.fprintf channel "%s %d %d %d\n" file run.status run.payload.W.lo run.payload.W.hi
  | _ -> ()

(* Library functions. *)
let id = Fun ("x", Var "x")
let map = Fun ("f", Rec ("map", "l",
  Case (Var "l", Nil, "h", "t", Cons (App (Var "f", Var "h"), App (Var "map", Var "t")))))
let sum = Rec ("sum", "l", Case (Var "l", Int 0, "h", "t", Add (Var "h", App (Var "sum", Var "t"))))
let length = Rec ("length", "l", Case (Var "l", Int 0, "h", "t", Add (Int 1, App (Var "length", Var "t"))))
let upto = Rec ("upto", "n", If (Eq (Var "n", Int 0), Nil, Cons (Var "n", App (Var "upto", Sub (Var "n", Int 1)))))
let add = Fun ("a", Fun ("b", Add (Var "a", Var "b")))
let twice = Fun ("f", Fun ("x", App (Var "f", App (Var "f", Var "x"))))
let compose = Fun ("f", Fun ("g", Fun ("x", App (Var "f", App (Var "g", Var "x")))))

let () =
  (* The design example: polymorphic [id] and [map]. *)
  let first = Fun ("l", Case (Var "l", False, "h", "t", Var "h")) in
  let design = program ["id", id; "map", map; "add", add; "sum", sum; "first", first]
    (Fun ("n", Let ("seed", App (Var "id", Var "n"),
      Let ("ys", apply "map" [App (Var "add", Int 3); Cons (Var "seed", Cons (Int 2, Nil))],
      Let ("bs", App (Var "id", apply "map" [Fun ("x", Less (Var "x", Int 10)); Var "ys"]),
      If (App (Var "first", Var "bs"), App (Var "sum", Var "ys"), Int 0)))))) in
  example "design" design 4;
  example "design" design 8;
  (* The identity. With the current frame region cut to 128 bytes, the
     premise of [normal] holds, so its return is proved as well as run. *)
  example "identity" (Fun ("x", Var "x")) 42;
  example ~frame_base:3968 "identity, small frame region" (Fun ("x", Var "x")) 42;
  (* Non-tail recursion. *)
  let fib = Rec ("fib", "n", If (Less (Var "n", Int 2), Var "n",
    Add (App (Var "fib", Sub (Var "n", Int 1)), App (Var "fib", Sub (Var "n", Int 2))))) in
  example "fibonacci" (program ["fib", fib] (Fun ("n", App (Var "fib", Var "n")))) 15;
  (* A list built and summed. *)
  example "sum-list" (program ["upto", upto; "sum", sum] (Fun ("n", App (Var "sum", App (Var "upto", Var "n"))))) 100;
  (* A tail loop: the call of [count] to itself is a jump, so the run uses
     one frame and allocates nothing. *)
  let counter = Rec ("count", "n", If (Less (Var "n", Int 3000), App (Var "count", Add (Var "n", Int 1)), Var "n")) in
  example "count" (program ["count", counter] (Fun ("n", App (Var "count", Var "n")))) 0;
  (* A curried loop with an accumulator. Only a recursive function's call to
     itself in tail position is compiled as a jump: here [loop acc] returns a
     closure and the call of that closure keeps its frame, so the stack grows
     by one frame per iteration, and each closure stays on the heap. *)
  let loop = Rec ("loop", "acc", Fun ("n",
    If (Eq (Var "n", Int 0), Var "acc", apply "loop" [Add (Var "acc", Var "n"); Sub (Var "n", Int 1)]))) in
  example "accumulate" (program ["loop", loop] (Fun ("n", apply "loop" [Int 0; Var "n"]))) 100;
  (* Higher-order functions and closures. *)
  example "closures" (program ["add", add; "twice", twice; "compose", compose]
    (Fun ("n", apply "compose" [App (Var "twice", App (Var "add", Var "n")); Fun ("x", Add (Var "x", Var "x")); Int 10]))) 7;
  (* Let-polymorphism: [twice], [map] and [length] are each used at two types. *)
  example "polymorphism" (program ["twice", twice; "map", map; "length", length; "sum", sum]
    (Fun ("n", Let ("xs", apply "twice" [App (Var "map", Fun ("x", Add (Var "x", Var "n"))); Cons (Int 1, Cons (Int 2, Nil))],
      Let ("bs", apply "twice" [Fun ("l", Cons (Less (Var "n", Int 5), Var "l"));
          apply "map" [Fun ("x", Less (Var "x", Int 10)); Var "xs"]],
      Add (App (Var "sum", Var "xs"), Add (App (Var "length", Var "xs"), App (Var "length", Var "bs")))))))) 3;
  (* [sum-list] with too little memory stops with a report of exhaustion. *)
  example ~expect:(`Exhausts 3) ~frames:8 "sum-list, 8 frames" (program ["upto", upto; "sum", sum]
    (Fun ("n", App (Var "sum", App (Var "upto", Var "n"))))) 100;
  example ~expect:(`Exhausts 2) ~heap_base:129024 "sum-list, 2 KiB heap" (program ["upto", upto; "sum", sum]
    (Fun ("n", App (Var "sum", App (Var "upto", Var "n"))))) 100;
  Option.iter close_out expected_channel
