(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_simple_proofs.ml hmc_heap_machine.ml hmc_heap_machine_proofs.ml hmc_heap_control.ml hmc_heap_allocating.ml hmc_heap_globals.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_runs.ml hmc_heap_extent_math.ml hmc_heap_static.ml hmc_heap_initialize.ml hmc_heap_execute.ml hmc_linear_bytes.ml hmc_linear_bounds.ml hmc_linear_preservation.ml hmc_memory_extent.ml hmc_memory_object.ml hmc_heap_image.ml hmc_heap_code_bounds.ml hmc_memory_suffix.ml hmc_pointer_frame_codec.ml hmc_pointer_frame_shape.ml hmc_memory_block_lookup.ml hmc_memory_saved_frame.ml hmc_memory_stack.ml hmc_memory_stack_capacity.ml";
 readonly_files = "hmc_memory_stack_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
*)
module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module B = Wasm_u32
module Q = Hmc_heap_state
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity;;
[%%expect{|
module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module B = Wasm_u32
module Q = Hmc_heap_state
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
|}]

let zero_capacity (u : unit) = ghost_ (
  let width : W.limb = 16 and top : W.limb = 8 and limit : W.limb = 8 in
  Capacity.fits_def width top limit;
  let proof : {u : unit | Capacity.fits width top limit} = refine_ () in proof);;
[%%expect{|
Line 4, characters 59-69:
4 |   let proof : {u : unit | Capacity.fits width top limit} = refine_ () in proof);;
                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let partial_frame (u : unit) = ghost_ (
  let width : W.limb = 16 and base : W.limb = 8 and top : W.limb = 23 in
  Capacity.region_def width (D.S D.Z) base top;
  Stack.previous_def width top;
  Capacity.region_def width D.Z base (Stack.previous width top);
  let proof : {u : unit | Capacity.region width (D.S D.Z) base top} = refine_ () in proof);;
[%%expect{|
Line 6, characters 70-80:
6 |   let proof : {u : unit | Capacity.region width (D.S D.Z) base top} = refine_ () in proof);;
                                                                          ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_empty_top (u : unit) = ghost_ (
  let width : W.limb = 16 and base : W.limb = 8 and top : W.limb = 24 in
  Stack.related_def G.Empty width B.End base top Q.Halt;
  Stack.pop_correct G.Empty width B.End base top Q.Halt ());;
[%%expect{|
Line 4, characters 56-58:
4 |   Stack.pop_correct G.Empty width B.End base top Q.Halt ());;
                                                            ^^
Error: Refinement could not be proved (counterexample)
|}]

let wrapped_stack (u : unit) = ghost_ (
  let width : W.limb = 16 and base : W.limb = 4294967280 and top : W.limb = 0 in
  Capacity.region_def width (D.S D.Z) base top;
  let proof : {u : unit | Capacity.region width (D.S D.Z) base top} = refine_ () in proof);;
[%%expect{|
Line 4, characters 70-80:
4 |   let proof : {u : unit | Capacity.region width (D.S D.Z) base top} = refine_ () in proof);;
                                                                          ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
