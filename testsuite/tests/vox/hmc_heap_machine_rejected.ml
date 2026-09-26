(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml hmc_slot_copy.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_simple_proofs.ml hmc_heap_machine.ml hmc_heap_machine_proofs.ml hmc_heap_control.ml hmc_heap_allocating.ml hmc_heap_globals.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_runs.ml";
 readonly_files = "hmc_heap_machine_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module B = Hmc_heap_simple
module G = Hmc_cfg_ir;;
[%%expect{|
module D = Hm_declarative
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module B = Hmc_heap_simple
module G = Hmc_cfg_ir
|}]

let wrong_branch (u : unit) = ghost_ (
  let a = {F.pc = D.Z; env = M.Empty; accumulator = V.Boolean false; temporaries = F.Empty; current = V.Nil} in
  let state = Q.Running (a, Q.Halt) in
  let op = G.Branch (D.Z, D.S D.Z) in
  B.step_def op state;
  let proof : {u : unit | B.step op state === Q.Running ({a with F.pc = D.Z}, Q.Halt)} = refine_ () in proof);;
[%%expect{|
Line 6, characters 89-99:
6 |   let proof : {u : unit | B.step op state === Q.Running ({a with F.pc = D.Z}, Q.Halt)} = refine_ () in proof);;
                                                                                             ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let lost_return_value (u : unit) = ghost_ (
  let saved = {F.pc = D.Z; env = M.Empty; accumulator = V.Boolean false; temporaries = F.Empty; current = V.Nil} in
  let callee = {saved with F.accumulator = V.Boolean true} in
  let state = Q.Running (callee, Q.Frame (saved, Q.Halt)) in
  B.step_def G.Return state;
  let proof : {u : unit | B.step G.Return state === Q.Running (saved, Q.Halt)} = refine_ () in proof);;
[%%expect{|
Line 6, characters 81-91:
6 |   let proof : {u : unit | B.step G.Return state === Q.Running (saved, Q.Halt)} = refine_ () in proof);;
                                                                                     ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let one_frame_too_many (u : unit) = ghost_ (
  let saved = {F.pc = D.Z; env = M.Empty; accumulator = V.Nil; temporaries = F.Empty; current = V.Nil} in
  let frames = Q.Frame (saved, Q.Halt) in
  Q.depth_def frames; Q.depth_def Q.Halt;
  D.present_def (D.S D.Z) (Q.depth frames); D.present_def D.Z D.Z;
  let proof : {u : unit | D.present (D.S D.Z) (Q.depth frames)} = refine_ () in proof);;
[%%expect{|
Line 6, characters 66-76:
6 |   let proof : {u : unit | D.present (D.S D.Z) (Q.depth frames)} = refine_ () in proof);;
                                                                      ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
