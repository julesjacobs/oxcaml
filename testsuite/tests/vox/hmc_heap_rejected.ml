(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml hmc_slot_copy.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml";
 readonly_files = "hmc_heap_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module R = Hmc_closure_semantics
module C = Hmc_tagged_cell
module K = Hmc_closure_ir
module M = Hmc_heap_objects
module E = Hmc_heap_extent;;
[%%expect{|
module D = Hm_declarative
module R = Hmc_closure_semantics
module C = Hmc_tagged_cell
module K = Hmc_closure_ir
module M = Hmc_heap_objects
module E = Hmc_heap_extent
|}]

let short_extent (u : unit) = ghost_ (
  let start : C.W.limb = 64 and stop : C.W.limb = 80 in
  E.span_def (D.S (D.S D.Z)) 64 80;
  E.span_def (D.S D.Z) 80 80;
  E.span_def D.Z 96 80;
  let proof : {u : unit | E.span (D.S (D.S D.Z)) start stop} = refine_ () in proof);;
[%%expect{|
Line 6, characters 63-73:
6 |   let proof : {u : unit | E.span (D.S (D.S D.Z)) start stop} = refine_ () in proof);;
                                                                   ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let overlapping_allocation (u : unit) = ghost_ (
  let object_ = M.Cons (C.Boolean true, C.Nil) in
  let first = M.Allocate ({M.address = 64; stop = 96; object_}, M.Empty_heap 64) in
  let second = M.Allocate ({M.address = 64; stop = 96; object_}, first) in
  M.valid_def K.Empty second; M.used_def first;
  let proof : {u : unit | M.valid K.Empty second} = refine_ () in proof);;
[%%expect{|
Line 6, characters 52-62:
6 |   let proof : {u : unit | M.valid K.Empty second} = refine_ () in proof);;
                                                        ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let dangling_reference (u : unit) = ghost_ (
  let object_ = M.Cons (C.Cons_pointer 64, C.Nil) in
  M.object_valid_def K.Empty M.No_values object_;
  M.decode_object_def M.No_values object_;
  M.decode_value_def M.No_values (C.Cons_pointer 64); M.lookup_def M.No_values 64;
  let proof : {u : unit | M.object_valid K.Empty M.No_values object_} = refine_ () in proof);;
[%%expect{|
Line 6, characters 72-82:
6 |   let proof : {u : unit | M.object_valid K.Empty M.No_values object_} = refine_ () in proof);;
                                                                            ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_object_kind (u : unit) = ghost_ (
  let address : C.W.limb = 64 in
  let initial = M.Empty_heap 64 in
  let object_ = M.Cons (C.Boolean true, C.Nil) in
  let heap = M.Allocate ({M.address = 64; stop = 96; object_}, initial) in
  M.view_def initial; M.view_def heap;
  M.decode_object_def M.No_values object_;
  M.decode_value_def M.No_values (C.Boolean true); M.decode_value_def M.No_values C.Nil;
  M.decode_def heap (C.Closure_pointer 64); M.decode_value_def (M.view heap) (C.Closure_pointer 64);
  M.lookup_def (M.view heap) 64;
  let proof : {u : unit | M.decode heap (C.Closure_pointer address) === Some (R.V.Closure (D.Z, R.V.Empty))} = refine_ () in proof);;
[%%expect{|
Line 11, characters 111-121:
11 |   let proof : {u : unit | M.decode heap (C.Closure_pointer address) === Some (R.V.Closure (D.Z, R.V.Empty))} = refine_ () in proof);;
                                                                                                                    ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
