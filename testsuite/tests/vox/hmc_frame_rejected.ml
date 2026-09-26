(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml hmc_slot_copy.ml";
 readonly_files = "hmc_frame_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
*)
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module K = Hmc_closure_ir
module M = Hmc_monomorphic
module G = Hmc_cfg_ir
module H = Hmc_frame_shape
module F = Hmc_frame_codec;;
[%%expect{|
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module K = Hmc_closure_ir
module M = Hmc_monomorphic
module G = Hmc_cfg_ir
module H = Hmc_frame_shape
module F = Hmc_frame_codec
|}]

let missing_local (u : unit) = ghost_ (
  let g = D.Binding (D.Forall (D.Z, D.Boolean), D.Empty_context) in
  H.environment_def g R.V.Empty;
  let proof : {u : unit | H.environment g R.V.Empty} = refine_ () in proof);;
[%%expect{|
Line 4, characters 55-65:
4 |   let proof : {u : unit | H.environment g R.V.Empty} = refine_ () in proof);;
                                                           ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_temporary_kind (u : unit) = ghost_ (
  let schema = G.Environment (D.Empty_context, G.Empty_temporaries) in
  let runtime = S.Value (R.V.True, R.V.Empty, S.Empty) in
  F.temporaries_shape_def schema runtime;
  let proof : {u : unit | F.temporaries_shape schema runtime} = refine_ () in proof);;
[%%expect{|
Line 5, characters 64-74:
5 |   let proof : {u : unit | F.temporaries_shape schema runtime} = refine_ () in proof);;
                                                                    ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_capture (u : unit) = ghost_ (
  let g = D.Binding (D.Forall (D.Z, D.Boolean), D.Empty_context) in
  let entry = {K.recursive = false; captured = g; argument = D.Word64; result = D.Word64;
    source = M.Nil; derivation = D.Empty_list D.Word64; body = K.Nil} in
  let table = K.Add (entry, K.Empty) in
  let closure = R.V.Closure (D.Z, R.V.Empty) in
  H.valid_def table closure; K.lookup_def table D.Z; K.size_def K.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  H.environment_def g R.V.Empty;
  let proof : {u : unit | H.valid table closure} = refine_ () in proof);;
[%%expect{|
Line 10, characters 51-61:
10 |   let proof : {u : unit | H.valid table closure} = refine_ () in proof);;
                                                        ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let truncated_frame (u : unit) = ghost_ (
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None} in
  let a = {S.pc = D.Z; env = R.V.Empty; accumulator = R.V.True; temporaries = S.Empty; current = R.V.Nil} in
  F.decode_def signature D.Z F.Empty;
  let proof : {u : unit | F.decode signature D.Z F.Empty === Some (a, F.Empty)} = refine_ () in proof);;
[%%expect{|
Line 5, characters 82-92:
5 |   let proof : {u : unit | F.decode signature D.Z F.Empty === Some (a, F.Empty)} = refine_ () in proof);;
                                                                                      ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
