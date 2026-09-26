(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml";
 readonly_files = "hmc_cfg_simulation_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module W = Hmc_cfg_states;;
[%%expect{|
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module W = Hmc_cfg_states
|}]

let missing_restore (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None};
    instruction = G.Jump D.Z} in
  let table = G.Add (block, G.Empty) in
  let k = W.Scope (R.V.Empty, D.Z, W.Halt (R.V.Empty, R.V.Nil, D.Z)) in
  W.continuation_valid_def table k;
  W.resume_def (W.Halt (R.V.Empty, R.V.Nil, D.Z));
  O.instruction_def table D.Z (G.Restore D.Z); G.lookup_def table D.Z; G.size_def G.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | W.continuation_valid table k} = refine_ () in proof);;
[%%expect{|
Line 10, characters 58-68:
10 |   let proof : {u : unit | W.continuation_valid table k} = refine_ () in proof);;
                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let changed_capture (u : unit) = ghost_ (
  let current = R.V.Closure (D.Z, R.V.Bind (R.V.True, R.V.Empty)) in
  let changed = R.V.Closure (D.Z, R.V.Bind (R.V.False, R.V.Empty)) in
  let k = W.Call_return (R.V.Empty, current, D.Z, R.V.Nil, W.Halt (R.V.Empty, R.V.Nil, D.Z)) in
  W.current_def k;
  let proof : {u : unit | W.current k === changed} = refine_ () in proof);;
[%%expect{|
Line 6, characters 53-63:
6 |   let proof : {u : unit | W.current k === changed} = refine_ () in proof);;
                                                         ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let administrative_step_count (u : unit) = ghost_ (
  let k = W.Scope (R.V.Empty, D.Z, W.Halt (R.V.Empty, R.V.Nil, D.Z)) in
  let state = W.Running (W.Returning, k, R.V.True) in
  W.source_steps_def state;
  let proof : {u : unit | W.source_steps state === D.S D.Z} = refine_ () in proof);;
[%%expect{|
Line 5, characters 62-72:
5 |   let proof : {u : unit | W.source_steps state === D.S D.Z} = refine_ () in proof);;
                                                                  ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let dropped_call_frame (u : unit) = ghost_ (
  let k = W.Call_return (R.V.Empty, R.V.Nil, D.Z, R.V.True, W.Halt (R.V.Empty, R.V.Nil, D.Z)) in
  W.frames_def k;
  let proof : {u : unit | W.frames k === S.Halt} = refine_ () in proof);;
[%%expect{|
Line 4, characters 51-61:
4 |   let proof : {u : unit | W.frames k === S.Halt} = refine_ () in proof);;
                                                       ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
