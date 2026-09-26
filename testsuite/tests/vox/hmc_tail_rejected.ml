(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml";
 readonly_files = "hmc_tail_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module W = Hmc_cfg_states
module T = Hmc_tail_sites
module I = Hmc_tail_ir
module F = Hmc_tail_continuation
module B = Hmc_tail_stack;;
[%%expect{|
module D = Hm_declarative
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module W = Hmc_cfg_states
module T = Hmc_tail_sites
module I = Hmc_tail_ir
module F = Hmc_tail_continuation
module B = Hmc_tail_stack
|}]

let pending_arithmetic (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None};
    instruction = G.Primitive (D.Add, D.Z)} in
  let table = G.Add (block, G.Empty) in
  T.exit_valid_def table (T.Return D.Z); O.instruction_def table D.Z G.Return;
  G.lookup_def table D.Z; G.size_def G.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | T.exit_valid table (T.Return D.Z)} = refine_ () in proof);;
[%%expect{|
Line 8, characters 63-73:
8 |   let proof : {u : unit | T.exit_valid table (T.Return D.Z)} = refine_ () in proof);;
                                                                   ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_rewrite (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None};
    instruction = G.Call D.Z} in
  let table = G.Add (block, G.Empty) in
  let sites = T.Site (D.Z, T.Return D.Z, T.Empty) in
  let code = I.Add (I.Keep (G.Call D.Z), I.Empty) in
  I.related_def table code sites; G.size_def G.Empty; I.select_def sites D.Z (G.Call D.Z);
  I.find_def sites D.Z;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | I.related table code sites} = refine_ () in proof);;
[%%expect{|
Line 10, characters 56-66:
10 |   let proof : {u : unit | I.related table code sites} = refine_ () in proof);;
                                                             ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let ordinary_call_bound (u : unit) = ghost_ (
  let code = I.Add (I.Keep (G.Call D.Z), I.Empty) in
  B.no_calls_def code; B.ordinary_call_def (I.Keep (G.Call D.Z));
  let proof : {u : unit | B.no_calls code} = refine_ () in proof);;
[%%expect{|
Line 4, characters 45-55:
4 |   let proof : {u : unit | B.no_calls code} = refine_ () in proof);;
                                                 ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let dropped_caller (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None};
    instruction = G.Return} in
  let blocks = G.Add (block, G.Empty) in
  let root = W.Halt (R.V.Empty, R.V.Nil, D.Z) in
  let k = W.Call_return (R.V.Empty, R.V.Nil, D.Z, R.V.True, root) in
  G.lookup_def blocks D.Z; G.size_def G.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  O.instruction_def blocks D.Z G.Return;
  W.continuation_valid_def blocks root; W.continuation_valid_def blocks k; F.tail_def k;
  let out = F.retarget blocks k R.V.Empty R.V.Nil D.Z () in
  W.frames_def k;
  let proof : {u : unit | W.frames out === S.Halt} = refine_ () in proof);;
[%%expect{|
Line 13, characters 53-63:
13 |   let proof : {u : unit | W.frames out === S.Halt} = refine_ () in proof);;
                                                          ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
