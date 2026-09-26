(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml";
 readonly_files = "hmc_cfg_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module K = Hmc_closure_ir
module M = Hmc_manifest
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin;;
[%%expect{|
module D = Hm_declarative
module K = Hmc_closure_ir
module M = Hmc_manifest
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
|}]

let dangling_jump (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None};
    instruction = G.Jump D.Z} in
  G.block_valid_def M.Nil K.Empty G.Empty block;
  G.accepts_def G.Empty D.Z D.Empty_context G.Empty_temporaries None; G.lookup_def G.Empty D.Z;
  let proof : {u : unit | G.block_valid M.Nil K.Empty G.Empty block} = refine_ () in proof);;
[%%expect{|
Line 6, characters 71-81:
6 |   let proof : {u : unit | G.block_valid M.Nil K.Empty G.Empty block} = refine_ () in proof);;
                                                                           ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_accumulator (u : unit) = ghost_ (
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = Some D.Word64} in
  let destination = {G.signature; instruction = G.Return} in
  let table = G.Add (destination, G.Empty) in
  let block = {G.signature = {signature with accumulator = None}; instruction = G.Load (G.Truth, D.Boolean, D.Constant, D.Z)} in
  G.block_valid_def M.Nil K.Empty table block;
  G.accepts_def table D.Z D.Empty_context G.Empty_temporaries (Some D.Boolean);
  G.lookup_def table D.Z; G.size_def G.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | G.block_valid M.Nil K.Empty table block} = refine_ () in proof);;
[%%expect{|
Line 10, characters 69-79:
10 |   let proof : {u : unit | G.block_valid M.Nil K.Empty table block} = refine_ () in proof);;
                                                                          ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_temporary (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = Some D.Word64};
    instruction = G.Save_value D.Z} in
  G.block_valid_def M.Nil K.Empty G.Empty block;
  let proof : {u : unit | G.block_valid M.Nil K.Empty G.Empty block} = refine_ () in proof);;
[%%expect{|
Line 5, characters 71-81:
5 |   let proof : {u : unit | G.block_valid M.Nil K.Empty G.Empty block} = refine_ () in proof);;
                                                                           ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let changed_operation (u : unit) = ghost_ (
  let block = {G.signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = Some D.Word64};
    instruction = G.Primitive (D.Subtract, D.Z)} in
  let table = G.Add (block, G.Empty) in
  O.instruction_def table D.Z (G.Primitive (D.Add, D.Z)); G.lookup_def table D.Z; G.size_def G.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | O.instruction table D.Z (G.Primitive (D.Add, D.Z))} = refine_ () in proof);;
[%%expect{|
Line 7, characters 80-90:
7 |   let proof : {u : unit | O.instruction table D.Z (G.Primitive (D.Add, D.Z))} = refine_ () in proof);;
                                                                                    ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
