(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml";
 readonly_files = "hmc_closure_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module K = Hmc_closure_ir
module R = Hmc_closure_semantics
module W = Hmc_closure_values;;
[%%expect{|
module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module K = Hmc_closure_ir
module R = Hmc_closure_semantics
module W = Hmc_closure_values
|}]

let wrong_capture (u : unit) = ghost_ (
  let captured = D.Binding (D.Forall (D.Z, D.Boolean), D.Empty_context) in
  let entry = {K.recursive = false; captured; argument = D.Word64; result = D.Word64;
    source = C.Local D.Z; derivation = D.Variable D.No_arguments; body = K.Local D.Z} in
  let table = K.Add (entry, K.Empty) in let code = K.Closure D.Z in
  let ty = D.Function (D.Word64, D.Word64) in let d = D.Abstraction (D.Word64, entry.K.derivation) in
  K.typed_def M.Nil table D.Empty_context code ty d;
  K.lookup_def table D.Z; K.size_def K.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | K.typed M.Nil table D.Empty_context code ty d} = refine_ () in proof);;
[%%expect{|
Line 10, characters 75-85:
10 |   let proof : {u : unit | K.typed M.Nil table D.Empty_context code ty d} = refine_ () in proof);;
                                                                                ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_recursion (u : unit) = ghost_ (
  let entry = {K.recursive = false; captured = D.Empty_context; argument = D.Word64; result = D.Word64;
    source = C.Local D.Z; derivation = D.Variable D.No_arguments; body = K.Local D.Z} in
  let table = K.Add (entry, K.Empty) in let code = K.Closure D.Z in
  let source = C.Recursive (C.Local D.Z) in
  K.related_def table source code; K.lookup_def table D.Z; K.size_def K.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | K.related table source code} = refine_ () in proof);;
[%%expect{|
Line 8, characters 57-67:
8 |   let proof : {u : unit | K.related table source code} = refine_ () in proof);;
                                                             ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_code (u : unit) = ghost_ (
  let value = R.V.Closure (D.Z, R.V.Empty) in
  W.valid_def K.Empty value; K.lookup_def K.Empty D.Z;
  let proof : {u : unit | W.valid K.Empty value} = refine_ () in proof);;
[%%expect{|
Line 4, characters 51-61:
4 |   let proof : {u : unit | W.valid K.Empty value} = refine_ () in proof);;
                                                       ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_result_type (u : unit) = ghost_ (
  let entry = {K.recursive = false; captured = D.Empty_context; argument = D.Word64; result = D.Boolean;
    source = C.Truth; derivation = D.Constant; body = K.Truth} in
  let table = K.Add (entry, K.Empty) in let code = K.Closure D.Z in
  let ty = D.Function (D.Word64, D.Word64) in let d = D.Abstraction (D.Word64, D.Constant) in
  K.typed_def M.Nil table D.Empty_context code ty d;
  K.lookup_def table D.Z; K.size_def K.Empty;
  let _ = Hm_elaboration_check.index_equal D.Z D.Z in
  let proof : {u : unit | K.typed M.Nil table D.Empty_context code ty d} = refine_ () in proof);;
[%%expect{|
Line 9, characters 75-85:
9 |   let proof : {u : unit | K.typed M.Nil table D.Empty_context code ty d} = refine_ () in proof);;
                                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
