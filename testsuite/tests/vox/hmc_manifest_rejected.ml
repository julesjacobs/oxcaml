(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml";
 readonly_files = "hmc_manifest_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module M = Hmc_manifest
module C = Hmc_monomorphic;;
[%%expect{|
module D = Hm_declarative
module M = Hmc_manifest
module C = Hmc_monomorphic
|}]

let dangling (id : D.index @ immutable) = ghost_ (
  let refs = M.Reference (D.Z, id) in
  M.size_def M.Nil; M.bounded_def D.Z refs; D.present_def D.Z id;
  let proof : {u : unit | M.bounded (M.size M.Nil) refs} = refine_ () in proof);;
[%%expect{|
Line 4, characters 59-69:
4 |   let proof : {u : unit | M.bounded (M.size M.Nil) refs} = refine_ () in proof);;
                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_body (body : Hmc_specialized_body.t @ immutable) = ghost_ (
  let entry = {M.body; dependencies = M.Reference (D.Z, D.Z)} in
  let table = M.Add (entry, M.Nil) in
  M.valid_def table; M.size_def M.Nil;
  M.bounded_def D.Z entry.M.dependencies; D.present_def D.Z D.Z;
  let proof : {u : unit | M.valid table} = refine_ () in proof);;
[%%expect{|
Line 6, characters 43-53:
6 |   let proof : {u : unit | M.valid table} = refine_ () in proof);;
                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let changed_variable (u : unit) = ghost_ (
  let code = C.Global (D.S D.Z, D.Z, D.Z) in C.erase_def code;
  let proof : {u : unit | C.erase code === D.Bound D.Z} = refine_ () in proof);;
[%%expect{|
Line 3, characters 58-68:
3 |   let proof : {u : unit | C.erase code === D.Bound D.Z} = refine_ () in proof);;
                                                              ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_code_type (u : unit) = ghost_ (
  let code = C.Global (D.Z, D.Z, D.Z) in
  Hmc_monomorphic_typing.typed_def M.Nil D.Empty_context code D.Word64 (D.Variable D.No_arguments);
  M.lookup_def M.Nil D.Z;
  let proof : {u : unit | Hmc_monomorphic_typing.typed M.Nil D.Empty_context code D.Word64
    (D.Variable D.No_arguments)} = refine_ () in proof);;
[%%expect{|
Line 6, characters 35-45:
6 |     (D.Variable D.No_arguments)} = refine_ () in proof);;
                                       ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
