(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml";
 readonly_files = "hmc_expansion_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
*)
module D = Hm_declarative
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module B = Hmc_specialized_body
module R = Hmc_reference_tree
module X = Hmc_expansion;;
[%%expect{|
module D = Hm_declarative
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module B = Hmc_specialized_body
module R = Hmc_reference_tree
module X = Hmc_expansion
|}]

let uninstantiated_body (origin : I.instance @ immutable) = ghost_ (
  let forged = {B.origin; derivation = origin.I.definition.T.derivation} in
  B.valid_def forged;
  let out : B.t = refine_ forged in out);;
[%%expect{|
Line 4, characters 18-32:
4 |   let out : B.t = refine_ forged in out);;
                      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_reference_index (catalog : T.catalog @ immutable) (origin : I.instance @ immutable) = ghost_ (
  let term = D.Bound D.Z in let args = A.declarative origin.I.key.A.arguments in
  let d = D.Variable args in let tree = R.Reference (D.S D.Z, origin) in
  R.records_def catalog D.Empty_context term d tree;
  R.global_index_def D.Empty_context D.Z;
  let proof : {u : unit | R.records catalog D.Empty_context term d tree} = refine_ () in proof);;
[%%expect{|
Line 6, characters 75-85:
6 |   let proof : {u : unit | R.records catalog D.Empty_context term d tree} = refine_ () in proof);;
                                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let omitted_global_reference (catalog : T.catalog @ immutable) = ghost_ (
  let term = D.Bound D.Z in let d = D.Variable D.No_arguments in
  R.records_def catalog D.Empty_context term d R.Empty;
  R.global_index_def D.Empty_context D.Z;
  let proof : {u : unit | R.records catalog D.Empty_context term d R.Empty} = refine_ () in proof);;
[%%expect{|
Line 5, characters 78-88:
5 |   let proof : {u : unit | R.records catalog D.Empty_context term d R.Empty} = refine_ () in proof);;
                                                                                  ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let insufficient_rank (origin : I.instance @ immutable) = ghost_ (
  D.present_def D.Z origin.I.key.A.owner;
  X.expand D.Z origin ());;
[%%expect{|
Line 3, characters 22-24:
3 |   X.expand D.Z origin ());;
                          ^^
Error: Refinement could not be proved (counterexample)
|}]

