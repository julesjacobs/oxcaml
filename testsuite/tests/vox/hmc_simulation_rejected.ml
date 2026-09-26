(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml";
 readonly_files = "hmc_simulation_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module L = Hmc_monomorphic_links
module W = Hmc_monomorphic_values
module H = Hmc_monomorphic_states
module S = Hmc_source_semantics
module V = Hm_interpreter_typing;;
[%%expect{|
module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module L = Hmc_monomorphic_links
module W = Hmc_monomorphic_values
module H = Hmc_monomorphic_states
module S = Hmc_source_semantics
module V = Hm_interpreter_typing
|}]

let missing_local (u : unit) = ghost_ (
  W.length_def W.Empty; W.environment_def W.Empty; W.valid_def M.Nil W.Empty;
  L.strip_def D.Z D.Z;
  W.local M.Nil W.Empty V.Empty D.Z ());;
[%%expect{|
Line 4, characters 36-38:
4 |   W.local M.Nil W.Empty V.Empty D.Z ());;
                                        ^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_catalog_index (table : M.table @ immutable) = ghost_ (
  let code = C.Global (D.S D.Z, D.Z, D.Z) in
  L.linked_def table T.Empty D.Z code; L.strip_def D.Z (D.S D.Z);
  let proof : {u : unit | L.linked table T.Empty D.Z code} = refine_ () in proof);;
[%%expect{|
Line 4, characters 61-71:
4 |   let proof : {u : unit | L.linked table T.Empty D.Z code} = refine_ () in proof);;
                                                                 ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let missing_capture (table : M.table @ immutable) = ghost_ (
  let code = C.Local (D.S D.Z) in
  let closure = W.Closure (T.Empty, code, W.Empty) in
  W.valid_def table closure; W.length_def W.Empty;
  L.linked_def table T.Empty (D.S D.Z) code;
  L.strip_def (D.S D.Z) (D.S D.Z); L.strip_def D.Z D.Z;
  let proof : {u : unit | W.valid table closure} = refine_ () in proof);;
[%%expect{|
Line 7, characters 51-61:
7 |   let proof : {u : unit | W.valid table closure} = refine_ () in proof);;
                                                       ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let changed_result (u : unit) = ghost_ (
  let zero = {Hmc_word64.lo = 0; hi = 0} in
  let one = {Hmc_word64.lo = 1; hi = 0} in
  let state = H.Done (W.Word zero) in H.source_def state; W.source_def (W.Word zero);
  let proof : {u : unit | H.source state === S.Done (V.Word one)} = refine_ () in proof);;
[%%expect{|
Line 5, characters 68-78:
5 |   let proof : {u : unit | H.source state === S.Done (V.Word one)} = refine_ () in proof);;
                                                                        ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
