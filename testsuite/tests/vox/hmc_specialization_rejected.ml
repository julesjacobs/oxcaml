(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml";
 readonly_files = "hmc_specialization_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
 { setup-ocamlopt.opt-build-env; ocamlopt.opt; run-expectnat; check-program-output; }
*)
module D = Hm_declarative
module G = Hmc_ground_type
module Args = Hmc_ground_arguments
module N = Hmc_no_free
module I = Hmc_instance;;
[%%expect{|
module D = Hm_declarative
module G = Hmc_ground_type
module Args = Hmc_ground_arguments
module N = Hmc_no_free
module I = Hmc_instance
|}]

let wrong_arity () = ghost_ (
  let scheme = D.Forall (D.Z, D.Word64) in
  D.scheme_wf_def D.Z scheme; D.add_def D.Z D.Z; D.mono_wf_def D.Z D.Word64;
  N.scheme_def scheme; N.mono_def D.Word64;
  let args = Args.Argument (G.Word64, Args.Empty) in
  Args.length_def args; Args.length_def Args.Empty; D.arity_def scheme;
  Args.instantiate (refine_ scheme) (refine_ args));;
[%%expect{|
Line 7, characters 36-50:
7 |   Args.instantiate (refine_ scheme) (refine_ args));;
                                        ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let free_template (p : Copy_spec.node Pref.t @ immutable) = ghost_ (
  let ty = D.Free p in let scheme = D.Forall (D.Z, ty) in
  D.scheme_wf_def D.Z scheme; D.add_def D.Z D.Z; D.mono_wf_def D.Z ty;
  N.scheme_def scheme; N.mono_def ty;
  Args.instantiate (refine_ scheme) (refine_ Args.Empty));;
[%%expect{|
Line 5, characters 36-56:
5 |   Args.instantiate (refine_ scheme) (refine_ Args.Empty));;
                                        ^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let changed_owner (input : I.instance @ immutable) = ghost_ (
  I.valid_def input;
  let key = {Args.owner = D.S input.I.key.Args.owner; arguments = input.I.key.Args.arguments} in
  let forged = {I.key; definition = input.I.definition; earlier = input.I.earlier; ty = input.I.ty} in
  I.valid_def forged;
  let out : I.instance = refine_ forged in out);;
[%%expect{|
Line 6, characters 25-39:
6 |   let out : I.instance = refine_ forged in out);;
                             ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

