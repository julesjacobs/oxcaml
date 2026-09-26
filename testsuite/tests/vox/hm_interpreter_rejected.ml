(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml";
 readonly_files = "hm_interpreter_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)
module D = Hm_declarative
module I = Hm_interpreter;;
[%%expect{|
module D = Hm_declarative
module I = Hm_interpreter
|}]

let wrong_type () =
  let term = D.Truth in
  let ty = D.Function (D.Boolean, D.Boolean) in
  ghost_ (D.typed_def D.Z D.Empty_context term ty D.Constant);
  I.run term #{I.ty = ty; derivation = D.Constant};;
[%%expect{|
Line 5, characters 13-50:
5 |   I.run term #{I.ty = ty; derivation = D.Constant};;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let unbound () =
  let term = D.Bound D.Z in
  let d = D.Variable D.No_arguments in
  ghost_ (D.typed_def D.Z D.Empty_context term D.Boolean d;
    D.lookup_def D.Empty_context D.Z);
  I.run term #{I.ty = D.Boolean; derivation = d};;
[%%expect{|
Line 6, characters 13-48:
6 |   I.run term #{I.ty = D.Boolean; derivation = d};;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let apply_boolean () =
  let term = D.Apply (D.Truth, D.Truth) in
  let d = D.Application (D.Boolean, D.Constant, D.Constant) in
  ghost_ (D.typed_def D.Z D.Empty_context term D.Boolean d;
    D.typed_def D.Z D.Empty_context D.Truth
      (D.Function (D.Boolean, D.Boolean)) D.Constant);
  I.run term #{I.ty = D.Boolean; derivation = d};;
[%%expect{|
Line 7, characters 13-48:
7 |   I.run term #{I.ty = D.Boolean; derivation = d};;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let bypass = I.eval;;
[%%expect{|
Line 1, characters 13-19:
1 | let bypass = I.eval;;
                 ^^^^^^
Error: Unbound value "I.eval"
|}]
