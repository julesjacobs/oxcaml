(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_source_safety.ml";
 readonly_files = "hmc_source_safety_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)
module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module P = Hmc_source_safety;;
[%%expect{|
module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module P = Hmc_source_safety
|}]

let stuck_is_safe () = ghost_ (
  let proof = P.Complete V.Leaf in
  P.valid_def S.Stuck D.Boolean proof;
  P.advance_preserves D.Z S.Stuck D.Boolean proof ());;
[%%expect{|
Line 4, characters 50-52:
4 |   P.advance_preserves D.Z S.Stuck D.Boolean proof ());;
                                                      ^^
Error: Refinement could not be proved (counterexample)
|}]

let truth_has_word_type () = ghost_ (
  D.typed_def D.Z D.Empty_context D.Truth D.Word64 D.Constant;
  P.initial_typed D.Truth D.Word64 D.Constant ());;
[%%expect{|
Line 3, characters 46-48:
3 |   P.initial_typed D.Truth D.Word64 D.Constant ());;
                                                  ^^
Error: Refinement could not be proved (counterexample)
|}]

let zero_fuel_finishes () : {u : unit |
    S.advance D.Z (S.initial D.Truth) === S.Done V.True} = ghost_ (
  S.initial_def D.Truth; S.advance_def D.Z (S.initial D.Truth); ());;
[%%expect{|
Line 3, characters 64-66:
3 |   S.initial_def D.Truth; S.advance_def D.Z (S.initial D.Truth); ());;
                                                                    ^^
Error: Refinement could not be proved (counterexample)
|}]

let halt_changes_type () : {u : unit |
    P.continuation_typed S.Halt D.Word64 D.Boolean P.Stop} = ghost_ (
  P.continuation_typed_def S.Halt D.Word64 D.Boolean P.Stop; ());;
[%%expect{|
Line 3, characters 61-63:
3 |   P.continuation_typed_def S.Halt D.Word64 D.Boolean P.Stop; ());;
                                                                 ^^
Error: Refinement could not be proved (counterexample)
|}]
