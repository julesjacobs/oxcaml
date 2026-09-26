(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_conditional_constraints.ml hm_list_case_constraints.ml";
 readonly_files = "hm_control_constraints_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)
module D = Hm_declarative
module C = Hm_conditional_constraints
module L = Hm_list_case_constraints;;
[%%expect{|
module D = Hm_declarative
module C = Hm_conditional_constraints
module L = Hm_list_case_constraints
|}]

let non_boolean_condition () = ghost_ (
  let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
  let term = D.If (word, D.Truth, D.False) in
  let d = D.Conditional (D.Word_constant, D.Constant, D.Constant) in
  D.typed_def D.Z D.Empty_context term D.Boolean d;
  C.construct D.Z D.Empty_context word D.Truth D.False D.Boolean d ());;
[%%expect{|
Line 6, characters 67-69:
6 |   C.construct D.Z D.Empty_context word D.Truth D.False D.Boolean d ());;
                                                                       ^^
Error: Refinement could not be proved (counterexample)
|}]

let unequal_conditional_branches () = ghost_ (
  let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
  let d = D.Conditional (D.Constant, D.Constant, D.Word_constant) in
  D.typed_def D.Z D.Empty_context (D.If (D.Truth, D.Truth, word)) D.Boolean d;
  C.construct D.Z D.Empty_context D.Truth D.Truth word D.Boolean d ());;
[%%expect{|
Line 5, characters 67-69:
5 |   C.construct D.Z D.Empty_context D.Truth D.Truth word D.Boolean d ());;
                                                                       ^^
Error: Refinement could not be proved (counterexample)
|}]

let list_head_is_not_a_tail () = ghost_ (
  let scrutinee = D.Cons (D.Truth, D.Nil) in
  let nonempty = D.Bound D.Z in let ty = D.List_type D.Boolean in
  let ds = D.List_cons (D.Boolean, D.Constant, D.Empty_list D.Boolean) in
  let d = D.List_case (D.Boolean, ds, D.Empty_list D.Boolean, D.Variable D.No_arguments) in
  D.typed_def D.Z D.Empty_context (D.CaseList (scrutinee, D.Nil, nonempty)) ty d;
  L.construct D.Z D.Empty_context scrutinee D.Nil nonempty ty d ());;
[%%expect{|
Line 7, characters 64-66:
7 |   L.construct D.Z D.Empty_context scrutinee D.Nil nonempty ty d ());;
                                                                    ^^
Error: Refinement could not be proved (counterexample)
|}]

let non_list_scrutinee () = ghost_ (
  let d = D.List_case (D.Boolean, D.Constant, D.Constant, D.Constant) in
  D.typed_def D.Z D.Empty_context (D.CaseList (D.Truth, D.Truth, D.False)) D.Boolean d;
  L.construct D.Z D.Empty_context D.Truth D.Truth D.False D.Boolean d ());;
[%%expect{|
Line 4, characters 70-72:
4 |   L.construct D.Z D.Empty_context D.Truth D.Truth D.False D.Boolean d ());;
                                                                          ^^
Error: Refinement could not be proved (counterexample)
|}]
