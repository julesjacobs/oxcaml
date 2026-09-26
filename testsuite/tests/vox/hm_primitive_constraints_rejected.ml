(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_primitive_constraints.ml";
 readonly_files = "hm_primitive_constraints_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   run-expectnat;
   check-program-output;
 }
*)
module D = Hm_declarative
module C = Hm_primitive_constraints;;
[%%expect{|
module D = Hm_declarative
module C = Hm_primitive_constraints
|}]

let boolean_operand () = ghost_ (
  let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
  D.typed_def D.Z D.Empty_context D.Truth D.Word64 D.Constant;
  C.construct D.Z D.Empty_context D.Truth word D.Constant D.Word_constant ());;
[%%expect{|
Line 4, characters 74-76:
4 |   C.construct D.Z D.Empty_context D.Truth word D.Constant D.Word_constant ());;
                                                                              ^^
Error: Refinement could not be proved (counterexample)
|}]

let boolean_list_as_word_arguments () = ghost_ (
  let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
  let d = D.List_cons (D.Boolean, D.Constant, D.Empty_list D.Boolean) in
  C.arguments_def D.Truth word;
  D.typed_def D.Z D.Empty_context (C.arguments D.Truth word) (D.List_type D.Boolean) d;
  C.invert D.Z D.Empty_context D.Truth word (D.List_type D.Boolean) d ());;
[%%expect{|
Line 6, characters 70-72:
6 |   C.invert D.Z D.Empty_context D.Truth word (D.List_type D.Boolean) d ());;
                                                                          ^^
Error: Refinement could not be proved (counterexample)
|}]
