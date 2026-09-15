(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml pooled_spec.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_declarative.ml hm_environment_spec.ml hm_execution_spec.ml";
 readonly_files = "hm_execution_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)
open Copy_spec;;
open Generalize_spec;;
open Hm_environment_spec;;
open Hm_execution_spec;;
[%%expect{|
|}]

module Skip_boolean_allocation = struct
  let bad : (p : node Pref.t) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
      (RBool p) (H.empty ()) Generalize_spec.Empty} @ ghost = fun p -> ghost_ (
    let h = H.empty () in let pool : pool = Generalize_spec.Empty in
    let env : env = Hm_environment_spec.Empty in let e = RBool p in
    ran_def h 0 pool env e h pool; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 49-58:
7 |     ran_def h 0 pool env e h pool; let u = () in refine_ u)
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Return_result_after_failure = struct
  let bad : (p : node Pref.t) @ immutable -> (left : execution) @ immutable ->
    {u : unit | result (RApp_left (left, D.Truth)) === Some p} @ ghost = fun p left -> ghost_ (
    let e = RApp_left (left, D.Truth) in result_def e; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 69-78:
4 |     let e = RApp_left (left, D.Truth) in result_def e; let u = () in refine_ u)
                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
