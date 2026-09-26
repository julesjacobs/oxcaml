(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "register_allocation_spec.ml register_allocation.mli register_allocation.ml";
 readonly_files = "register_allocation_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

let graph = Register_allocation.graph;;
[%%expect{|
Line 1, characters 12-37:
1 | let graph = Register_allocation.graph;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Register_allocation.graph"
|}]

open Register_allocation_spec;;

let (stuck_is_equivalent @ total) :
    unit -> {u : unit | observable_equal Stuck Stuck} @ ghost =
  fun () -> ghost_ (
  observable_equal_def Stuck Stuck;
  let u = () in
  u);;
[%%expect{|
Line 8, characters 2-3:
8 |   u);;
      ^
Error: Refinement could not be proved (counterexample)
|}]

let (different_results_are_equivalent @ total) :
    unit -> {u : unit | observable_equal (Done 1) (Done 2)} @ ghost =
  fun () -> ghost_ (
  observable_equal_def (Done 1) (Done 2);
  let u = () in
  u);;
[%%expect{|
Line 6, characters 2-3:
6 |   u);;
      ^
Error: Refinement could not be proved (counterexample)
|}]
