(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml pooled_spec.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_declarative.ml hm_environment_spec.ml hm_execution_spec.ml hm_runtime_spec.ml";
 readonly_files = "hm_runtime_rejected.ml";
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
open Hm_declarative;;
open Hm_runtime_spec;;
[%%expect{|
|}]

module Unbound_closed_variable = struct
  let bad : {e : term | scoped_term Z e && term_let_free e} =
    let z = Z in let e = Bound z in ghost_ (scoped_term_def z e; present_def z z);
    refine_ e
end;;
[%%expect{|
Line 4, characters 4-13:
4 |     refine_ e
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Let_in_let_free_driver = struct
  let bad : {e : term | term_let_free e} =
    let e = Let (Truth, Bound Z) in ghost_ (term_let_free_def e);
    refine_ e
end;;
[%%expect{|
Line 4, characters 4-13:
4 |     refine_ e
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_mark_invariant = struct
  let bad : (h : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      {u : unit | safe h x} @ ghost = fun h x -> ghost_ (
    safe_def h x; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 32-41:
4 |     safe_def h x; let u = () in refine_ u)
                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unchecked_level_increment = struct
  let bad : (depth : {n : int | n >= 0}) -> {n : int | n >= 0} = fun depth ->
    let refine_ depth = depth in let next = depth + 1 in refine_ next
end;;
[%%expect{|
Line 3, characters 57-69:
3 |     let refine_ depth = depth in let next = depth + 1 in refine_ next
                                                             ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Discard_parent_pool = struct
  let bad : (p : node Pref.t) @ immutable ->
      {u : unit | Generalize_spec.covered (H.put (H.empty ()) p (cell Var 0)) (-1) Generalize_spec.Empty p} @ ghost = fun p -> ghost_ (
    let h = H.empty () in let desc : desc = Var in let v = cell desc 0 in cell_def desc 0;
    let after = H.put h p v in let empty : Generalize_spec.pool = Generalize_spec.Empty in
    Generalize_spec.covered_def after (-1) empty p; Generalize_spec.listed_def empty p;
    Level_spec.at_level_def after p; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 51-60:
7 |     Level_spec.at_level_def after p; let u = () in refine_ u)
                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
