(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml structure_spec.ml";
 readonly_files = "structure_rejected.ml";
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
open Level_spec;;
open Level_unifier_spec;;
open Level_finite_spec;;
open Structure_spec;;
[%%expect{|
|}]

module Self_link = struct
  let bad : (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
      {u : unit | finite h a && finite h b} ->
      {u : unit | linkable h a a} @ ghost = fun h a b premise -> ghost_ (
    let refine_ premise = premise in linkable_def h a a; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 71-80:
5 |     let refine_ premise = premise in linkable_def h a a; let u = () in refine_ u)
                                                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unequal_readback = struct
  let bad : (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
      {u : unit | finite h a && finite h b && not (readback a === readback b)} ->
      {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
    let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 71-80:
5 |     let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
                                                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Deeper_target = struct
  let bad : (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
      {u : unit | finite h a && finite h b && at_level h (tree_root a) === Finite 0 && at_level h (tree_root b) === Finite 1} ->
      {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
    let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 71-80:
5 |     let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
                                                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generic_source = struct
  let bad : (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
      {u : unit | finite h a && finite h b && at_level h (tree_root a) === Generic} ->
      {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
    let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 71-80:
5 |     let refine_ premise = premise in linkable_def h a b; let u = () in refine_ u)
                                                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

