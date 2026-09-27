(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml structure_spec.ml";
 readonly_files = "structure_rejected.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
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
Line 4, characters 18-32:
4 |       {u : unit | linkable h a a} @ ghost = fun h a b premise -> ghost_ (
                      ^^^^^^^^^^^^^^
  The refinement is stated here.
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
Line 4, characters 18-32:
4 |       {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
                      ^^^^^^^^^^^^^^
  The refinement is stated here.
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
Line 4, characters 18-32:
4 |       {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
                      ^^^^^^^^^^^^^^
  The refinement is stated here.
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
Line 4, characters 18-32:
4 |       {u : unit | linkable h a b} @ ghost = fun h a b premise -> ghost_ (
                      ^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

