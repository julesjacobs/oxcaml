(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml provenance_spec.ml";
 readonly_files = "copy_order_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

open Copy_spec;;
open Level_spec;;
[%%expect{|
|}]

module Deep_boundary = struct
  let bad : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      {u : unit | H.mem h x && H.at h x === Some (cell Var 2)} ->
      {u : unit | not (H.mem h x) || not (finite_node h x) || below h x 1} @ ghost =
    fun h x premise -> ghost_ (
      let refine_ premise = premise in let desc : desc = Var in cell_def desc 2;
      finite_node_def h x; below_def h x 1; at_level_def h x;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 20-29:
8 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unordered_instance = struct
  let bad : (h : node Pref.heap) @ immutable -> (root : node Pref.t) @ immutable ->
      (child : node Pref.t) @ immutable ->
      {u : unit | H.mem h child && H.at h child === Some (cell Var 2)
        && H.at h root === Some (cell (Arrow (child, child)) 1)} ->
      {u : unit | ordered h root} @ ghost = fun h root child premise -> ghost_ (
    let refine_ premise = premise in let leaf : desc = Var in cell_def leaf 2;
    let desc : desc = Arrow (child, child) in cell_def desc 1;
    ordered_def h root; children_below_def h desc 1; below_def h child 1;
    at_level_def h child; let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 40-49:
10 |     at_level_def h child; let u = () in refine_ u)
                                             ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
