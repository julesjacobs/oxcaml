(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml provenance_spec.ml";
 readonly_files = "provenance_rejected.ml";
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
open Lower_locality_spec;;
[%%expect{|
|}]

module Unrelated_write = struct
  let bad : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (old : node) @ immutable -> {u : unit | not (p === q)} ->
      {u : unit | confined (Lower (p, old, Keep)) (Tip q)} @ ghost =
    fun p q old premise -> ghost_ (
      let refine_ premise = premise in let tree = Tip q in
      let rest = Keep in let edits = Lower (p, old, rest) in
      confined_def edits tree; contains_def tree p; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 66-75:
8 |       confined_def edits tree; contains_def tree p; let u = () in refine_ u)
                                                                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Wrong_target = struct
  let bad : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | not (p === q)} ->
      {u : unit | bound_root (Tip p) === q} @ ghost = fun p q premise -> ghost_ (
    let refine_ premise = premise in let tree = Tip p in bound_root_def tree;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 18-27:
6 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

open Level_unifier_spec;;
[%%expect{|
|}]

module Wrong_binding_level = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (q : node Pref.t) @ immutable -> (after : node Pref.heap) @ immutable ->
      (edits : lowering) @ immutable -> (tree : bounded) @ immutable ->
      (rest : derivation) @ immutable ->
      {u : unit | at_level h p === Finite 2 &&
        unified h p q true after (Lowering (1, edits, tree, rest))} ->
      {u : unit | false} @ ghost = fun h p q after edits tree rest premise -> ghost_ (
    let refine_ premise = premise in let d = Lowering (1, edits, tree, rest) in
    let ok = true in unified_def h p q ok after d; let u = () in refine_ u)
end;;
[%%expect{|
module Wrong_binding_level :
  sig
    val bad :
      (h : Copy_spec.node Pref.heap) @ immutable ->
      (p : Copy_spec.node Pref.t) @ immutable ->
      (q : Copy_spec.node Pref.t) @ immutable ->
      (after : Copy_spec.node Pref.heap) @ immutable ->
      (edits : Level_spec.lowering) @ immutable ->
      (tree : Level_spec.bounded) @ immutable ->
      (rest : Level_unifier_spec.derivation) @ immutable ->
      {u : unit
        | ((Level_spec.at_level h p) === (Copy_spec.Finite 2)) &&
            (Level_unifier_spec.unified h p q true after
               (Level_unifier_spec.Lowering (1, edits, tree, rest)))} ->
      {u : unit | false} @ ghost
  end
|}]

open Generalize_spec;;
open Provenance_spec;;
[%%expect{|
|}]

module Invent_saved_root = struct
  let bad : (saved : node Pref.heap) @ immutable -> (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> {u : unit | not (H.mem saved p)} ->
      {u : unit | originates saved h 1 p (Origin (p, Stop))} @ ghost =
    fun saved h p premise -> ghost_ (
      let refine_ premise = premise in let path = Stop in let o = Origin (p, path) in
      originates_def saved h 1 p o; below_def saved p 1;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 20-29:
8 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
