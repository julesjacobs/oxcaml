(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml copy_cleanup_spec.ml";
 readonly_files = "copy_cleanup_rejected.ml";
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
open Copy_cleanup_spec;;
[%%expect{|
|}]

module Forget_cleanup = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (epoch : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some
        {desc = Var; level = Generic; memo = Memo (epoch, q)}} ->
      {u : unit | swept_at h h (Entry (p, Empty)) p} @ ghost =
    fun h p epoch q premise -> ghost_ (
      let refine_ premise = premise in let trail = Entry (p, Empty) in
      swept_at_def h h trail p; listed_def trail p;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 20-29:
10 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Change_descriptor = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (cell Var 0)} ->
      {u : unit | swept_at h (H.put h p (cell Bool 0)) (Entry (p, Empty)) p} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let trail = Entry (p, Empty) in
      let a : desc = Var in let b : desc = Bool in cell_def a 0; cell_def b 0;
      let after = H.put h p (cell b 0) in swept_at_def h after trail p; listed_def trail p;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 20-29:
9 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
