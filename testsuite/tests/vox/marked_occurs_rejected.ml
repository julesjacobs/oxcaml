(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml ";
 readonly_files = "marked_occurs_rejected.ml";
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
open Level_unifier_spec;;
[%%expect{|
|}]

module Invent_cached_search = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (cell Var 0)} ->
      {u : unit | marks_valid h p (Marked (No_marks, p, cell Var 0, Hit))} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let old = cell Var 0 in
      let empty = No_marks in let search = Hit in
      let d = Marked (empty, p, old, search) in
      marks_valid_def h p d; searched_def h p p false search;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 20-29:
10 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Leave_mark_set = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (cell Var 0)} ->
      {u : unit | H.at h p === H.at (H.put h p (set_visited (cell Var 0) true)) p} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let desc : desc = Var in cell_def desc 0;
      let old = cell desc 0 in set_visited_def old true;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 20-29:
8 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Preexisting_mark = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.at h p === Some (set_visited (cell Var 0) true)} ->
      {u : unit | match H.at h p with None -> true | Some v -> not v.visited} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let desc : desc = Var in
      let old = cell desc 0 in set_visited_def old true;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 20-29:
8 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
