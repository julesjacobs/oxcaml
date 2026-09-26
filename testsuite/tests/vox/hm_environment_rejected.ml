(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml";
 readonly_files = "hm_environment_rejected.ml";
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
open Hm_environment_spec;;
[%%expect{|
|}]

module Rewrite_generic_descriptor = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | H.at h p === Some {desc = Var; level = Generic; memo = Empty_memo; visited = false}} ->
    {u : unit | protected_at h
      (H.put h p {desc = Bool; level = Generic; memo = Empty_memo; visited = false}) 0 p} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in
      let v : node = {desc = Bool; level = Generic; memo = Empty_memo; visited = false} in
      let after = H.put h p v in protected_at_def h after 0 p;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 20-29:
10 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generalize_protected_boundary = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some (cell Var 0)} ->
    {u : unit | protected_at h
      (H.put h p {desc = Var; level = Generic; memo = Empty_memo; visited = false}) 0 p} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let desc : desc = Var in cell_def desc 0;
      let v : node = {desc; level = Generic; memo = Empty_memo; visited = false} in
      let after = H.put h p v in protected_at_def h after 0 p;
      below_def h p 0; below_def after p 0; at_level_def h p; at_level_def after p;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 11, characters 20-29:
11 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
