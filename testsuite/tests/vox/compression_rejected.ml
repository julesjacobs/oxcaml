(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml compression_spec.ml";
 readonly_files = "compression_rejected.ml";
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
open Level_unifier_spec;;
open Compression_spec;;
[%%expect{|
|}]

module Unresolved_redirect = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (q : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
      {u : unit | active h p && observe h p === Some (Link q)} ->
      {u : unit | rewritten h (H.put h p (redirect h p r)) (Write (p, q, r, Here, Done))} @ ghost =
    fun h p q r premise -> ghost_ (
      let refine_ premise = premise in let d = Write (p, q, r, Here, Done) in
      let after = H.put h p (redirect h p r) in rewritten_def h after d;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 20-29:
9 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generic_redirect = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (q : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
      (path : resolution) @ immutable ->
      {u : unit | at_level h p === Generic && observe h p === Some (Link q) && resolves h p r path} ->
      {u : unit | rewritten h (H.put h p (redirect h p r)) (Write (p, q, r, path, Done))} @ ghost =
    fun h p q r path premise -> ghost_ (
      let refine_ premise = premise in let d = Write (p, q, r, path, Done) in
      let after = H.put h p (redirect h p r) in rewritten_def h after d; active_def h p;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 20-29:
10 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
