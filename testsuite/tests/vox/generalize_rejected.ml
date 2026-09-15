(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml generalize_spec.ml";
 readonly_files = "generalize_rejected.ml";
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
open Generalize_spec;;
[%%expect{|
|}]

module Missing_pool_entry = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (cell Var 2)} ->
      {u : unit | covered h 0 Empty p} @ ghost = fun h p premise -> ghost_ (
    let refine_ premise = premise in let desc = Var in cell_def desc 2;
    let pool = Empty in listed_def pool p; covered_def h 0 pool p; at_level_def h p;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 18-27:
7 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generalize_equal_level = struct
  let bad : unit -> {u : unit | close_level 2 (Finite 2) === Generic} @ ghost = fun () -> ghost_ (
    let level = Finite 2 in close_level_def 2 level; let u = () in refine_ u)
end;;
[%%expect{|
Line 3, characters 67-76:
3 |     let level = Finite 2 in close_level_def 2 level; let u = () in refine_ u)
                                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generalize_environment = struct
  let bad : unit -> {u : unit | close_level 1 (Finite 0) === Generic} @ ghost = fun () -> ghost_ (
    let level = Finite 0 in close_level_def 1 level; let u = () in refine_ u)
end;;
[%%expect{|
Line 3, characters 67-76:
3 |     let level = Finite 0 in close_level_def 1 level; let u = () in refine_ u)
                                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Escaping_child = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.at h p === Some (cell (Link q) 0) && H.at h q === Some (cell Var 3)} ->
      {u : unit | ordered h p} @ ghost = fun h p q premise -> ghost_ (
    let refine_ premise = premise in let desc = Link q in cell_def desc 0;
    let var = Var in cell_def var 3; ordered_def h p; children_below_def h desc 0;
    below_def h q 0; at_level_def h q; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 53-62:
7 |     below_def h q 0; at_level_def h q; let u = () in refine_ u)
                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Invent_pool_ownership = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h p)} -> {u : unit | pool_scoped h (Entry (p, Empty))} @ ghost = fun h p premise -> ghost_ (
    let refine_ premise = premise in let pool = Entry (p, Empty) in pool_scoped_def h pool; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 106-115:
4 |     let refine_ premise = premise in let pool = Entry (p, Empty) in pool_scoped_def h pool; let u = () in refine_ u)
                                                                                                              ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
