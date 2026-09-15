(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml generalize_spec.ml level_unifier_spec.ml level_finite_spec.ml pooled_spec.ml";
 readonly_files = "pooled_rejected.ml";
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
open Level_unifier_spec;;
open Level_finite_spec;;
open Pooled_spec;;
[%%expect{|
|}]

module Omit_session = struct
  let bad : (epoch : node Pref.t) @ immutable -> {u : unit | registered Empty epoch Start === Empty} @ ghost = fun epoch -> ghost_ (
    let base = Empty in let d = Start in registered_def base epoch d; let u = () in refine_ u)
end;;
[%%expect{|
Line 3, characters 84-93:
3 |     let base = Empty in let d = Start in registered_def base epoch d; let u = () in refine_ u)
                                                                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Omit_copy = struct
  let bad : (epoch : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (old : node) @ immutable ->
      {u : unit | registered Empty epoch (Fresh (Start, p, q, old, Var)) === Entry (epoch, Empty)} @ ghost = fun epoch p q old -> ghost_ (
    let base = Empty in let start = Start in let d = Fresh (start, p, q, old, Var) in
    registered_def base epoch start; registered_def base epoch d; let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 80-89:
6 |     registered_def base epoch start; registered_def base epoch d; let u = () in refine_ u)
                                                                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Invent_link_allocation = struct
  let bad : (epoch : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (old : node) @ immutable ->
      {u : unit | registered Empty epoch (Alias (Start, p, q, old)) === Entry (q, Entry (epoch, Empty))} @ ghost = fun epoch p q old -> ghost_ (
    let base = Empty in let start = Start in let d = Alias (start, p, q, old) in
    registered_def base epoch start; registered_def base epoch d; let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 80-89:
6 |     registered_def base epoch start; registered_def base epoch d; let u = () in refine_ u)
                                                                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Stale_variable_tree = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | observe h p === Some (Link q)} -> {u : unit | finite h (Free p)} @ ghost = fun h p q premise -> ghost_ (
    let refine_ premise = premise in let t = Free p in tree_root_def t; finite_def h t; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 102-111:
4 |     let refine_ premise = premise in let t = Free p in tree_root_def t; finite_def h t; let u = () in refine_ u)
                                                                                                          ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Finite_self_cycle = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | observe h p === Some (Link p)} -> {u : unit | finite h (Alias_tree (p, Free p))} @ ghost = fun h p premise -> ghost_ (
    let refine_ premise = premise in let c = Free p in let t = Alias_tree (p, c) in
    tree_root_def t; tree_root_def c; finite_def h t; finite_def h c; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 84-93:
5 |     tree_root_def t; tree_root_def c; finite_def h t; finite_def h c; let u = () in refine_ u)
                                                                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Stale_allocation_coverage = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | covered (H.put h p (cell Var 2)) 0 Empty p} @ ghost = fun h p -> ghost_ (
    let desc = Var in cell_def desc 2; let after = H.put h p (cell desc 2) in let pool = Empty in
    covered_def after 0 pool p; listed_def pool p; at_level_def after p; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 87-96:
5 |     covered_def after 0 pool p; listed_def pool p; at_level_def after p; let u = () in refine_ u)
                                                                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
