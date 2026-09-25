(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml generalize_spec.ml level_unifier_spec.ml level_finite_spec.ml pooled_spec.ml nested_pool_spec.ml";
 readonly_files = "nested_pool_rejected.ml";
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
open Nested_pool_spec;;
[%%expect{|
|}]

module Drop_lowered = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | retained h p} ->
    {u : unit | transfer h (Entry (p, Empty)) Empty === Empty} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let empty = Empty in
      let one = Entry (p, empty) in transfer_def h one empty;
      transfer_def h empty one; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 46-55:
8 |       transfer_def h empty one; let u = () in refine_ u)
                                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Keep_generic = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | not (retained h p)} ->
    {u : unit | transfer h (Entry (p, Empty)) Empty === Entry (p, Empty)} @ ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in let empty = Empty in
      let one = Entry (p, empty) in transfer_def h one empty;
      transfer_def h empty empty; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 48-57:
8 |       transfer_def h empty empty; let u = () in refine_ u)
                                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Lose_parent = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | transfer h Empty (Entry (p, Empty)) === Empty} @ ghost =
    fun h p -> ghost_ (
      let empty = Empty in let one = Entry (p, empty) in
      transfer_def h empty one; let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 46-55:
6 |       transfer_def h empty one; let u = () in refine_ u)
                                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
