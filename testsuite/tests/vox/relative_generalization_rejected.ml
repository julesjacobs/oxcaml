(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml generalize_spec.ml provenance_spec.ml";
 readonly_files = "relative_generalization_rejected.ml";
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

module Change_boundary = struct
  let bad : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | below h x 1 && not (rho x === eta x)} ->
      {u : unit | interpret rho eta (Boundary x) === eta x} @ ghost =
    fun h x rho eta premise -> ghost_ (
      let refine_ premise = premise in let s = Boundary x in
      interpret_def rho eta s; let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 45-54:
9 |       interpret_def rho eta s; let u = () in refine_ u)
                                                 ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generalize_low_variable = struct
  let bad : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      {u : unit | H.at h x === Some (cell Var 1)} ->
      {u : unit | scheme h 1 (Tip x) === Parameter x} @ ghost =
    fun h x premise -> ghost_ (
      let refine_ premise = premise in let t = Tip x in
      let desc : desc = Var in cell_def desc 1; at_level_def h x; let level = Finite 1 in close_level_def 1 level;
      scheme_def h 1 t; bound_root_def t; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 56-65:
8 |       scheme_def h 1 t; bound_root_def t; let u = () in refine_ u)
                                                            ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Forget_model_agreement = struct
  let bad : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | H.mem h x && H.at h x === Some (cell Var 1)} ->
      {u : unit | rho x === eta x} @ ghost = fun h x rho eta premise -> ghost_ (
    let refine_ premise = premise in let desc : desc = Var in cell_def desc 1;
    equation_def h rho x; equation_def h eta x; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 62-71:
8 |     equation_def h rho x; equation_def h eta x; let u = () in refine_ u)
                                                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
