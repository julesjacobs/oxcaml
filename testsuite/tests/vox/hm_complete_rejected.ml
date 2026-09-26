(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml level_mgu_spec.ml hm_declarative.ml";
 readonly_files = "hm_complete_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)
open Hm_declarative;;
[%%expect{|
|}]

module Identity_is_boolean = struct
  let bad : (d : typing) @ immutable ->
      {u : unit | typed Z Empty_context (Lambda (Bound Z)) Boolean d} @ ghost = fun d -> ghost_ (
    let z = Z in let env = Empty_context in let e = Lambda (Bound z) in let t = Boolean in
    typed_def z env e t d; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 41-50:
5 |     typed_def z env e t d; let u = () in refine_ u)
                                             ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Specialization_is_principal = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
      (delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | Copy_spec.Function (Copy_spec.Variable p, Copy_spec.Variable p) ===
        Level_mgu_spec.substitute delta (Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean))} @ ghost = fun p delta -> ghost_ (
    let b = Copy_spec.Boolean in let t = Copy_spec.Function (b, b) in
    Level_mgu_spec.substitute_def delta t; Level_mgu_spec.substitute_def delta b;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Reuse_identity_copy = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
      (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | rho p === Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean)} ->
      {u : unit | rho p === Copy_spec.Function
        (Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean), Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean))} @ ghost =
    fun _p _rho assigned -> ghost_ (let refine_ assigned = assigned in let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 85-94:
7 |     fun _p _rho assigned -> ghost_ (let refine_ assigned = assigned in let u = () in refine_ u)
                                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generalize_outer_binding = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | open_scheme (Forall (Z, Free p)) No_arguments === Boolean} @ ghost = fun p -> ghost_ (
    let z = Z in let t = Free p in let s = Forall (z, t) in
    let args = No_arguments in open_scheme_def s args; open_type_def args t;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 18-27:
6 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
