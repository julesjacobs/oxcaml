(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml";
 readonly_files = "level_mgu_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

open Copy_spec;;
open Level_unifier_spec;;
open Level_finite_spec;;
open Level_mgu_spec;;
open Level_mgu_proofs;;
[%%expect{|
|}]

module Merged_variables = struct
  let bad : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | not (p === q)} ->
      {u : unit | readback (Free p) === readback (Free q)} @ ghost =
    fun p q premise -> ghost_ (
      let refine_ premise = premise in
      let a = Free p in let b = Free q in readback_def a; readback_def b;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 20-29:
8 |       let u = () in refine_ u)
                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Over_specialized = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && observe h p === Some Var} ->
      {u : unit | readback (Free p) === Boolean} @ ghost = fun h p premise -> ghost_ (
    let refine_ premise = premise in
    let t = Free p in readback_def t;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 18-27:
7 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Omitted_substitution_child = struct
  let bad : (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | delta p === Boolean && delta q === Boolean} ->
      {u : unit | substitute delta (Function (Variable p, Variable q)) === Function (Boolean, Variable q)}
      @ ghost = fun delta p q premise -> ghost_ (
    let refine_ premise = premise in
    let a = Variable p in let b = Variable q in let t = Function (a, b) in
    substitute_def delta a; substitute_def delta b; substitute_def delta t;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 10, characters 18-27:
10 |     let u = () in refine_ u)
                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_operand_equality = struct
  let bad :
      (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
      (trees : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x &&
          (if H.mem after x then finite after t else observe after x === None)} @ immutable)) @ total ->
      (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (normal : ((x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = trees x in normalizes after sigma x t})) @ total ->
      (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
      (x : node Pref.t) @ immutable ->
      {u : unit | unified h p q true after d} ->
      {u : unit | rho x === substitute rho (sigma x)} @ ghost =
    fun h p q after d trees sigma normal rho model x premise -> ghost_ (
      let refine_ premise = premise in let u = () in
      mgu_factor_at h p q after d trees sigma normal rho model x (refine_ u))
end;;
[%%expect{|
Line 19, characters 65-76:
19 |       mgu_factor_at h p q after d trees sigma normal rho model x (refine_ u))
                                                                      ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
