(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml generalize_spec.ml pooled_spec.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_declarative.ml hm_environment_spec.ml hm_execution_spec.ml";
 readonly_files = "hm_model_rejected.ml";
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
open Level_finite_spec;;
[%%expect{|
|}]

module Cyclic_readback = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {t : tree | finite (H.put h p (cell (Link p) 0)) t && tree_root t === p} @ immutable ghost = fun h p -> ghost_ (
    let desc = Link p in let v = cell desc 0 in cell_def desc 0;
    let after = H.put h p v in let t = Free p in
    finite_def after t; tree_root_def t; observe_def after p; refine_ t)
end;;
[%%expect{|
Line 6, characters 62-71:
6 |     finite_def after t; tree_root_def t; observe_def after p; refine_ t)
                                                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unconstrained_binding = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (q : node Pref.t) @ immutable ->
      (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | node_equation (H.put h p (cell (Link q) 0)) rho p} @ ghost = fun h p q rho -> ghost_ (
    let desc = Link q in let v = cell desc 0 in cell_def desc 0;
    let after = H.put h p v in observe_def after p; node_equation_def after rho p;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
