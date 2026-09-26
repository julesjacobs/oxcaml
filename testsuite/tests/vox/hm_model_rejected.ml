(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_execution_spec.ml";
 readonly_files = "hm_model_rejected.ml";
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
[%%expect{|
|}]

module Cyclic_readback = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
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
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
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
