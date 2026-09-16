(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_execution_spec.ml";
 readonly_files = "hm_execution_rejected.ml";
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
open Hm_environment_spec;;
open Hm_execution_spec;;
[%%expect{|
|}]

module Skip_boolean_allocation = struct
  let bad : (p : node Pref.t) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
      (RBool p) (H.empty ()) Generalize_spec.Empty} @ ghost = fun p -> ghost_ (
    let h = H.empty () in let pool : pool = Generalize_spec.Empty in
    let env : env = Hm_environment_spec.Empty in let e = RBool p in
    ran_def h 0 pool env e h pool; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 49-58:
7 |     ran_def h 0 pool env e h pool; let u = () in refine_ u)
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Return_result_after_failure = struct
  let bad : (p : node Pref.t) @ immutable -> (left : execution) @ immutable ->
    {u : unit | result (RApp_left (left, D.Truth)) === Some p} @ ghost = fun p left -> ghost_ (
    let e = RApp_left (left, D.Truth) in result_def e; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 69-78:
4 |     let e = RApp_left (left, D.Truth) in result_def e; let u = () in refine_ u)
                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
