(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml level_spec.ml lower_locality_spec.ml level_proofs.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml generalize_spec.ml generalize_proofs.ml level_finite_spec.ml pooled_spec.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml structure_spec.ml copy_cleanup_spec.ml nested_pool_spec.ml representative_level.ml representative_pool_spec.ml effective_level.ml representative_certificate.ml copy_certificate_spec.ml terminal_lower_spec.ml effective_compression_spec.ml effective_unifier_spec.ml hm_primitive_constraints.ml hm_type_proofs.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_annotation_trace_demo.ml";
 { bytecode; }
 { native; }
*)
module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module E = Hm_effective_execution_spec

let (literal @ total) (p : Copy_spec.node Pref.t @ immutable) :
    {u : unit | A.root (A.Boolean_literal p) === E.result (E.RBool p)}
    @ ghost = ghost_ (
  let trace = A.Boolean_literal p in
  let execution = E.RBool p in
  S.records_def trace execution;
  S.root_agrees trace execution ())

let () = ()
