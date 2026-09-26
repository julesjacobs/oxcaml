(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml level_spec.ml lower_locality_spec.ml level_proofs.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml generalize_spec.ml generalize_proofs.ml level_finite_spec.ml pooled_spec.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml structure_spec.ml copy_cleanup_spec.ml nested_pool_spec.ml representative_level.ml representative_pool_spec.ml effective_level.ml representative_certificate.ml copy_certificate_spec.ml hm_type_proofs.ml terminal_lower_spec.ml effective_compression_spec.ml effective_unifier_spec.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_readback_runtime.ml hm_annotation_snapshot.ml hm_annotation_snapshot_demo.ml";
 { bytecode; }
*)
module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module F = Level_finite_spec
module U = Level_unifier_spec
module H = Pref.Heap
module Snapshot = Hm_annotation_snapshot

let () =
  let state = Pref.empty () in
  let node = Copy_spec.cell Copy_spec.Bool 0 in
  ghost_ (Copy_spec.cell_def Copy_spec.Bool 0);
  let allocated = Pref.alloc node state in
  let p = allocated.value in
  let h = ghost_ (Pref.own (borrow_ allocated.state)) in
  let literal = A.Boolean_literal p in
  let application = A.Application (Some p, literal, literal) in
  let trace = A.Let_binding (literal, application) in
  ghost_ (S.owned_def h literal; S.option_owned_def h (Some p);
    S.owned_def h application; S.owned_def h trace);
  let trees : ((x : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === x &&
        (if H.mem h x then F.finite h t else U.observe h x === None)}
      @ immutable) @ total ghost = ghost_ (fun x ->
    let tree = if x === p then F.Constant_tree x else F.Free x in
    F.tree_root_def tree; F.finite_def h tree; U.observe_def h x;
    tree) in
  let snapshot = Snapshot.collect h trees trace (borrow_ allocated.state)
    Snapshot.Empty in
  ghost_ (A.contains_def p literal; A.contains_def p application;
    A.contains_def p trace; A.option_contains_def p (Some p);
    Snapshot.record_lookup h trees p trace Snapshot.Empty;
    let tree = trees p in
    F.finite_def h tree; F.tree_root_def tree; F.readback_def tree;
    U.observe_def h p);
  let result : {ty : Copy_spec.ty option | ty === Some Copy_spec.Boolean} @ immutable =
    Snapshot.lookup p snapshot in
  match result with
  | Some Copy_spec.Boolean -> ()
  | _ -> failwith "wrong annotation snapshot"
