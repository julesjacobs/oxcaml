(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_table_bindings.ml vox_table_bindings_bridge.ml vox_table_implementation.ml vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml vox_egraph_rule_apply.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_evidence.ml vox_egraph_match_subst.ml vox_egraph_match_intro.ml vox_egraph_match_bindings.ml vox_egraph_binding_frame.ml vox_egraph_match_store_intro.ml vox_egraph_pattern_admit.ml vox_egraph_rule_rewrite.ml vox_egraph_closure_spec.ml vox_egraph_quantifier.mli vox_egraph_quantifier_measure.ml vox_egraph_quantifier.ml vox_egraph_saturation_spec.ml vox_egraph_saturation_proof.ml vox_egraph_assignment_spec.ml vox_egraph_assignments.ml vox_egraph_assignment_proof.ml vox_egraph_rule_scan.ml egraph_rule_scan.ml";
 { native; }
*)
module H = Vox_egraph_rule_hashcons
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec
module Scan = Vox_egraph_rule_scan
module C = Vox_egraph_saturation_spec
module P = Vox_egraph_match_evidence

let () =
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  ghost_ (
    R.lookup_rule_def rules 0;
    R.rule_valid_def rule;
    R.pat_sort_def [] rule.lhs;
    R.pat_sort_def [] rule.rhs;
    R.vars_in_def rule.rhs rule.lhs);
  let state = H.create rules in
  let #{H.value = _; state} = H.admit_expr state (L.Int_lit 0) in
  let #{Scan.status; fuel = _; state} = Scan.rule state rules 0 rule 1 in
  assert (status = Scan.Work_limit);
  let #{Scan.status; fuel = _; state} = Scan.rule state rules 0 rule 1000 in
  assert (status = Scan.Changed);
  let #{Scan.status; fuel; state} = Scan.rule state rules 0 rule 1000 in
  assert (status = Scan.Stable && 0 <= fuel && fuel < 1000);
  let {H.owner = _; store} = state in
  ghost_ (
    match status with
    | Scan.Stable ->
      let _ : {u : unit | C.closed_rule (P.view store) rule} = () in ()
    | _ -> ());
  ()
