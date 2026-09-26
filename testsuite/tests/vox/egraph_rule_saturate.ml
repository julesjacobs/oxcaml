(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_table_bindings.ml vox_table_bindings_bridge.ml vox_table_implementation.ml vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml vox_egraph_rule_apply.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_evidence.ml vox_egraph_match_subst.ml vox_egraph_match_intro.ml vox_egraph_match_bindings.ml vox_egraph_binding_frame.ml vox_egraph_match_store_intro.ml vox_egraph_pattern_admit.ml vox_egraph_rule_rewrite.ml vox_egraph_closure_spec.ml vox_egraph_quantifier.mli vox_egraph_quantifier_measure.ml vox_egraph_quantifier.ml vox_egraph_saturation_spec.ml vox_egraph_saturation_proof.ml vox_egraph_assignment_spec.ml vox_egraph_assignments.ml vox_egraph_assignment_proof.ml vox_egraph_rule_scan.ml vox_egraph_rule_cursor.ml vox_egraph_rules_scan.ml vox_egraph_congruence_spec.ml vox_egraph_congruence_proof.ml vox_egraph_fixedpoint_spec.ml vox_egraph_rule_saturate.ml egraph_rule_rewrite.ml egraph_rule_saturate.ml";
 { native; }
*)
module H = Vox_egraph_rule_hashcons
module Q = Vox_egraph_rule_query
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec
module Sat = Vox_egraph_rule_saturate
module F = Vox_egraph_fixedpoint_spec
module P = Vox_egraph_match_evidence

let () =
  let zero = {R.vars = [L.Integer]; lhs = R.Add (R.Var 0, R.Int_lit 0); rhs = R.Var 0} in
  let equal = {R.vars = [L.Integer]; lhs = R.Eq_int (R.Var 0, R.Var 0); rhs = R.Bool_lit true} in
  let rules = R.Rule_cons (zero, R.Rule_cons (equal, R.No_rules)) in
  if not (R.valid rules) then assert false else
  let state = H.create rules in
  let expr = L.Eq_int (L.Add (L.Add (L.Int_input, L.Int_lit 0), L.Int_lit 0), L.Int_input) in
  let #{Q.status; state; proof = _} = Q.expressions state expr (L.Bool_lit true) in
  assert (status = Q.Not_proved);
  let #{Sat.status; fuel; state} = Sat.saturate state rules 20 20 100000 in
  assert (status = Sat.Fixed_point && fuel >= 0);
  let {H.owner; store} = state in
  ghost_ (
    match status with
    | Sat.Fixed_point -> let _ : {u : unit | F.fixed (P.view store) rules} = () in ()
    | _ -> ());
  let state = {H.owner; store} in
  let #{Q.status; state; proof = _} = Q.expressions state expr (L.Bool_lit true) in
  assert (status = Q.Equal);
  let #{Sat.status; fuel = _; state} = Sat.saturate state rules 0 20 100000 in
  assert (status = Sat.Round_limit);
  let #{Sat.status; fuel = _; state} = Sat.saturate state rules 20 0 100000 in
  assert (status = Sat.Rebuild_limit);
  let #{Sat.status; fuel; state = _} = Sat.saturate state rules 20 20 0 in
  assert (status = Sat.Search_limit && fuel = 0)

let () =
  let grow = {R.vars = [L.Integer]; lhs = R.Var 0; rhs = R.Add (R.Var 0, R.Var 0)} in
  let rules = R.Rule_cons (grow, R.No_rules) in
  if not (R.valid rules) then assert false else
  let state = H.create rules in
  let #{H.value = _; state} = H.admit_expr state (L.Int_lit 0) in
  let #{Sat.status; fuel = _; state} = Sat.saturate state rules 10 10 10000 in
  assert (status = Sat.Fixed_point);
  let count = (let view = borrow_ state in view.owner.count) in
  assert (count = 2);
  let #{Q.status; state = _; proof = _} =
    Q.expressions state (L.Int_lit 0) (L.Add (L.Int_lit 0, L.Int_lit 0)) in
  assert (status = Q.Equal)
