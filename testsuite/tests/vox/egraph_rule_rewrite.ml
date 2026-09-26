(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_table_bindings.ml vox_table_bindings_bridge.ml vox_table_implementation.ml vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml vox_egraph_rule_apply.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_evidence.ml vox_egraph_match_subst.ml vox_egraph_match_intro.ml vox_egraph_match_bindings.ml vox_egraph_binding_frame.ml vox_egraph_match_store_intro.ml vox_egraph_pattern_admit.ml vox_egraph_rule_rewrite.ml egraph_rule_rewrite.ml";
 { bytecode; }
 { native; }
*)
module H = Vox_egraph_rule_hashcons
module A = Vox_egraph_rule_apply
module W = Vox_egraph_rule_rewrite
module Q = Vox_egraph_rule_query
module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec

let () =
  let equation = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  let collapse = {R.vars = [L.Integer; L.Boolean];
    lhs = R.Add (R.Var 0, R.Var 0); rhs = R.Var 0} in
  let rules = R.Rule_cons (equation, R.Rule_cons (collapse, R.No_rules)) in
  let state = H.create rules in
  let #{A.status; state; left = zero; right = _; merged = _} =
    A.instance state rules 0 [] in
  assert (status = A.Applied);
  let expr = L.Add (L.Int_lit 0, L.Int_lit 1) in
  let #{H.value; state} = H.admit_expr state expr in
  match value with
  | None -> assert false
  | Some root ->
    let #{W.status; state; merged = _; left = _; right = _} =
      W.matched state rules 1 [zero; -1] (-1) in
    assert (status = W.No_match);
    let #{W.status; state; merged; left = _; right = _} =
      W.matched state rules 1 [zero; -1] root in
    assert (status = W.Applied && merged);
    let #{Q.status; state; proof = _} = Q.expressions state expr (L.Int_lit 0) in
    assert (status = Q.Equal);
    let #{W.status; state; merged; left = _; right = _} =
      W.matched state rules 1 [zero; -1] root in
    assert (status = W.Applied && not merged);
    let #{W.status; state = _; merged = _; left = _; right = _} =
      W.matched state rules 1 [zero] root in
    assert (status = W.Invalid_bindings)

let () =
  let rule = {R.vars = []; lhs = R.Int_input;
    rhs = R.Add (R.Int_input, R.Int_lit 0)} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  let state = H.create rules in
  let #{H.value; state} = H.admit_expr state L.Int_input in
  match value with
  | None -> assert false
  | Some root ->
    let #{W.status; state; merged; left = _; right = _} =
      W.matched state rules 0 [] root in
    assert (status = W.Applied && merged);
    let before = (let view = borrow_ state in view.owner.count) in
    let #{W.status; state; merged; left = _; right = _} =
      W.matched state rules 0 [] root in
    assert (status = W.Applied && not merged);
    let after = (let view = borrow_ state in view.owner.count) in
    assert (before = after);
    let {H.owner; store} = state in
    ghost_ (
      R.lookup_rule_def rules 0;
      if after = before && not merged && status === W.Applied then
        let _ : {u : unit |
          Vox_egraph_match_spec.matches
            (Vox_egraph_match_evidence.view store) rule.rhs [] root} =
          () in ());
    let state = {H.owner; store} in
    let #{Q.status; state = _; proof = _} =
      Q.expressions state L.Int_input (L.Add (L.Int_input, L.Int_lit 0)) in
    assert (status = Q.Equal)
