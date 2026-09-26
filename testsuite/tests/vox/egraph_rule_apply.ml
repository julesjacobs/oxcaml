(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml vox_egraph_rule_apply.ml egraph_rule_apply.ml";
 { bytecode; }
 { native; }
*)

module H = Vox_egraph_rule_hashcons
module A = Vox_egraph_rule_apply
module Q = Vox_egraph_rule_query
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec
module E = Vox_egraph_derivation_spec

let () =
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  let state = H.create rules in
  let #{A.status; merged; state; left = _; right = _} =
    A.instance state rules 0 [] in
  assert (status = A.Applied && merged);
  let #{Q.status; state; proof} =
    Q.expressions state (L.Int_lit 0) (L.Int_lit 1) in
  assert (status = Q.Equal);
  ghost_ (
    match proof with
    | Some proof ->
      let _ : {u : unit | E.valid rules proof &&
        E.left proof === L.Int_lit 0 && E.right proof === L.Int_lit 1} =
        () in ()
    | None -> ());
  let #{A.status; merged; state; left = _; right = _} =
    A.instance state rules 0 [] in
  assert (status = A.Applied && not merged);
  let #{A.status; state = _; merged = _; left = _; right = _} =
    A.instance state rules 1 [] in
  assert (status = A.Invalid_rule)

let () =
  let rule = {R.vars = [L.Integer];
    lhs = R.Add (R.Var 0, R.Int_lit 0); rhs = R.Var 0} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  let state = H.create rules in
  let source = L.Add (L.Int_input, L.Int_lit 0) in
  let #{Q.status; state; proof = _} =
    Q.expressions state source L.Int_input in
  assert (status = Q.Not_proved);
  let #{A.status; merged; state; left = _; right = _} =
    A.instance state rules 0 [L.Int_input] in
  assert (status = A.Applied && merged);
  let #{Q.status; state; proof = _} =
    Q.expressions state source L.Int_input in
  assert (status = Q.Equal);
  let #{A.status; state = _; merged = _; left = _; right = _} =
    A.instance state rules 0 [L.Bool_input] in
  assert (status = A.Invalid_rule)
