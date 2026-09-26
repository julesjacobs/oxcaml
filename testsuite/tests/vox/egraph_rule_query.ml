(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml egraph_rule_query.ml";
 { bytecode; }
 { native; }
*)

module H = Vox_egraph_rule_hashcons
module Q = Vox_egraph_rule_query
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec
module E = Vox_egraph_derivation_spec

let () =
  let state = H.create R.No_rules in
  let source = L.Add (L.Int_input, L.Int_lit 0) in
  let #{Q.status; state; proof} = Q.expressions state source source in
  assert (status = Q.Equal);
  ghost_ (
    match proof with
    | Some proof ->
      let _ : {u : unit | E.valid R.No_rules proof &&
        E.left proof === source && E.right proof === source} = () in
      ()
    | None -> ());
  let #{Q.status; state; proof = _} =
    Q.expressions state source (L.Bool_lit true) in
  assert (status = Q.Not_proved);
  let #{Q.status; state; proof = _} =
    Q.expressions state (L.Add (L.Bool_lit true, L.Int_lit 0)) source in
  assert (status = Q.Invalid_input);
  let rec tree n =
    if n <= 0 then L.Int_input
    else L.Add (L.Int_lit n, tree (n - 1)) in
  let large = tree 300 in
  let #{Q.status; state = _; proof = _} =
    Q.expressions state large large in
  assert (status = Q.Node_limit)
