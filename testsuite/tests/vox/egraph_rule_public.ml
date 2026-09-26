(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml vox_table_model.ml vox_table_model_proofs.ml vox_table_bits.ml vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml vox_table_map.ml vox_table_invariant.ml vox_table_initial.ml vox_table_update_proofs.ml vox_table_insert_proofs.ml vox_table_migration_proofs.ml vox_table_read_proofs.ml vox_table_search_spec.ml vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_table_storage.mli vox_table_storage.ml vox_table_search.ml vox_table_mutation.ml vox_table_coverage.ml vox_table_occupancy.ml vox_table_vacancy_progress.ml vox_table_vacancy.ml vox_table_insert.ml vox_table_migrate.ml vox_table_resize.ml vox_verified_flat_hashtbl.mli vox_table_bindings.ml vox_table_bindings_bridge.ml vox_table_implementation.ml vox_verified_flat_hashtbl.ml vox_egraph_key.ml vox_egraph_arena_spec.ml vox_egraph_index_spec.ml vox_egraph_owner.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml vox_egraph_rule_query.ml vox_egraph_rule_apply.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_evidence.ml vox_egraph_match_subst.ml vox_egraph_match_intro.ml vox_egraph_match_bindings.ml vox_egraph_binding_frame.ml vox_egraph_match_store_intro.ml vox_egraph_pattern_admit.ml vox_egraph_rule_rewrite.ml vox_egraph_closure_spec.ml vox_egraph_quantifier.mli vox_egraph_quantifier_measure.ml vox_egraph_quantifier.ml vox_egraph_saturation_spec.ml vox_egraph_saturation_proof.ml vox_egraph_assignment_spec.ml vox_egraph_assignments.ml vox_egraph_assignment_proof.ml vox_egraph_rule_scan.ml vox_egraph_rule_cursor.ml vox_egraph_rules_scan.ml vox_egraph_congruence_spec.ml vox_egraph_congruence_proof.ml vox_egraph_fixedpoint_spec.ml vox_egraph_rule_saturate.ml vox_egraph_snapshot_spec.ml vox_egraph_snapshot_proof.ml vox_egraph_preservation_spec.ml vox_egraph_preservation_proof.ml vox_egraph_model_evidence.ml vox_egraph_rule_handle.mli vox_egraph_rule_handle.ml vox_egraph_interpret_wrapping.mli vox_egraph_interpret_wrapping.ml egraph_rule_public.ml";
 { bytecode; }
*)
module G = Vox_egraph_rule_handle
module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module E = Vox_egraph_derivation_spec
module F = Vox_egraph_fixedpoint_spec
module Snapshot = Vox_egraph_snapshot_spec
module Interpret = Vox_egraph_interpret_wrapping

let interpreted_query :
    (input_rules : {rs : R.t | R.valid rs}) @ immutable ->
    (left : L.expr) @ immutable -> (right : L.expr) @ immutable ->
    (env : L.env) @ immutable ghost ->
    ((index : int) ->
      ((rule : R.rule) @ immutable ->
        ((subst : R.subst) @ immutable ->
          ({u : unit | R.lookup_rule input_rules index === Some rule &&
            R.rule_valid rule && R.subst_valid rule.vars subst} ->
           {u : unit | L.eval (R.instantiate rule.lhs subst) env ===
             L.eval (R.instantiate rule.rhs subst) env} @ ghost) @ total) @ total) @ total) @ total ghost ->
    {proved : bool | if proved then L.eval left env === L.eval right env else true} =
  fun input_rules left right env interpretation ->
    let state = G.create input_rules in
    let #{G.id = _; state} = G.admit state left in
    let #{G.id = _; state} = G.admit state right in
    let #{G.status = _; fuel = _; state} = G.saturate state 20 20 100000 in
    let #{G.status; state = _; proof} = G.query state left right in
    match status with
    | G.Equal ->
      ghost_ (
        match proof with
        | Some proof -> Interpret.sound input_rules proof env interpretation ()
        | None -> ());
      true
    | _ -> false

let () =
  let rule = {R.vars = [L.Integer]; lhs = R.Add (R.Var 0, R.Int_lit 0); rhs = R.Var 0} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  if not (R.valid rules) then assert false else
  let state = G.create rules in
  let input = L.Add (L.Int_input, L.Int_lit 0) in
  let #{G.id; state} = G.admit state input in
  ghost_ (
    match id with
    | Some id ->
      let view = borrow_ state in
      let _ : {u : unit | Snapshot.origin (G.model view) id === Some input} = () in ()
    | None -> ());
  let admitted = ghost_ (let view = borrow_ state in G.model view) in
  let #{G.status; fuel = _; state} = G.saturate state 10 10 10000 in
  assert (status = G.Fixed_point);
  ghost_ (
    match id with
    | Some id ->
      let view = borrow_ state in
      G.preserved_origin admitted (G.model view) id ();
      let _ : {u : unit | Snapshot.origin (G.model view) id === Some input} = () in ()
    | None -> ());
  ghost_ (
    match status with
    | G.Fixed_point ->
      let view = borrow_ state in
      let _ : {u : unit | F.fixed (G.model view) rules} = () in ()
    | _ -> ());
  let #{G.status; state = _; proof} = G.query state input L.Int_input in
  assert (status = G.Equal);
  ghost_ (
    match status, proof with
    | G.Equal, Some proof ->
      let _ : {u : unit | E.valid rules proof && E.left proof === input &&
        E.right proof === L.Int_input} = () in ()
    | _ -> ());
  ()

let rec chain depth =
  if depth <= 0 then L.Int_lit 0
  else L.Add (L.Int_lit 1, chain (depth - 1))

let () =
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 2} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  if not (R.valid rules) then assert false else
  let state = G.create rules in
  let #{G.id; state} = G.admit state (chain 510) in
  assert (Option.is_some id);
  let #{G.status; fuel = _; state = _} = G.saturate state 10 10 10000 in
  assert (status = G.Saturation_node_limit)

let () =
  let rules = R.No_rules in
  if not (R.valid rules) then assert false else
  let state = G.create rules in
  let #{G.id; state} = G.admit state (L.Int_lit 0) in
  match id with
  | None -> assert false
  | Some a ->
    let #{G.id; state} = G.admit state (L.Bool_lit false) in
    match id with
    | None -> assert false
    | Some b ->
      let #{G.equal; state; proof = _} = G.same_class state a b in
      assert (not equal);
      let #{G.equal; state = _; proof} = G.same_class state a a in
      assert equal;
      ghost_ (
        match proof with
        | None -> ()
        | Some proof ->
          let _ : {u : unit | E.valid rules proof && E.left proof === E.right proof} = () in ());
      ()

let () =
  let rules = R.No_rules in
  if not (R.valid rules) then assert false else
  let state = G.create rules in
  let size = Obj.size (Obj.repr state) in
  let expected = if Sys.backend_type = Sys.Native then 2 else 3 in
  assert (size = expected)
