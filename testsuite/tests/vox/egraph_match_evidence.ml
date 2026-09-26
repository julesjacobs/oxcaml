(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml vox_egraph_key.ml vox_egraph_match_spec.ml vox_egraph_rule_node.ml vox_egraph_rule_store.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_evidence.ml vox_egraph_match_intro.ml vox_egraph_match_bindings.ml egraph_match_evidence.ml";
 { bytecode; }
*)

module V = Vox_egraph_rule_store
module N = Vox_egraph_rule_node
module R = Vox_egraph_rule_spec
module P = Vox_egraph_match_evidence
module Scan = Vox_egraph_match_scan
module Q = Vox_egraph_match_spec
module E = Vox_egraph_derivation_spec
module S = Vox_egraph_rule_semantics

let check (state : {s : V.t | V.valid s}) (pat : R.pat @ immutable) root =
  ghost_ (P.bounds state (); P.view_def state);
  let found = Scan.matches state.nodes state.semantic.union.parents
    state.semantic.union.count pat [] root in
  assert found;
  if found then ghost_ (
    Vox_egraph_match_bindings.matched state pat [] root ();
    P.agrees_def state.semantic.origins [] [];
    let proof = P.derive state pat [] [] root () in
    let _ : {u : unit | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins root &&
      E.right proof === R.instantiate pat []} = () in ());
  ()

let () =
  let state = V.create R.No_rules in
  let #{V.value = zero; state} = V.add state (N.Int_lit 0) in
  match zero with
  | None -> assert false
  | Some zero ->
    let #{V.value = sum; state} = V.add state (N.Add (zero, zero)) in
    match sum with
    | None -> assert false
    | Some sum ->
      check state (R.Add (R.Int_lit 0, R.Int_lit 0)) sum;
      let #{V.value = equal; state} = V.add state (N.Eq_int (sum, zero)) in
      match equal with
      | None -> assert false
      | Some equal ->
        check state (R.Eq_int (R.Add (R.Int_lit 0, R.Int_lit 0), R.Int_lit 0)) equal
