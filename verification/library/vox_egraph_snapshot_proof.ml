module C = Vox_egraph_snapshot_spec
module V = Vox_egraph_rule_store
module P = Vox_egraph_match_evidence
module O = Vox_egraph_match_observation
module Q = Vox_egraph_match_spec
module S = Vox_egraph_rule_semantics
module N = Vox_egraph_rule_node
module I = Vox_iarray

let rec (origin @ total) : (state : V.t) @ immutable -> (id : int) ->
    {u : unit | V.valid state && 0 <= id && id < state.semantic.union.count} ->
    {u : unit | C.origin (P.view state) id === Some (S.origin state.semantic.origins id)}
      @ ghost = fun state id premise -> ghost_ (
  let graph = P.view state in
  P.bounds state ();
  P.view_def state;
  O.node_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
  V.valid_def state;
  V.nodes_valid_def state state.semantic.union.count;
  V.node_at_data state.nodes state.sorts state.semantic.origins state.semantic.union.count id ();
  V.node_ok_data_def state.nodes state.sorts state.semantic.origins id;
  C.origin_def graph id;
  (match I.at state.nodes id with
   | Some (Some node) ->
     N.origin_def state.semantic.origins node;
     N.children_below_def id node;
     (match node with
      | Q.Add (a, b) | Q.Eq_int (a, b) -> origin state a (); origin state b ()
      | Q.Int_if (c, a, b) | Q.Bool_if (c, a, b) ->
        origin state c (); origin state a (); origin state b ()
      | _ -> ())
   | _ -> ());
  ())
  [@@decreases if id > 0 then id else 0]
