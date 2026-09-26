module C = Vox_egraph_closure_spec
module V = Vox_egraph_rule_store
module B = Vox_egraph_match_subst
module Q = Vox_egraph_match_spec
module P = Vox_egraph_match_evidence
module O = Vox_egraph_match_observation
module N = Vox_egraph_rule_node
module I = Vox_iarray
module L = Vox_egraph_language_spec

let rec (binding_valid @ total) : (state : V.t) @ immutable ->
    (vars : L.sort list) @ immutable -> (bindings : int list) @ immutable ->
    {u : unit | V.valid state} ->
    {u : unit | C.binding_valid (P.view state) vars bindings = B.accepts state vars bindings}
      @ ghost = fun state vars bindings premise -> ghost_ (
  let graph = P.view state in
  P.bounds state ();
  P.view_def state;
  O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
  C.binding_valid_def graph vars bindings;
  B.accepts_def state vars bindings;
  (match vars, bindings with
   | sort :: vars, id :: ids ->
     binding_valid state vars ids ();
     if id >= 0 then (
       if id < state.semantic.union.count then (
         O.node_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
         V.valid_def state;
         V.nodes_valid_def state state.semantic.union.count;
         V.node_at_data state.nodes state.sorts state.semantic.origins
           state.semantic.union.count id ();
         V.node_ok_data_def state.nodes state.sorts state.semantic.origins id;
         match I.at state.nodes id with
         | Some (Some node) -> N.sort_def node; C.node_sort_def node
         | _ -> ())
       else Q.node_def graph id)
   | _ -> ());
  ())
