module C = Vox_egraph_congruence_spec
module Q = Vox_egraph_match_spec
module P = Vox_egraph_match_evidence
module O = Vox_egraph_match_observation
module V = Vox_egraph_rule_store
module N = Vox_egraph_rule_node
module I = Vox_iarray

let (signature @ total) : (state : V.t) @ immutable ->
    (a : Q.node) @ immutable -> (b : Q.node) @ immutable ->
    {u : unit | V.valid state && N.children_below state.semantic.union.count a &&
      N.children_below state.semantic.union.count b} ->
    {u : unit | C.same_node (P.view state) a b =
      (N.signature state.semantic.union.parents a === N.signature state.semantic.union.parents b)}
      @ ghost = fun state a b premise -> ghost_ (
  let count = state.semantic.union.count in
  let parents = state.semantic.union.parents in
  P.bounds state ();
  P.view_def state;
  N.children_below_def count a;
  N.children_below_def count b;
  C.same_node_def (P.view state) a b;
  N.signature_def parents a;
  N.signature_def parents b;
  N.canonical_def parents a;
  N.canonical_def parents b;
  N.key_exact (N.canonical parents a) (N.canonical parents b);
  (match a, b with
   | Q.Add (x, y), Q.Add (u, v) | Q.Eq_int (x, y), Q.Eq_int (u, v) ->
     O.same state.nodes parents count x u ();
     O.same state.nodes parents count y v ()
   | Q.Int_if (z, x, y), Q.Int_if (w, u, v)
   | Q.Bool_if (z, x, y), Q.Bool_if (w, u, v) ->
     O.same state.nodes parents count z w ();
     O.same state.nodes parents count x u ();
     O.same state.nodes parents count y v ()
   | _ -> ());
  ())

let (pair @ total) : (state : V.t) @ immutable -> (left : int) -> (right : int) ->
    {u : unit | V.valid state && 0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count} ->
    {u : unit | C.pair (P.view state) left right = V.pair_closed state left right}
      @ ghost = fun state left right premise -> ghost_ (
  let graph = P.view state in
  P.bounds state ();
  P.view_def state;
  O.node_at state.nodes state.semantic.union.parents state.semantic.union.count left ();
  O.node_at state.nodes state.semantic.union.parents state.semantic.union.count right ();
  O.same state.nodes state.semantic.union.parents state.semantic.union.count left right ();
  C.pair_def graph left right;
  V.pair_closed_def state left right;
  V.collision_def state left right;
  (match Q.node graph left, Q.node graph right with
   | Some a, Some b ->
     P.node_facts state left a ();
     P.node_facts state right b ();
     signature state a b ()
   | _ -> ());
  ())

let rec (scan_at @ total) : (state : V.t) @ immutable ->
    (left : int) -> (right : int) -> (fuel : int) -> (a : int) -> (b : int) ->
    {u : unit | V.valid state && V.closed_fuel state left right fuel &&
      0 <= left && left <= a && a < state.semantic.union.count &&
      0 <= right && right <= state.semantic.union.count &&
      0 <= b && b < state.semantic.union.count && (if left = a then right <= b else true)} ->
    {u : unit | V.pair_closed state a b} @ ghost =
  fun state left right fuel a b premise -> ghost_ (
    V.closed_fuel_def state left right fuel;
    if fuel > 0 then (
      if right = state.semantic.union.count then
        scan_at state (left + 1) 0 (fuel - 1) a b ()
      else if left <> a || right <> b then
        scan_at state left (right + 1) (fuel - 1) a b ());
    ())
  [@@decreases if fuel > 0 then fuel else 0]

let rec (row @ total) : (state : V.t) @ immutable ->
    (fuel : int) -> (left : int) -> (count : int) ->
    {u : unit | V.valid state && V.closed_fuel state 0 0 fuel &&
      0 <= left && left < state.semantic.union.count &&
      0 <= count && count <= state.semantic.union.count} ->
    {u : unit | C.row (P.view state) left count} @ ghost =
  fun state fuel left count premise -> ghost_ (
    C.row_def (P.view state) left count;
    if count > 0 then (
      scan_at state 0 0 fuel left (count - 1) ();
      pair state left (count - 1) ();
      row state fuel left (count - 1) ());
    ())
  [@@decreases if count > 0 then count else 0]

let rec (rows @ total) : (state : V.t) @ immutable -> (fuel : int) -> (count : int) ->
    {u : unit | V.valid state && V.closed_fuel state 0 0 fuel &&
      0 <= count && count <= state.semantic.union.count} ->
    {u : unit | C.rows (P.view state) count} @ ghost = fun state fuel count premise -> ghost_ (
  P.bounds state ();
  P.view_def state;
  O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
  C.rows_def (P.view state) count;
  if count > 0 then (
    row state fuel (count - 1) state.semantic.union.count ();
    rows state fuel (count - 1) ());
  ())
  [@@decreases if count > 0 then count else 0]

let (closed @ total) : (state : V.t) @ immutable -> (fuel : int) ->
    {u : unit | V.valid state && V.closed_fuel state 0 0 fuel} ->
    {u : unit | C.closed (P.view state)} @ ghost = fun state fuel premise -> ghost_ (
  P.bounds state ();
  P.view_def state;
  O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
  rows state fuel state.semantic.union.count ();
  C.closed_def (P.view state);
  ())
