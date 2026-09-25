module C = Vox_egraph_preservation_spec
module S = Vox_egraph_snapshot_proof
module P = Vox_egraph_match_evidence
module V = Vox_egraph_rule_store
module F = Vox_egraph_origin_frame
module O = Vox_egraph_match_observation

let rec (origins @ total) : (before : V.t) @ immutable -> (after : V.t) @ immutable -> (count : int) ->
    {u : unit | V.valid before && V.valid after && 0 <= count &&
      count <= before.semantic.union.count && count <= after.semantic.union.count &&
      F.preserved before.semantic.origins after.semantic.origins count} ->
    {u : unit | C.origins (P.view before) (P.view after) count} @ ghost =
  fun before after count premise -> ghost_ (
    C.origins_def (P.view before) (P.view after) count;
    if count > 0 then (
      F.at before.semantic.origins after.semantic.origins count (count - 1) ();
      Vox_egraph_rule_semantics.origin_def before.semantic.origins (count - 1);
      Vox_egraph_rule_semantics.origin_def after.semantic.origins (count - 1);
      S.origin before (count - 1) ();
      S.origin after (count - 1) ();
      F.weaken before.semantic.origins after.semantic.origins count (count - 1) ();
      origins before after (count - 1) ());
    ())
  [@@decreases if count > 0 then count else 0]

let (extends @ total) : (before : V.t) @ immutable -> (after : V.t) @ immutable ->
    {u : unit | V.valid before && V.valid after && before.semantic.union.count <= after.semantic.union.count &&
      F.preserved before.semantic.origins after.semantic.origins before.semantic.union.count} ->
    {u : unit | C.extends (P.view before) (P.view after)} @ ghost = fun before after premise -> ghost_ (
  P.bounds before ();
  P.view_def before;
  P.view_def after;
  O.observe_def before.nodes before.semantic.union.parents before.semantic.union.count;
  O.observe_def after.nodes after.semantic.union.parents after.semantic.union.count;
  origins before after before.semantic.union.count ();
  C.extends_def (P.view before) (P.view after);
  ())

let rec (at @ total) :
    (before : Vox_egraph_match_spec.graph) @ immutable ->
    (after : Vox_egraph_match_spec.graph) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | C.origins before after count && 0 <= id && id < count} ->
    {u : unit | Vox_egraph_snapshot_spec.origin before id === Vox_egraph_snapshot_spec.origin after id}
      @ ghost = fun before after count id premise -> ghost_ (
  C.origins_def before after count;
  if id < count - 1 then at before after (count - 1) id ();
  ())
  [@@decreases if count > 0 then count else 0]

let (preserved_origin @ total) :
    (before : Vox_egraph_match_spec.graph) @ immutable ->
    (after : Vox_egraph_match_spec.graph) @ immutable -> (id : int) ->
    {u : unit | C.extends before after && 0 <= id && id < before.count} ->
    {u : unit | Vox_egraph_snapshot_spec.origin before id === Vox_egraph_snapshot_spec.origin after id}
      @ ghost = fun before after id premise -> ghost_ (
  C.extends_def before after;
  at before after before.count id ())
