module Preserves = Vox_egraph_preservation_spec
module Preserve = Vox_egraph_preservation_proof
module H = Vox_egraph_rule_hashcons
module V = Vox_egraph_rule_store
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module P = Vox_egraph_match_evidence

type raw = {engine : H.t; rules : R.t @@ global; model : Q.graph @@ ghost}
type t = {s : raw |  H.O.valid s.engine.owner && V.valid s.engine.store &&
      s.engine.owner.count = s.engine.store.semantic.union.count &&
      H.matching (H.A.contents s.engine.owner.arena)
        s.engine.store.nodes s.engine.owner.count &&
      H.O.Memo.Spec.valid s.engine.owner.view &&
      H.H.at (H.P.own s.engine.owner.token) (H.T.location s.engine.owner.memo) ===
        Some s.engine.owner.view.model &&
  R.valid s.rules && s.rules === s.engine.store.semantic.rules &&
  s.model === P.view s.engine.store}

let[@def] (model @ total) (state : t @ local immutable) = ghost_ (
  let state = state in state.model)

let[@def] (rules @ total) (state : t @ local immutable) = ghost_ (
  let state = state in state.rules)

let create : (input_rules : {rs : R.t | R.valid rs}) @ immutable ->
    {state : t | (model state).count = 0 && rules state === input_rules} @ unique =
  fun rules ->
    let engine = H.create rules in
    let {H.owner; store} = engine in
    let model = ghost_ (
      P.view_def store;
      Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count;
      P.view store) in
    let engine = {H.owner; store} in
    let state : t = {engine; rules; model} in
    ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
    state

module L = Vox_egraph_language_spec
module Snapshot = Vox_egraph_snapshot_spec
module Snapshot_proof = Vox_egraph_snapshot_proof

type admit_result = #{id : int option @@ aliased; state : t}

let admit : (state : t) @ unique -> (expr : L.expr) @ immutable ->
    {r : admit_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      (model r.#state).count >= (model state).count &&
      (match r.#id with
       | None -> L.sort expr === None || (model r.#state).count = 512
       | Some id -> 0 <= id && id < (model r.#state).count &&
         Snapshot.origin (model r.#state) id === Some expr)} @ unique =
  fun state expr ->
    ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
    let {engine; rules; model = _} = state in
    let {H.owner; store} = engine in
    let before = ghost_ store in
    ghost_ (
      P.view_def store;
      Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count);
    let #{H.value; state = engine} = H.admit_expr {H.owner; store} expr in
    let {H.owner; store} = engine in
    let model = ghost_ (
      P.view_def store;
      Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count;
      P.view store) in
    ghost_ (Preserve.extends before store ());
    ghost_ (match value with None -> () | Some id -> Snapshot_proof.origin store id ());
    let engine = {H.owner; store} in
    let state : t = {engine; rules; model} in
    ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
    #{id = value; state}

module E = Vox_egraph_derivation_spec
module Query = Vox_egraph_rule_query

type equality = Query.status = Equal | Not_proved | Invalid_input | Node_limit
type query_result = #{status : equality; state : t; proof : E.evidence option @@ ghost}

let query : (state : t) @ unique -> (left : L.expr) @ immutable -> (right : L.expr) @ immutable ->
    {r : query_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      (match r.#status, r.#proof with
       | Equal, Some proof -> E.valid (rules state) proof &&
         E.left proof === left && E.right proof === right
       | Not_proved, None -> true
       | Invalid_input, None -> L.sort left === None || L.sort right === None
       | Node_limit, None -> (model r.#state).count = 512
       | _ -> false)} @ unique = fun state left right ->
  ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
  let {engine; rules; model = _} = state in
  let {H.owner; store} = engine in
  let before = ghost_ store in
  let engine = {H.owner; store} in
  let #{Query.status; state = engine; proof} = Query.expressions engine left right in
  let {H.owner; store} = engine in
  ghost_ (Preserve.extends before store ());
  let model = ghost_ (
    P.view_def store;
    Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count;
    P.view store) in
  let engine = {H.owner; store} in
  let state : t = {engine; rules; model} in
  ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
  #{status; state; proof}

module Sat = Vox_egraph_rule_saturate
module Fixed = Vox_egraph_fixedpoint_spec

type saturation_status = Fixed_point | Saturation_node_limit | Search_limit | Rebuild_limit | Round_limit
type saturation_result = #{status : saturation_status; fuel : int; state : t}

let saturate : (state : t) @ unique -> (rounds : int) -> (rebuild_passes : int) ->
    (fuel : {f : int | 0 <= f && f <= 4611686018427387903}) ->
    {r : saturation_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      0 <= r.#fuel && r.#fuel <= fuel &&
      (match r.#status with
       | Fixed_point -> Fixed.fixed (model r.#state) (rules state)
       | Saturation_node_limit -> (model r.#state).count = 512
       | Search_limit -> r.#fuel = 0
       | Rebuild_limit | Round_limit -> true)} @ unique = fun state rounds rebuild_passes fuel ->
  ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
  let {engine; rules; model = _} = state in
  let {H.owner; store} = engine in
  let before = ghost_ store in
  let engine = {H.owner; store} in
  let #{Sat.status; fuel; state = engine} = Sat.saturate engine rules rounds rebuild_passes fuel in
  let {H.owner; store} = engine in
  ghost_ (Preserve.extends before store ());
  let model = ghost_ (
    P.view_def store;
    Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count;
    P.view store) in
  let engine = {H.owner; store} in
  let state : t = {engine; rules; model} in
  ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
  let status = match status with
    | Sat.Fixed_point -> Fixed_point
    | Sat.Node_limit -> Saturation_node_limit
    | Sat.Search_limit -> Search_limit
    | Sat.Rebuild_limit -> Rebuild_limit
    | Sat.Round_limit -> Round_limit in
  #{status; fuel; state}

let preserved_origin = Preserve.preserved_origin

module Model_evidence = Vox_egraph_model_evidence

type class_result = #{equal : bool; state : t; proof : E.evidence option @@ ghost}

let (same_class @ total) : (state : t) @ unique ->
    (a : {i : int | 0 <= i && i < (model state).count}) ->
    (b : {i : int | 0 <= i && i < (model state).count}) ->
    {r : class_result | model r.#state === model state && rules r.#state === rules state &&
      r.#equal = Q.same (model state) a b &&
      (match r.#proof with
       | None -> not r.#equal
       | Some proof -> r.#equal && E.valid (rules state) proof &&
         Snapshot.origin (model state) a === Some (E.left proof) &&
         Snapshot.origin (model state) b === Some (E.right proof))} @ unique =
  fun state a b ->
    ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
    let {engine; rules; model} = state in
    let {H.owner; store} = engine in
    ghost_ (
      P.view_def store;
      Vox_egraph_match_observation.observe_def store.nodes store.semantic.union.parents store.semantic.union.count);
    let #{Model_evidence.equal; proof} = Model_evidence.query store a b in
    let engine = {H.owner; store} in
    let state : t = {engine; rules; model} in
    ghost_ (model_def (borrow_ state); rules_def (borrow_ state));
    #{equal; state; proof}
