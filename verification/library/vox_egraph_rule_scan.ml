module Frame = Vox_egraph_origin_frame
module H = Vox_egraph_rule_hashcons
module V = Vox_egraph_rule_store
module W = Vox_egraph_rule_rewrite
module R = Vox_egraph_rule_spec
module P = Vox_egraph_match_evidence
module C = Vox_egraph_closure_spec
module S = Vox_egraph_saturation_spec
module Cases = Vox_egraph_assignment_spec
module Equiv = Vox_egraph_assignment_proof
module CP = Vox_egraph_saturation_proof

type status = Stable | Changed | Node_limit | Work_limit
type result = #{status : status; fuel : int; state : H.t}

let rec roots : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) ->
    (rule : {r : R.rule | R.lookup_rule rules index === Some r && R.rule_valid r}) @ immutable ->
    (bindings : int list) @ immutable ->
    (count : {n : int | 0 <= n && n <= state.owner.count}) ->
    (fuel : {f : int | 0 <= f}) ->
    {r : result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store.semantic.rules === rules &&
      Frame.preserved state.store.semantic.origins r.#state.store.semantic.origins state.owner.count &&
      r.#state.owner.count >= state.owner.count &&
      0 <= r.#fuel && r.#fuel <= fuel &&
      (match r.#status with
       | Stable -> r.#state.store === state.store &&
         r.#state.owner.count = state.owner.count &&
         C.closed_roots (P.view r.#state.store) rule bindings count
       | Node_limit -> r.#state.owner.count = 512
       | Work_limit -> r.#fuel = 0
       | Changed -> true)} @ unique =
  fun state rules index rule bindings count fuel ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    if count <= 0 then (
      let {H.owner; store} = state in
      ghost_ (C.closed_roots_def (P.view store) rule bindings count);
      #{status = Stable; fuel; state = {H.owner; store}})
    else if fuel <= 0 then #{status = Work_limit; fuel = 0; state}
    else
      let before = (let view = borrow_ state in view.owner.count) in
      let #{W.status; merged; state; left = _; right = _} =
        W.matched_rule state rules index rule bindings (count - 1) in
      match status with
      | W.Node_limit -> #{status = Node_limit; fuel = fuel - 1; state}
      | W.Applied | W.Invalid_rule | W.Invalid_bindings | W.No_match ->
        let after = (let view = borrow_ state in view.owner.count) in
        if merged || after > before then #{status = Changed; fuel = fuel - 1; state}
        else
          let {H.owner; store} = state in
          ghost_ (
            CP.binding_valid store rule.vars bindings ();
            C.closed_roots_def (P.view store) rule bindings count);
          let state = {H.owner; store} in
          let #{status; fuel; state} = roots state rules index rule bindings
            ((count - 1)) ((fuel - 1)) in
          let {H.owner; store} = state in
          ghost_ (C.closed_roots_def (P.view store) rule bindings count);
          #{status; fuel; state = {H.owner; store}}
  [@@decreases count]

let rec cases : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) ->
    (rule : {r : R.rule | R.lookup_rule rules index === Some r && R.rule_valid r}) @ immutable ->
    (work : Cases.cases) @ immutable ->
    (fuel : {f : int | 0 <= f}) ->
    {r : result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store.semantic.rules === rules &&
      Frame.preserved state.store.semantic.origins r.#state.store.semantic.origins state.owner.count &&
      r.#state.owner.count >= state.owner.count &&
      0 <= r.#fuel && r.#fuel <= fuel &&
      (match r.#status with
       | Stable -> r.#state.store === state.store &&
         r.#state.owner.count = state.owner.count &&
         Cases.closed_cases (P.view r.#state.store) rule work
       | Node_limit -> r.#state.owner.count = 512
       | Work_limit -> r.#fuel = 0
       | Changed -> true)} @ unique =
  fun state rules index rule work fuel ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    match work with
    | Cases.End ->
      let {H.owner; store} = state in
      ghost_ (Cases.closed_cases_def (P.view store) rule work);
      #{status = Stable; fuel; state = {H.owner; store}}
    | Cases.Case (bindings, rest) ->
      if fuel <= 0 then #{status = Work_limit; fuel = 0; state}
      else
        let count = (let view = borrow_ state in view.owner.count) in
        ghost_ (let view = borrow_ state in H.O.valid_def view.owner);
        let result = roots state rules index rule bindings (count) ((fuel - 1)) in
        match result.#status with
        | Changed | Node_limit | Work_limit -> result
        | Stable ->
          let #{status = _; fuel; state} = result in
          let #{status; fuel; state} = cases state rules index rule rest fuel in
          let {H.owner; store} = state in
          ghost_ (
            P.view_def store;
            Vox_egraph_match_observation.observe_def store.nodes
              store.semantic.union.parents store.semantic.union.count;
            Cases.closed_cases_def (P.view store) rule work);
          #{status; fuel; state = {H.owner; store}}

module A = Vox_egraph_assignments

let rule : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) ->
    (rule : {r : R.rule | R.lookup_rule rules index === Some r && R.rule_valid r}) @ immutable ->

    (fuel : {f : int | 0 <= f}) ->
    {r : result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store.semantic.rules === rules &&
      Frame.preserved state.store.semantic.origins r.#state.store.semantic.origins state.owner.count &&
      r.#state.owner.count >= state.owner.count &&
      0 <= r.#fuel && r.#fuel <= fuel &&
      (match r.#status with
       | Stable -> r.#state.store === state.store &&
         r.#state.owner.count = state.owner.count &&
         S.closed_rule (P.view r.#state.store) rule
       | Node_limit -> r.#state.owner.count = 512
       | Work_limit -> r.#fuel = 0
       | Changed -> true)} @ unique =
  fun state rules index rule fuel ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    let count = (let view = borrow_ state in view.owner.count) in
    match A.enumerate count rule.vars fuel with
    | A.Exhausted -> #{status = Work_limit; fuel = 0; state}
    | A.Done (work, left) ->
      let #{status; fuel; state} = cases state rules index rule work left in
      let {H.owner; store} = state in
      ghost_ (
        P.view_def store;
        Vox_egraph_match_observation.observe_def store.nodes
          store.semantic.union.parents store.semantic.union.count;
        Equiv.closed_rule (P.view store) rule);
      #{status; fuel; state = {H.owner; store}}
