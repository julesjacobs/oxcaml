module Frame = Vox_egraph_origin_frame
module H = Vox_egraph_rule_hashcons
module V = Vox_egraph_rule_store
module R = Vox_egraph_rule_spec
module P = Vox_egraph_match_evidence
module Scan = Vox_egraph_rule_scan
module Rules = Vox_egraph_rules_scan
module Cursor = Vox_egraph_rule_cursor
module CP = Vox_egraph_congruence_proof
module F = Vox_egraph_fixedpoint_spec

type status = Fixed_point | Node_limit | Search_limit | Rebuild_limit | Round_limit
type result = #{status : status; fuel : int; state : H.t}

let rec saturate : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules && R.valid rs}) @ immutable ->
    (rounds : int) -> (rebuild_passes : int) ->
    (fuel : {f : int | 0 <= f && f <= 4611686018427387903}) ->
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
       | Fixed_point -> F.fixed (P.view r.#state.store) rules
       | Node_limit -> r.#state.owner.count = 512
       | Search_limit -> r.#fuel = 0
       | Rebuild_limit | Round_limit -> true)} @ unique =
  fun state rules rounds rebuild_passes fuel ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    let before = ghost_ (let view = borrow_ state in view.store.semantic.origins) in
    let count = ghost_ (let view = borrow_ state in view.owner.count) in
    if rounds <= 0 then #{status = Round_limit; fuel; state}
    else
      let #{H.status; state} = H.rebuild state rebuild_passes in
      match status with
      | H.Work_limit -> #{status = Rebuild_limit; fuel; state}
      | H.Stable_pass ->
        let {H.owner; store} = state in
        ghost_ (
          CP.closed store (512 * 513 + 1) ();
          Cursor.drop_def rules 0);
        let state = {H.owner; store} in
        let #{Scan.status; fuel; state} = Rules.scan state rules 0 rules fuel in
        match status with
        | Scan.Node_limit -> #{status = Node_limit; fuel; state}
        | Scan.Work_limit -> #{status = Search_limit; fuel; state}
        | Scan.Changed ->
          let middle = ghost_ (let view = borrow_ state in view.store.semantic.origins) in
          let middle_count = ghost_ (let view = borrow_ state in view.owner.count) in
          let #{status; fuel; state} = saturate state rules (rounds - 1) rebuild_passes (fuel) in
          ghost_ (
            let view = borrow_ state in
            Frame.weaken middle view.store.semantic.origins middle_count count ();
            Frame.compose before middle view.store.semantic.origins count ());
          #{status; fuel; state}
        | Scan.Stable ->
          let {H.owner; store} = state in
          ghost_ (F.fixed_def (P.view store) rules);
          #{status = Fixed_point; fuel; state = {H.owner; store}}
  [@@decreases if rounds > 0 then rounds else 0]
