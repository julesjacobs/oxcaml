module Frame = Vox_egraph_origin_frame
open Vox_egraph_rule_scan
module Cursor = Vox_egraph_rule_cursor

let rec scan : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : {i : int | 0 <= i && i <= 4611686018427387903}) ->
    (todo : {rs : R.t | Cursor.drop rules index === rs && R.valid rs}) @ immutable ->
    (fuel : {f : int | let i = index in 0 <= f && f <= 4611686018427387903 - i}) ->
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
         S.closed_rules (P.view r.#state.store) todo
       | Node_limit -> r.#state.owner.count = 512
       | Work_limit -> r.#fuel = 0
       | Changed -> true)} @ unique =
  fun state rules index todo fuel ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    ghost_ (R.valid_def todo);
    match todo with
    | R.No_rules ->
      let {H.owner; store} = state in
      ghost_ (S.closed_rules_def (P.view store) todo);
      #{status = Stable; fuel; state = {H.owner; store}}
    | R.Rule_cons (head, tail) ->
      if fuel <= 0 then #{status = Work_limit; fuel = 0; state}
      else (
        ghost_ (Cursor.lookup rules index ());
        let result = rule state rules index head ((fuel - 1)) in
        match result.#status with
        | Changed | Node_limit | Work_limit -> result
        | Stable ->
          ghost_ (Cursor.advance rules index ());
          let #{status = _; fuel = remaining; state} = result in
          ghost_ (let _ : {u : unit | 0 <= remaining && remaining <= 4611686018427387903 - (index + 1)} = () in ());
          let next = index + 1 in
          let budget : {f : int | 0 <= f && f <= 4611686018427387903 - next} = remaining in
          let #{status; fuel; state} = scan state rules (next) tail budget in
          let {H.owner; store} = state in
          ghost_ (S.closed_rules_def (P.view store) todo);
          #{status; fuel; state = {H.owner; store}})
