module H = Vox_egraph_rule_hashcons
module G = Vox_egraph_rule_union
module V = Vox_egraph_rule_store
module S = Vox_egraph_rule_semantics
module F = Vox_egraph_origin_frame
module E = Vox_egraph_derivation_spec
module L = Vox_egraph_language_spec

type node_result = #{
  equal : bool;
  state : H.t;
  proof : E.evidence option @@ ghost;
}

let nodes : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (a : {i : int | 0 <= i && i < state.owner.count}) ->
    (b : {i : int | 0 <= i && i < state.owner.count}) ->
    {r : node_result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store === state.store &&
      r.#state.owner.count = state.owner.count &&
      (match r.#proof with
       | None -> not r.#equal
       | Some proof -> r.#equal &&
         E.valid state.store.semantic.rules proof &&
         E.left proof === S.origin state.store.semantic.origins a &&
         E.right proof === S.origin state.store.semantic.origins b)}
    @ unique = fun state a b ->
  let {H.owner; store} = state in
  ghost_ (V.valid_def store);
  let equal = G.same store.semantic a b in
  let proof = ghost_ (
    if equal then (
      V.nodes_valid_def store store.semantic.union.count;
      V.node_at_data store.nodes store.sorts store.semantic.origins
        store.semantic.union.count a ();
      V.node_at_data store.nodes store.sorts store.semantic.origins
        store.semantic.union.count b ();
      V.node_ok_data_def store.nodes store.sorts store.semantic.origins a;
      V.node_ok_data_def store.nodes store.sorts store.semantic.origins b;
      Some (G.same_evidence store.semantic a b ()))
    else None) in
  let state = {H.owner; store} in
  #{equal; state; proof}

type status = Equal | Not_proved | Invalid_input | Node_limit
type result = #{
  status : status;
  state : H.t;
  proof : E.evidence option @@ ghost;
}

let expressions : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (left : L.expr) @ immutable -> (right : L.expr) @ immutable ->
    {r : result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count >= state.owner.count &&
      F.preserved state.store.semantic.origins r.#state.store.semantic.origins state.owner.count &&
      (match r.#status, r.#proof with
       | Equal, Some proof -> E.valid state.store.semantic.rules proof &&
         E.left proof === left && E.right proof === right
       | Not_proved, None -> true
       | Invalid_input, None -> L.sort left === None || L.sort right === None
       | Node_limit, None -> r.#state.owner.count = 512
       | _ -> false)} @ unique = fun state left right ->
  ghost_ (let view = borrow_ state in H.O.valid_def view.owner);
  let initial = ghost_ (let view = borrow_ state in view.store.semantic.origins) in
  let initial_count = ghost_ (let view = borrow_ state in view.owner.count) in
  let #{H.value = first; state} = H.admit_expr state left in
  match first with
  | None ->
    let status = match L.sort left with
      | None -> Invalid_input | Some _ -> Node_limit in
    #{status; state; proof = ghost_ None}
  | Some first ->
    let before = ghost_ (
      let view = borrow_ state in view.store.semantic.origins) in
    let count = ghost_ (let view = borrow_ state in view.owner.count) in
    let #{H.value = second; state} = H.admit_expr state right in
    ghost_ (
      let view = borrow_ state in
      F.weaken before view.store.semantic.origins count initial_count ();
      F.compose initial before view.store.semantic.origins initial_count ());
    (match second with
     | None ->
       let status = match L.sort right with
         | None -> Invalid_input | Some _ -> Node_limit in
       #{status; state; proof = ghost_ None}
     | Some second ->
       ghost_ (
         let view = borrow_ state in
         F.at before view.store.semantic.origins count first ();
         S.origin_def before first;
         S.origin_def view.store.semantic.origins first);
       let #{equal; state; proof} = nodes state first second in
       #{status = (if equal then Equal else Not_proved); state; proof})
