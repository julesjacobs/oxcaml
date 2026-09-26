module Frame = Vox_egraph_origin_frame
module H = Vox_egraph_rule_hashcons
module V = Vox_egraph_rule_store
module S = Vox_egraph_rule_semantics
module M = Vox_egraph_union_spec
module R = Vox_egraph_rule_spec
module EP = Vox_egraph_derivation

module P = Vox_egraph_match_evidence
module Scan = Vox_egraph_match_scan
module B = Vox_egraph_match_subst
module Q = Vox_egraph_match_spec
module E = Vox_egraph_derivation_spec
module BF = Vox_egraph_binding_frame
module PA = Vox_egraph_pattern_admit
module SI = Vox_egraph_match_store_intro

type status = Applied | Invalid_rule | Invalid_bindings | No_match | Node_limit
type result = #{
  status : status;
  merged : bool;
  left : int;
  right : int;
  state : H.t;
}

let matched_rule : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) ->
    (rule : {r : R.rule | R.lookup_rule rules index === Some r && R.rule_valid r}) @ immutable ->
    (bindings : int list) @ immutable -> (root : int) ->
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
      (if not r.#merged && r.#state.owner.count = state.owner.count then
        r.#state.store === state.store else true) &&
      (match r.#status with
       | Invalid_rule ->
         (match R.lookup_rule rules index with
          | None -> true | Some rule -> not (R.rule_valid rule))
       | Invalid_bindings ->
         (match R.lookup_rule rules index with
          | None -> false | Some rule ->
            not (B.accepts state.store rule.vars bindings))
       | No_match ->
         (match R.lookup_rule rules index with
          | None -> false | Some rule ->
            not (Q.matches (P.view state.store) rule.lhs bindings root))
       | Node_limit -> r.#state.owner.count = 512
       | Applied ->
         r.#left = root && 0 <= root && root < r.#state.owner.count &&
         0 <= r.#right && r.#right < r.#state.owner.count &&
         M.root r.#state.store.semantic.union.parents root =
           M.root r.#state.store.semantic.union.parents r.#right &&
         (if not r.#merged && r.#state.owner.count = state.owner.count then
           match R.lookup_rule rules index with
           | None -> false | Some rule ->
             Q.matches (P.view r.#state.store) rule.rhs bindings root
          else true))} @ unique =
  fun state rules index rule bindings root ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    let {H.owner; store} = state in
    ghost_ (P.bounds store (); P.view_def store);
    let #{B.accepted; subst} = B.build store rule.vars bindings in
    if not accepted then
      #{status = Invalid_bindings; merged = false; left = -1; right = -1;
        state = {H.owner; store}}
    else
      let subst = ghost_ (match subst with Some subst -> subst | None -> []) in
      if not (Scan.matches store.nodes store.semantic.union.parents
        store.semantic.union.count rule.lhs bindings root) then
        #{status = No_match; merged = false; left = -1; right = -1;
          state = {H.owner; store}}
      else (
        ghost_ (
          Q.matches_def (P.view store) rule.lhs bindings root;
          P.matched_id store root (Q.classes (P.view store) rule.lhs bindings) ());
        if Scan.matches store.nodes store.semantic.union.parents
          store.semantic.union.count rule.rhs bindings root then
          #{status = Applied; merged = false; left = root; right = root;
            state = {H.owner; store}}
        else
        let proof = ghost_ (
          let matched = P.derive store rule.lhs bindings subst root () in
          let instance = EP.rule_instance rules index rule subst () in
          EP.transitive rules matched instance ()) in
        let before = ghost_ store.semantic.origins in
        let count = ghost_ store.semantic.union.count in
        ghost_ (BF.rhs_admissible store rule bindings subst root ());
        let #{H.value = right; state} =
          PA.admit {H.owner; store} rule.rhs bindings subst () in
        match right with
        | None ->
          #{status = Node_limit; merged = false; left = -1; right = -1; state}
        | Some right ->
          ghost_ (
            let view = borrow_ state in
            Frame.at before view.store.semantic.origins count root ();
            S.origin_def before root;
            S.origin_def view.store.semantic.origins root);
          let #{H.merged; state} = H.merge_nodes state root right proof in
          let {H.owner; store} = state in
          ghost_ (
            let view = borrow_ owner in
            if not merged && view.count = count then
              SI.same_match store rule.rhs bindings root right ());
          #{status = Applied; merged; left = root; right; state = {H.owner; store}})

let matched : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) -> (bindings : int list) @ immutable -> (root : int) ->
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
      (if not r.#merged && r.#state.owner.count = state.owner.count then
        r.#state.store === state.store else true) &&
      (match r.#status with
       | Invalid_rule ->
         (match R.lookup_rule rules index with
          | None -> true | Some rule -> not (R.rule_valid rule))
       | Invalid_bindings ->
         (match R.lookup_rule rules index with
          | None -> false | Some rule ->
            not (B.accepts state.store rule.vars bindings))
       | No_match ->
         (match R.lookup_rule rules index with
          | None -> false | Some rule ->
            not (Q.matches (P.view state.store) rule.lhs bindings root))
       | Node_limit -> r.#state.owner.count = 512
       | Applied ->
         r.#left = root && 0 <= root && root < r.#state.owner.count &&
         0 <= r.#right && r.#right < r.#state.owner.count &&
         M.root r.#state.store.semantic.union.parents root =
           M.root r.#state.store.semantic.union.parents r.#right &&
         (if not r.#merged && r.#state.owner.count = state.owner.count then
           match R.lookup_rule rules index with
           | None -> false | Some rule ->
             Q.matches (P.view r.#state.store) rule.rhs bindings root
          else true))} @ unique =
  fun state rules index bindings root ->
    ghost_ (let view = borrow_ state in
      H.O.valid_def view.owner;
      Frame.identity view.store.semantic.origins view.owner.count);
    match R.lookup_rule rules index with
    | None -> #{status = Invalid_rule; merged = false; left = -1; right = -1; state}
    | Some rule ->
      if not (R.rule_valid rule) then
        #{status = Invalid_rule; merged = false; left = -1; right = -1; state}
      else
        matched_rule state rules index rule bindings root
