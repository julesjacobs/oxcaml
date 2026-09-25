module H = Vox_egraph_rule_hashcons
module V = Vox_egraph_rule_store
module S = Vox_egraph_rule_semantics
module F = Vox_egraph_origin_frame
module M = Vox_egraph_union_spec
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules
module EP = Vox_egraph_derivation

type status = Applied | Invalid_rule | Node_limit
type result = #{
  status : status;
  merged : bool;
  left : int;
  right : int;
  state : H.t;
}

let instance : (state : {s : H.t | H.O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      H.matching (H.A.contents s.owner.arena)
        s.store.nodes s.owner.count &&
      H.O.Memo.Spec.valid s.owner.view &&
      H.H.at (H.P.own s.owner.token) (H.T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (rules : {rs : R.t | rs === state.store.semantic.rules}) @ immutable ->
    (index : int) -> (subst : R.subst) @ immutable ->
    {r : result | H.O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      H.matching (H.A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      H.O.Memo.Spec.valid r.#state.owner.view &&
      H.H.at (H.P.own r.#state.owner.token) (H.T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      r.#state.store.semantic.rules === rules &&
      r.#state.owner.count >= state.owner.count &&
      (if not r.#merged && r.#state.owner.count = state.owner.count then
        r.#state.store === state.store else true) &&
      (match r.#status with
       | Invalid_rule ->
         (match R.lookup_rule rules index with
          | None -> true
          | Some rule -> not (R.rule_valid rule) ||
            not (R.subst_valid rule.vars subst))
       | Node_limit -> r.#state.owner.count = 512
       | Applied ->
         0 <= r.#left && r.#left < r.#state.owner.count &&
         0 <= r.#right && r.#right < r.#state.owner.count &&
         M.root r.#state.store.semantic.union.parents r.#left =
           M.root r.#state.store.semantic.union.parents r.#right &&
         (match R.lookup_rule rules index with
          | None -> false
          | Some rule ->
            S.origin r.#state.store.semantic.origins r.#left ===
              R.instantiate rule.lhs subst &&
            S.origin r.#state.store.semantic.origins r.#right ===
              R.instantiate rule.rhs subst))} @ unique =
  fun state rules index subst ->
    match R.lookup_rule rules index with
    | None ->
      #{status = Invalid_rule; merged = false; left = -1; right = -1; state}
    | Some rule ->
      if not (R.rule_valid rule) || not (R.subst_valid rule.vars subst) then
        #{status = Invalid_rule; merged = false; left = -1; right = -1; state}
      else (
        ghost_ (RP.instance_sorted rule subst ());
        let lhs = R.instantiate rule.lhs subst in
        let rhs = R.instantiate rule.rhs subst in
        let #{H.value = left; state} = H.admit_expr state lhs in
        match left with
        | None ->
          #{status = Node_limit; merged = false; left = -1; right = -1; state}
        | Some left ->
          let before = ghost_ (
            let view = borrow_ state in view.store.semantic.origins) in
          let count = ghost_ (
            let view = borrow_ state in view.owner.count) in
          let #{H.value = right; state} = H.admit_expr state rhs in
          match right with
          | None ->
            #{status = Node_limit; merged = false; left = -1; right = -1; state}
          | Some right ->
            ghost_ (
              let view = borrow_ state in
              F.at before view.store.semantic.origins count left ();
              S.origin_def before left;
              S.origin_def view.store.semantic.origins left);
            let proof = ghost_ (EP.rule_instance rules index rule subst ()) in
            let #{H.merged; state} = H.merge_nodes state left right proof in
            #{status = Applied; merged; left; right; state})
