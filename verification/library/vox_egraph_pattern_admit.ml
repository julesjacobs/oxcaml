open Vox_egraph_rule_hashcons
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module MP = Vox_egraph_match_evidence
module BF = Vox_egraph_binding_frame
module SI = Vox_egraph_match_store_intro

let rec admit :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (pat : R.pat) @ immutable ->
    (bindings : int list) @ immutable -> (subst : R.subst) @ immutable ghost ->
    {u : unit | BF.bounded state.owner.count bindings &&
      BF.available state.owner.count pat bindings &&
      MP.agrees state.store.semantic.origins bindings subst &&
      not (L.sort (R.instantiate pat subst) === None)} @ ghost ->
    {r : result | O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      F.preserved state.store.semantic.origins
        r.#state.store.semantic.origins state.owner.count &&
      r.#state.owner.count >= state.owner.count &&
      (if r.#state.owner.count = state.owner.count then
        r.#state.store === state.store else true) &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (match r.#value with
       | None -> r.#state.owner.count = 512
       | Some id -> 0 <= id && id < r.#state.owner.count &&
         S.origin r.#state.store.semantic.origins id === R.instantiate pat subst &&
         (if r.#state.owner.count = state.owner.count then
           Q.matches (MP.view r.#state.store) pat bindings id else true))} @ unique =
  fun state pat bindings subst premise ->
    let expr = ghost_ (R.instantiate pat subst) in
    ghost_ (
      R.instantiate_def pat subst;
      let view = borrow_ state in
      BF.available_def view.owner.count pat bindings);
    ghost_ (let view = borrow_ state in
      O.valid_def view.owner; L.sort_def expr);
    let before = ghost_ (
      let view = borrow_ state in view.store.semantic.origins) in
    let count = ghost_ (let view = borrow_ state in view.owner.count) in
    ghost_ (F.identity before count);
    match pat with
    | R.Var index ->
      (match Q.binding bindings index with
       | None -> #{value = None; state}
       | Some id ->
         let allocated = let view = borrow_ state in view.owner.count in
         if 0 <= id && id < allocated then (
           ghost_ (MP.agrees_at before bindings subst index id ());
           let {owner; store} = state in
           ghost_ (SI.variable store bindings index id ());
           #{value = Some id; state = {owner; store}})
         else #{value = None; state})
    | R.Int_lit literal ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count (N.Int_lit literal));
      let state = {owner; store} in
      let node = (N.Int_lit literal) in
      let #{value; state} = add state node in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins (N.Int_lit literal));
      let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}}
    | R.Bool_lit literal ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count (N.Bool_lit literal));
      let state = {owner; store} in
      let node = (N.Bool_lit literal) in
      let #{value; state} = add state node in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins (N.Bool_lit literal));
      let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}}
    | R.Int_input ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count N.Int_input);
      let state = {owner; store} in
      let node = N.Int_input in
      let #{value; state} = add state node in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins N.Int_input);
      let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}}
    | R.Bool_input ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count N.Bool_input);
      let state = {owner; store} in
      let node = N.Bool_input in
      let #{value; state} = add state node in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins N.Bool_input);
      let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}}
    | R.Add (left, right) | R.Eq_int (left, right) ->
      ghost_ (
        let view = borrow_ state in
        BF.weaken count view.owner.count bindings ();
        BF.preserve before view.store.semantic.origins count bindings subst ();
        BF.available_weaken count view.owner.count left bindings ());
      let #{value = first; state} = admit state left bindings subst () in
      (match first with
       | None -> #{value = None; state}
       | Some first ->
         let middle = ghost_ (
           let view = borrow_ state in view.store.semantic.origins) in
         let middle_count = ghost_ (
           let view = borrow_ state in view.owner.count) in
         ghost_ (
           let view = borrow_ state in
           BF.weaken count view.owner.count bindings ();
           BF.preserve before view.store.semantic.origins count bindings subst ();
           BF.available_weaken count view.owner.count right bindings ());
         let #{value = second; state} = admit state right bindings subst () in
         ghost_ (
           let view = borrow_ state in
           F.weaken middle view.store.semantic.origins
             middle_count count ();
           F.compose before middle view.store.semantic.origins count ();
              F.at middle view.store.semantic.origins middle_count first ();
              S.origin_def middle first;
              S.origin_def view.store.semantic.origins first;
              ());
         (match second with
          | None -> #{value = None; state}
          | Some second ->
            let node = match pat with
              | R.Add _ -> N.Add (first, second)
              | _ -> N.Eq_int (first, second) in
            let {owner; store} = state in
            ghost_ (
              if not (L.sort expr === None) then (
                V.child_typed store first L.Integer ();
                V.child_typed store second L.Integer ());
              N.well_typed_def store.sorts store.semantic.union.count node);
            let state = {owner; store} in
            let middle = ghost_ (
              let view = borrow_ state in view.store.semantic.origins) in
            let middle_count = ghost_ (
              let view = borrow_ state in view.owner.count) in
            let #{value; state} = add state node in
            ghost_ (
              let view = borrow_ state in
              F.weaken middle view.store.semantic.origins
                middle_count count ();
              F.compose before middle view.store.semantic.origins count ();
              F.at middle view.store.semantic.origins middle_count first ();
              S.origin_def middle first;
              S.origin_def view.store.semantic.origins first;
              F.at middle view.store.semantic.origins middle_count second ();
              S.origin_def middle second;
              S.origin_def view.store.semantic.origins second;
              N.origin_def view.store.semantic.origins node;
              ());
            let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}}))
    | R.Int_if (condition, yes, no)
    | R.Bool_if (condition, yes, no) ->
      ghost_ (
        let view = borrow_ state in
        BF.weaken count view.owner.count bindings ();
        BF.preserve before view.store.semantic.origins count bindings subst ();
        BF.available_weaken count view.owner.count condition bindings ());
      let #{value = first; state} = admit state condition bindings subst () in
      (match first with
       | None -> #{value = None; state}
       | Some first ->
         let middle = ghost_ (
           let view = borrow_ state in view.store.semantic.origins) in
         let middle_count = ghost_ (
           let view = borrow_ state in view.owner.count) in
         ghost_ (
           let view = borrow_ state in
           BF.weaken count view.owner.count bindings ();
           BF.preserve before view.store.semantic.origins count bindings subst ();
           BF.available_weaken count view.owner.count yes bindings ());
         let #{value = second; state} = admit state yes bindings subst () in
         ghost_ (
           let view = borrow_ state in
           F.weaken middle view.store.semantic.origins
             middle_count count ();
           F.compose before middle view.store.semantic.origins count ();
              F.at middle view.store.semantic.origins middle_count first ();
              S.origin_def middle first;
              S.origin_def view.store.semantic.origins first;
              ());
         (match second with
          | None -> #{value = None; state}
          | Some second ->
            let middle = ghost_ (
              let view = borrow_ state in view.store.semantic.origins) in
            let middle_count = ghost_ (
              let view = borrow_ state in view.owner.count) in
            ghost_ (
              let view = borrow_ state in
              BF.weaken count view.owner.count bindings ();
              BF.preserve before view.store.semantic.origins count bindings subst ();
              BF.available_weaken count view.owner.count no bindings ());
            let #{value = third; state} = admit state no bindings subst () in
            ghost_ (
              let view = borrow_ state in
              F.weaken middle view.store.semantic.origins
                middle_count count ();
              F.compose before middle view.store.semantic.origins count ();
              F.at middle view.store.semantic.origins middle_count first ();
              S.origin_def middle first;
              S.origin_def view.store.semantic.origins first;
              F.at middle view.store.semantic.origins middle_count second ();
              S.origin_def middle second;
              S.origin_def view.store.semantic.origins second;
              ());
            (match third with
             | None -> #{value = None; state}
             | Some third ->
               let node = match pat with
                 | R.Int_if _ -> N.Int_if (first, second, third)
                 | _ -> N.Bool_if (first, second, third) in
               let {owner; store} = state in
               ghost_ (
                 if not (L.sort expr === None) then (
                   let expected = match pat with
                     | R.Int_if _ -> L.Integer | _ -> L.Boolean in
                   V.child_typed store first L.Boolean ();
                   V.child_typed store second expected ();
                   V.child_typed store third expected ());
                 N.well_typed_def store.sorts store.semantic.union.count node);
            let state = {owner; store} in
               let middle = ghost_ (
                 let view = borrow_ state in view.store.semantic.origins) in
               let middle_count = ghost_ (
                 let view = borrow_ state in view.owner.count) in
               let #{value; state} = add state node in
               ghost_ (
                 let view = borrow_ state in
                 F.weaken middle view.store.semantic.origins
                   middle_count count ();
                 F.compose before middle view.store.semantic.origins count ();
              F.at middle view.store.semantic.origins middle_count first ();
              S.origin_def middle first;
              S.origin_def view.store.semantic.origins first;
              F.at middle view.store.semantic.origins middle_count second ();
              S.origin_def middle second;
              S.origin_def view.store.semantic.origins second;
              F.at middle view.store.semantic.origins middle_count third ();
              S.origin_def middle third;
              S.origin_def view.store.semantic.origins third;
              N.origin_def view.store.semantic.origins node;
              ());
               let {owner; store} = state in
      ghost_ (
        let view = borrow_ owner in
        if view.count = count then
          match value with None -> () | Some id -> SI.constructor store pat bindings id node ());
      #{value; state = {owner; store}})))
