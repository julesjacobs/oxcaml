module I = Vox_iarray
module F = Vox_egraph_origin_frame
module K = Vox_egraph_key
module N = Vox_egraph_rule_node
module O = Vox_egraph_owner
module V = Vox_egraph_rule_store
module A = Borrow_iarray.Owned_array
module P = Ghost_pref
module H = P.Heap
module T = Vox_table_storage
module S = Vox_egraph_rule_semantics
module G = Vox_egraph_rule_union
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation
module U = Vox_egraph_union
module M = Vox_egraph_union_spec
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof

let[@def] (slot_matches @ total)
    (cells : K.t option iarray @ immutable)
    (nodes : N.t option iarray @ immutable) (id : int) = ghost_ (
  match I.at cells id, I.at nodes id with
  | Some (Some key), Some (Some node) -> key === N.key node
  | _ -> false)

let[@def] rec (matching @ total)
    (cells : K.t option iarray @ immutable)
    (nodes : N.t option iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else slot_matches cells nodes (count - 1) &&
    matching cells nodes (count - 1))
  [@@decreases if count > 0 then count else 0]

let rec (matching_at @ total) :
    (cells : K.t option iarray) @ immutable ->
    (nodes : N.t option iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | matching cells nodes count && 0 <= id && id < count} ->
    {u : unit | slot_matches cells nodes id} @ ghost =
  fun cells nodes count id premise -> ghost_ (
    matching_def cells nodes count;
    if id < count - 1 then matching_at cells nodes (count - 1) id ();
    ())
  [@@decreases if count > 0 then count else 0]

let rec (matching_frame @ total) :
    (cells : K.t option iarray) @ immutable ->
    (nodes : N.t option iarray) @ immutable ->
    (count : int) -> (index : int) ->
    (node : N.t) @ immutable ->
    {u : unit | 0 <= count && count <= index &&
      matching cells nodes count} ->
    {u : unit | matching
      (I.updated cells index (Some (N.key node)))
      (I.updated nodes index (Some node)) count} @ ghost =
  fun cells nodes count index node premise -> ghost_ (
    let next_cells = I.updated cells index (Some (N.key node)) in
    let next_nodes = I.updated nodes index (Some node) in
    matching_def cells nodes count;
    matching_def next_cells next_nodes count;
    if count > 0 then (
      let id = count - 1 in
      slot_matches_def cells nodes id;
      slot_matches_def next_cells next_nodes id;
      I.updated_read cells index (Some (N.key node)) id;
      I.updated_read nodes index (Some node) id;
      matching_frame cells nodes (count - 1) index node ());
    ())
  [@@decreases if count > 0 then count else 0]

let (matching_append @ total) :
    (cells : K.t option iarray) @ immutable ->
    (nodes : N.t option iarray) @ immutable ->
    (count : int) -> (node : N.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length cells &&
      count < Iarray.length nodes && matching cells nodes count} ->
    {u : unit | matching
      (I.updated cells count (Some (N.key node)))
      (I.updated nodes count (Some node)) (count + 1)} @ ghost =
  fun cells nodes count node premise -> ghost_ (
    let next_cells = I.updated cells count (Some (N.key node)) in
    let next_nodes = I.updated nodes count (Some node) in
    matching_frame cells nodes count count node ();
    I.updated_read cells count (Some (N.key node)) count;
    I.updated_read nodes count (Some node) count;
    slot_matches_def next_cells next_nodes count;
    matching_def next_cells next_nodes (count + 1);
    ())

let (lookup_origin @ total) :
    (cells : K.t option iarray) @ immutable ->
    (nodes : N.t option iarray) @ immutable ->
    (sorts : Vox_egraph_language_spec.sort iarray) @ immutable ->
    (origins : Vox_egraph_language_spec.expr iarray) @ immutable ->
    (count : int) -> (id : int) -> (node : N.t) @ immutable ->
    {u : unit | matching cells nodes count &&
      V.nodes_valid_data nodes sorts origins count &&
      0 <= id && id < count &&
      I.at cells id === Some (Some (N.key node))} ->
    {u : unit | S.origin origins id === N.origin origins node &&
      I.at nodes id === Some (Some node)}
    @ ghost = fun cells nodes sorts origins count id node premise -> ghost_ (
  matching_at cells nodes count id ();
  V.node_at_data nodes sorts origins count id ();
  slot_matches_def cells nodes id;
  V.node_ok_data_def nodes sorts origins id;
  match I.at nodes id with
  | Some (Some stored) ->
    N.key_exact stored node;
    ()
  | _ -> ())

type t = {owner : O.t; store : V.t @@ aliased}

let[@def] valid (state : t @ immutable) =
  ghost_ (
    O.valid state.owner && V.valid state.store &&
    state.owner.count = state.store.semantic.union.count &&
    matching (A.contents state.owner.arena) state.store.nodes
      state.owner.count &&
    O.Memo.Spec.valid state.owner.view &&
    H.at (P.own state.owner.token) (T.location state.owner.memo) ===
      Some state.owner.view.model)

type result = #{value : int option @@ aliased; state : t}

let create : (rules : Vox_egraph_rule_spec.t) @ immutable ghost ->
    {state : t | O.valid state.owner && V.valid state.store &&
      state.store.semantic.rules === rules &&
      state.owner.count = state.store.semantic.union.count &&
      matching (A.contents state.owner.arena) state.store.nodes
        state.owner.count &&
      O.Memo.Spec.valid state.owner.view &&
      H.at (P.own state.owner.token) (T.location state.owner.memo) ===
        Some state.owner.view.model && state.owner.count = 0} @ unique =
  fun rules ->
    let owner = O.create () in
    let store = V.create rules in
    ghost_ (
      let cells = A.contents (borrow_ owner.arena) in
      matching_def cells store.nodes 0);
    let state = {owner; store} in
    state

let add : (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (node : N.t) @ immutable ->
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
       | None -> r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
         (state.owner.count = 512 ||
          not (N.well_typed state.store.sorts state.owner.count node))
       | Some id -> 0 <= id && id < r.#state.owner.count &&
         I.at r.#state.store.nodes id === Some (Some node) &&
         S.origin r.#state.store.semantic.origins id ===
           N.origin r.#state.store.semantic.origins node)}
    @ unique = fun state node ->
  let {owner; store} = state in
  let count = let borrowed = borrow_ owner in borrowed.count in
  ghost_ (F.identity store.semantic.origins count);
  if not (N.well_typed store.sorts count node) then
    let state = {owner; store} in #{value = None; state}
  else
    let key = N.key node in
    let #{O.value = found; state = owner} = O.lookup owner key in
    match found with
    | Some id ->
      ghost_ (
        let cells = A.contents (borrow_ owner.arena) in
        V.valid_def store;
        V.nodes_valid_def store count;
        lookup_origin cells store.nodes store.sorts
          store.semantic.origins count id node ());
      let state = {owner; store} in
      #{value = Some id; state}
    | None ->
      if count = 512 then
        let state = {owner; store} in
        #{value = None; state}
      else
        let cells = ghost_ (A.contents (borrow_ owner.arena)) in
        let nodes = ghost_ store.nodes in
        ghost_ (
          O.valid_def (borrow_ owner);
          V.valid_def store);
        let #{O.value = admitted; state = owner} = O.append owner key in
        let #{V.value = stored; state = store} = V.add store node in
        ghost_ (
          let _ : {u : unit | matching cells nodes count} = () in
          matching_append cells nodes count node ());
        ghost_ (
          let new_cells = A.contents (borrow_ owner.arena) in
          V.valid_def store;
          V.nodes_valid_def store (count + 1);
          lookup_origin new_cells store.nodes store.sorts
            store.semantic.origins (count + 1) count node ());
        let state = {owner; store} in
        #{value = admitted; state}

let rec admit_raw :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (expr : L.expr) @ immutable ->
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
       | None -> L.sort expr === None || r.#state.owner.count = 512
       | Some id -> 0 <= id && id < r.#state.owner.count &&
         S.origin r.#state.store.semantic.origins id === expr)} @ unique =
  fun state expr ->
    ghost_ (let view = borrow_ state in
      O.valid_def view.owner; L.sort_def expr);
    let before = ghost_ (
      let view = borrow_ state in view.store.semantic.origins) in
    let count = ghost_ (let view = borrow_ state in view.owner.count) in
    match expr with
    | L.Int_lit literal ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count (N.Int_lit literal));
      let state = {owner; store} in
      let #{value; state} = add state (N.Int_lit literal) in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins (N.Int_lit literal));
      #{value; state}
    | L.Bool_lit literal ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count (N.Bool_lit literal));
      let state = {owner; store} in
      let #{value; state} = add state (N.Bool_lit literal) in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins (N.Bool_lit literal));
      #{value; state}
    | L.Int_input ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count N.Int_input);
      let state = {owner; store} in
      let #{value; state} = add state N.Int_input in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins N.Int_input);
      #{value; state}
    | L.Bool_input ->
      let {owner; store} = state in
      ghost_ (N.well_typed_def store.sorts store.semantic.union.count N.Bool_input);
      let state = {owner; store} in
      let #{value; state} = add state N.Bool_input in
      ghost_ (
        let view = borrow_ state in
        N.origin_def view.store.semantic.origins N.Bool_input);
      #{value; state}
    | L.Add (left, right) | L.Eq_int (left, right) ->
      let #{value = first; state} = admit_raw state left in
      (match first with
       | None -> #{value = None; state}
       | Some first ->
         let middle = ghost_ (
           let view = borrow_ state in view.store.semantic.origins) in
         let middle_count = ghost_ (
           let view = borrow_ state in view.owner.count) in
         let #{value = second; state} = admit_raw state right in
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
            let node = match expr with
              | L.Add _ -> N.Add (first, second)
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
            #{value; state}))
    | L.Int_if (condition, yes, no)
    | L.Bool_if (condition, yes, no) ->
      let #{value = first; state} = admit_raw state condition in
      (match first with
       | None -> #{value = None; state}
       | Some first ->
         let middle = ghost_ (
           let view = borrow_ state in view.store.semantic.origins) in
         let middle_count = ghost_ (
           let view = borrow_ state in view.owner.count) in
         let #{value = second; state} = admit_raw state yes in
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
            let #{value = third; state} = admit_raw state no in
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
               let node = match expr with
                 | L.Int_if _ -> N.Int_if (first, second, third)
                 | _ -> N.Bool_if (first, second, third) in
               let {owner; store} = state in
               ghost_ (
                 if not (L.sort expr === None) then (
                   let expected = match expr with
                     | L.Int_if _ -> L.Integer | _ -> L.Boolean in
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
               #{value; state})))

let admit_expr :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (expr : L.expr) @ immutable ->
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
       | None -> L.sort expr === None || r.#state.owner.count = 512
       | Some id -> 0 <= id && id < r.#state.owner.count &&
         S.origin r.#state.store.semantic.origins id === expr)} @ unique =
  fun state expr -> admit_raw state expr

type merge_result = #{merged : bool; state : t}

let merge : (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (a : {i : int | 0 <= i && i < state.owner.count}) ->
    (b : {i : int | 0 <= i && i < state.owner.count}) ->
    (proof : {p : E.evidence | E.valid state.store.semantic.rules p &&
      E.left p === S.origin state.store.semantic.origins
        (U.larger (M.root state.store.semantic.union.parents a)
          (M.root state.store.semantic.union.parents b)) &&
      E.right p === S.origin state.store.semantic.origins
        (U.smaller (M.root state.store.semantic.union.parents a)
          (M.root state.store.semantic.union.parents b))})
      @ immutable ghost ->
    {r : merge_result | O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (if r.#merged then true else
        r.#state.store === state.store) &&
      M.root r.#state.store.semantic.union.parents a =
        M.root r.#state.store.semantic.union.parents b}
    @ unique = fun state a b proof ->
  let {owner; store} = state in
  ghost_ (V.valid_def store);
  let #{G.merged; state = semantic} = G.merge store.semantic a b proof in
  let next_store = {store with semantic} in
  ghost_ (
    V.valid_def store;
    V.valid_def next_store;
    V.nodes_valid_def store store.semantic.union.count;
    V.nodes_valid_def next_store semantic.union.count);
  let state = {owner; store = next_store} in
  #{merged; state}

let merge_nodes : (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (a : {i : int | 0 <= i && i < state.owner.count}) ->
    (b : {i : int | 0 <= i && i < state.owner.count}) ->
    (proof : {p : E.evidence | E.valid state.store.semantic.rules p &&
      E.left p === S.origin state.store.semantic.origins a &&
      E.right p === S.origin state.store.semantic.origins b})
      @ immutable ghost ->
    {r : merge_result | O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (if r.#merged then true else
        r.#state.store === state.store) &&
      M.root r.#state.store.semantic.union.parents a =
        M.root r.#state.store.semantic.union.parents b}
    @ unique = fun state a b proof ->
  let {owner; store} = state in
  ghost_ (V.valid_def store);
  let #{G.merged; state = semantic} =
    G.merge_nodes store.semantic a b proof in
  let next_store = {store with semantic} in
  ghost_ (
    V.valid_def store;
    V.valid_def next_store;
    V.nodes_valid_def store store.semantic.union.count;
    V.nodes_valid_def next_store semantic.union.count);
  let state = {owner; store = next_store} in
  #{merged; state}

let merge_collision :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (left : {i : int | 0 <= i && i < state.owner.count}) ->
    (right : {i : int | 0 <= i && i < state.owner.count}) ->
    {r : merge_result | O.valid r.#state.owner && V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (if r.#merged then true else
        r.#state.store === state.store) &&
      V.pair_closed r.#state.store left right} @ unique =
  fun state left right ->
    let {owner; store} = state in
    match I.at store.nodes left, I.at store.nodes right with
    | Some (Some first), Some (Some second) ->
      let parents = store.semantic.union.parents in
      let first_key = N.signature parents first in
      let second_key = N.signature parents second in
      if K.equal first_key second_key then
        let proof = ghost_ (
          K.exact first_key second_key;
          V.collision_def store left right;
          N.signature_def parents first;
          N.signature_def parents second;
          V.collision_evidence store left right ()) in
        let state = {owner; store} in
        let #{merged; state} = merge_nodes state left right proof in
        let {owner; store} = state in
        ghost_ (V.pair_closed_def store left right);
        let state = {owner; store} in
        #{merged; state}
      else (
        ghost_ (
          K.exact first_key second_key;
          V.collision_def store left right;
          V.pair_closed_def store left right);
        let state = {owner; store} in
        #{merged = false; state})
    | _ ->
      ghost_ (
        V.collision_def store left right;
        V.pair_closed_def store left right);
      let state = {owner; store} in
      #{merged = false; state}

type scan_result = #{changed : bool; complete : bool; state : t}

let rec (scan_pairs @ total) :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (left : {i : int | 0 <= i && i <= state.owner.count}) ->
    (right : {i : int | 0 <= i && i <= state.owner.count}) ->
    (fuel : int) ->
    {r : scan_result | O.valid r.#state.owner &&
      V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (if r.#changed then true else
        r.#state.store === state.store) &&
      (if r.#complete && not r.#changed then
        V.closed_fuel r.#state.store left right fuel else true)} @ unique =
  fun state left right fuel ->
    let count = let borrowed = borrow_ state in borrowed.owner.count in
    if left = count then (
      let {owner; store} = state in
      ghost_ (V.closed_fuel_def store left right fuel);
      let state = {owner; store} in
      #{changed = false; complete = true; state})
    else if fuel <= 0 then
      #{changed = false; complete = false; state}
    else if right = count then (
      let #{changed; complete; state} =
        scan_pairs state (left + 1) 0 (fuel - 1) in
      let {owner; store} = state in
      ghost_ (V.closed_fuel_def store left right fuel);
      let state = {owner; store} in
      #{changed; complete; state})
    else if left = right then (
      let #{changed; complete; state} =
        scan_pairs state left (right + 1) (fuel - 1) in
      let {owner; store} = state in
      ghost_ (V.pair_closed_def store left right;
        V.closed_fuel_def store left right fuel);
      let state = {owner; store} in
      #{changed; complete; state})
    else
      let #{merged; state} = merge_collision state left right in
      let {owner; store} = state in
      let pair_store = ghost_ store in
      let state = {owner; store} in
      let #{changed; complete; state} =
        scan_pairs state left (right + 1) (fuel - 1) in
      let {owner; store} = state in
      ghost_ (
        V.pair_closed_def pair_store left right;
        V.closed_fuel_def store left right fuel);
      let state = {owner; store} in
      #{changed = merged || changed; complete; state}
  [@@decreases if fuel > 0 then fuel else 0]

type rebuild_status = Stable_pass | Work_limit
type rebuild_result = #{status : rebuild_status; state : t}

let rec (rebuild @ total) :
    (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (passes : int) ->
    {r : rebuild_result | O.valid r.#state.owner &&
      V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      (match r.#status with
       | Stable_pass -> V.closed_fuel r.#state.store 0 0
           (512 * 513 + 1)
       | Work_limit -> true)} @ unique =
  fun state passes ->
    if passes <= 0 then #{status = Work_limit; state}
    else (
      ghost_ (
        let borrowed = borrow_ state in
        O.valid_def borrowed.owner);
      let #{changed; complete; state} =
        scan_pairs state 0 0 (512 * 513 + 1) in
      if not complete then #{status = Work_limit; state}
      else if changed then rebuild state (passes - 1)
      else #{status = Stable_pass; state})
  [@@decreases if passes > 0 then passes else 0]

type extract_result = #{expr : L.expr @@ aliased; state : t;
  proof : E.evidence @@ ghost}

let extract : (state : {s : t | O.valid s.owner && V.valid s.store &&
      s.owner.count = s.store.semantic.union.count &&
      matching (A.contents s.owner.arena) s.store.nodes s.owner.count &&
      O.Memo.Spec.valid s.owner.view &&
      H.at (P.own s.owner.token) (T.location s.owner.memo) ===
        Some s.owner.view.model}) @ unique ->
    (id : {i : int | 0 <= i && i < state.owner.count}) ->
    {r : extract_result | O.valid r.#state.owner &&
      V.valid r.#state.store &&
      r.#state.store.semantic.rules === state.store.semantic.rules &&
      r.#state.owner.count = state.owner.count &&
      r.#state.store.nodes === state.store.nodes &&
      r.#state.store.sorts === state.store.sorts &&
      r.#state.store.semantic.origins === state.store.semantic.origins &&
      r.#state.owner.count = r.#state.store.semantic.union.count &&
      matching (A.contents r.#state.owner.arena)
        r.#state.store.nodes r.#state.owner.count &&
      O.Memo.Spec.valid r.#state.owner.view &&
      H.at (P.own r.#state.owner.token)
        (T.location r.#state.owner.memo) ===
        Some r.#state.owner.view.model &&
      E.valid state.store.semantic.rules r.#proof &&
      E.left r.#proof === S.origin state.store.semantic.origins id &&
      E.right r.#proof === r.#expr}
    @ unique = fun state id ->
  let {owner; store} = state in
  ghost_ (
    V.valid_def store;
    G.valid_def store.semantic;
    U.valid_def store.semantic.union);
  let root = U.find store.semantic.union id in
  let expr = V.extract_origin store root in
  let proof = ghost_ (
    V.nodes_valid_def store store.semantic.union.count;
    V.node_at_data store.nodes store.sorts store.semantic.origins
      store.semantic.union.count id ();
    V.node_at_data store.nodes store.sorts store.semantic.origins
      store.semantic.union.count root ();
    V.node_ok_data_def store.nodes store.sorts store.semantic.origins id;
    V.node_ok_data_def store.nodes store.sorts store.semantic.origins root;
    M.root_spec store.semantic.union.parents
      store.semantic.union.count root ();
    M.root_def store.semantic.union.parents root;
    G.same_evidence store.semantic id root ()) in
  let state = {owner; store} in
  #{expr; state; proof}
