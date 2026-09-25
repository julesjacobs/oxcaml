module V = Vox_egraph_rule_store
module G = Vox_egraph_rule_union
module U = Vox_egraph_union
module S = Vox_egraph_rule_semantics
module M = Vox_egraph_union_spec
module N = Vox_egraph_rule_node
module I = Vox_iarray
module Q = Vox_egraph_match_spec
module O = Vox_egraph_match_observation
module Scan = Vox_egraph_match_scan
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation

let[@def] (view @ total) (state : V.t @ immutable) = ghost_ (
  O.observe state.nodes state.semantic.union.parents state.semantic.union.count)

let[@def] rec (agrees @ total) (origins : L.expr iarray @ immutable)
    (bindings : int list @ immutable) (subst : R.subst @ immutable) = ghost_ (
  match bindings, subst with
  | [], [] -> true
  | id :: ids, expr :: exprs ->
    (if id >= 0 then expr === S.origin origins id else true) &&
    agrees origins ids exprs
  | _ -> false)

let rec (agrees_at @ total) :
    (origins : L.expr iarray) @ immutable ->
    (bindings : int list) @ immutable -> (subst : R.subst) @ immutable ->
    (index : int) -> (id : int) ->
    {u : unit | agrees origins bindings subst &&
      Q.binding bindings index === Some id && 0 <= id} ->
    {u : unit | R.lookup_expr subst index === S.origin origins id}
    @ ghost = fun origins bindings subst index id premise -> ghost_ (
  agrees_def origins bindings subst;
  Q.binding_def bindings index;
  R.lookup_expr_def subst index;
  (match bindings, subst with
   | _ :: ids, _ :: exprs ->
     if index > 0 then agrees_at origins ids exprs (index - 1) id ()
   | _ -> ());
  ())

let (bounds @ total) : (state : V.t) @ immutable ->
    {u : unit | V.valid state} ->
    {u : unit | 0 <= state.semantic.union.count &&
      state.semantic.union.count <= Iarray.length state.nodes &&
      state.semantic.union.count <= Iarray.length state.semantic.union.parents}
      @ ghost = fun state premise -> ghost_ (
  V.valid_def state;
  G.valid_def state.semantic;
  U.valid_def state.semantic.union;
  ())

let (matched_id @ total) : (state : V.t) @ immutable ->
    (id : int) -> (classes : int list) @ immutable ->
    {u : unit | V.valid state && Q.in_classes (view state) id classes} ->
    {u : unit | 0 <= id && id < state.semantic.union.count &&
      Q.member (M.root state.semantic.union.parents id) classes} @ ghost =
  fun state id classes premise -> ghost_ (
    bounds state ();
    view_def state;
    Q.in_classes_def (view state) id classes;
    O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
    Q.class_id_def (view state) id;
    O.class_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
    ())

let (origin_typed @ total) : (state : V.t) @ immutable -> (id : int) ->
    {u : unit | V.valid state && 0 <= id && id < state.semantic.union.count} ->
    {u : unit | not (L.sort (S.origin state.semantic.origins id) === None)}
      @ ghost = fun state id premise -> ghost_ (
  V.valid_def state;
  V.nodes_valid_def state state.semantic.union.count;
  V.node_at_data state.nodes state.sorts state.semantic.origins
    state.semantic.union.count id ();
  V.node_ok_data_def state.nodes state.sorts state.semantic.origins id;
  ())

let (same @ total) : (state : V.t) @ immutable -> (left : int) -> (right : int) ->
    {u : unit | V.valid state && 0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      M.root state.semantic.union.parents left = M.root state.semantic.union.parents right} ->
    {p : E.evidence | E.valid state.semantic.rules p &&
      E.left p === S.origin state.semantic.origins left &&
      E.right p === S.origin state.semantic.origins right} @ ghost =
  fun state left right premise -> ghost_ (
    V.valid_def state;
    origin_typed state left ();
    origin_typed state right ();
    G.same_evidence state.semantic left right ())

let (child_sorted @ total) : (state : V.t) @ immutable ->
    (limit : int) -> (id : int) -> (sort : L.sort) ->
    {u : unit | V.valid state && limit <= state.semantic.union.count &&
      N.child_has_sort state.sorts limit id sort} ->
    {u : unit | L.sort (S.origin state.semantic.origins id) === Some sort}
      @ ghost = fun state limit id sort premise -> ghost_ (
  V.valid_def state;
  V.nodes_valid_def state state.semantic.union.count;
  N.child_has_sort_def state.sorts limit id sort;
  N.child_has_sort_def state.sorts state.semantic.union.count id sort;
  V.child_origin_sort state.nodes state.sorts state.semantic.origins
    state.semantic.union.count id sort ())

let (node_facts @ total) : (state : V.t) @ immutable ->
    (id : int) -> (node : Q.node) @ immutable ->
    {u : unit | V.valid state && 0 <= id && id < state.semantic.union.count &&
      Q.node (view state) id === Some node} ->
    {u : unit | S.origin state.semantic.origins id === N.origin state.semantic.origins node &&
      N.children_below state.semantic.union.count node &&
      N.child_origins_typed state.semantic.origins node}
      @ ghost = fun state id node premise -> ghost_ (
  bounds state ();
  view_def state;
  O.node_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
  V.valid_def state;
  V.nodes_valid_def state state.semantic.union.count;
  V.node_at_data state.nodes state.sorts state.semantic.origins
    state.semantic.union.count id ();
  V.node_ok_data_def state.nodes state.sorts state.semantic.origins id;
  N.children_below_def id node;
  N.children_below_def state.semantic.union.count node;
  N.well_typed_def state.sorts id node;
  N.child_origins_typed_def state.semantic.origins node;
  (match node with
   | Q.Int_lit _ | Q.Bool_lit _ | Q.Int_input | Q.Bool_input -> ()
   | Q.Add (a, b) | Q.Eq_int (a, b) ->
     child_sorted state id a L.Integer ();
     child_sorted state id b L.Integer ()
   | Q.Int_if (c, a, b) ->
     child_sorted state id c L.Boolean ();
     child_sorted state id a L.Integer ();
     child_sorted state id b L.Integer ()
   | Q.Bool_if (c, a, b) ->
     child_sorted state id c L.Boolean ();
     child_sorted state id a L.Boolean ();
     child_sorted state id b L.Boolean ());
  ())

let (witness @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (first : int list) @ immutable ->
    (second : int list) @ immutable -> (third : int list) @ immutable ->
    (root : int) ->
    {u : unit | V.valid state &&
      Q.member (M.root state.semantic.union.parents root)
        (Q.collect (view state) pat first second third state.semantic.union.count)} ->
    {id : int | 0 <= id && id < state.semantic.union.count &&
      M.root state.semantic.union.parents id = M.root state.semantic.union.parents root &&
      (match Q.node (view state) id with None -> false | Some node ->
        Q.layer (view state) pat first second third node)} @ ghost =
  fun state pat first second third root premise -> ghost_ (
    bounds state ();
    view_def state;
    match Scan.find_layer state.nodes state.semantic.union.parents
      state.semantic.union.count pat first second third
      (M.root state.semantic.union.parents root) state.semantic.union.count with
    | None -> (-1)
    | Some id ->
      O.class_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
      id)

let rec (derive @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    (subst : R.subst) @ immutable -> (root : int) ->
    {u : unit | V.valid state && agrees state.semantic.origins bindings subst &&
      Q.matches (view state) pat bindings root} ->
    {p : E.evidence | E.valid state.semantic.rules p &&
      E.left p === S.origin state.semantic.origins root &&
      E.right p === R.instantiate pat subst} @ ghost =
  fun state pat bindings subst root premise -> ghost_ (
    let graph = view state in
    let origins = state.semantic.origins in
    let rules = state.semantic.rules in
    Q.matches_def graph pat bindings root;
    matched_id state root (Q.classes graph pat bindings) ();
    Q.classes_def graph pat bindings;
    R.instantiate_def pat subst;
    view_def state;
    O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
    match pat with
    | R.Var index ->
      (match Q.binding bindings index with
       | None ->
         Q.member_def (M.root state.semantic.union.parents root) [];
         (E.Refl (S.origin origins root))
       | Some id ->
         match Q.class_id graph id with
         | None ->
           Q.member_def (M.root state.semantic.union.parents root) [];
           (E.Refl (S.origin origins root))
         | Some label ->
           Q.member_def (M.root state.semantic.union.parents root) [label];
           Q.member_def (M.root state.semantic.union.parents root) [];
           Q.class_id_def graph id;
           bounds state ();
           O.class_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
           agrees_at origins bindings subst index id ();
           same state root id ())
    | _ ->
      let first, second, third = match pat with
        | R.Add (a, b) | R.Eq_int (a, b) ->
          Q.classes graph a bindings, Q.classes graph b bindings, []
        | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
          Q.classes graph c bindings, Q.classes graph a bindings, Q.classes graph b bindings
        | _ -> [], [], [] in
      let id = witness state pat first second third root () in
      match Q.node graph id with
      | None -> (E.Refl (S.origin origins root))
      | Some node ->
        node_facts state id node ();
        N.origin_def origins node;
        N.children_below_def state.semantic.union.count node;
        N.child_origins_typed_def origins node;
        Q.layer_def graph pat first second third node;
        let proof = match pat, node with
          | R.Add (a, b), Q.Add (x, y) ->
            Q.matches_def graph a bindings x;
            Q.matches_def graph b bindings y;
            let pa = derive state a bindings subst x () in
            let pb = derive state b bindings subst y () in
            EP.add_congruence rules pa pb ()
          | R.Eq_int (a, b), Q.Eq_int (x, y) ->
            Q.matches_def graph a bindings x;
            Q.matches_def graph b bindings y;
            let pa = derive state a bindings subst x () in
            let pb = derive state b bindings subst y () in
            EP.eq_congruence rules pa pb ()
          | R.Int_if (c, a, b), Q.Int_if (z, x, y) ->
            Q.matches_def graph c bindings z;
            Q.matches_def graph a bindings x;
            Q.matches_def graph b bindings y;
            let pc = derive state c bindings subst z () in
            let pa = derive state a bindings subst x () in
            let pb = derive state b bindings subst y () in
            EP.int_if_congruence rules pc pa pb ()
          | R.Bool_if (c, a, b), Q.Bool_if (z, x, y) ->
            Q.matches_def graph c bindings z;
            Q.matches_def graph a bindings x;
            Q.matches_def graph b bindings y;
            let pc = derive state c bindings subst z () in
            let pa = derive state a bindings subst x () in
            let pb = derive state b bindings subst y () in
            EP.bool_if_congruence rules pc pa pb ()
          | _ ->
            let proof = E.Refl (S.origin origins id) in
            origin_typed state id ();
            E.valid_def rules proof;
            E.left_def proof;
            E.right_def proof;
            E.endpoints_def proof;
            proof in
        let prefix = same state root id () in
        EP.transitive rules prefix proof ())
