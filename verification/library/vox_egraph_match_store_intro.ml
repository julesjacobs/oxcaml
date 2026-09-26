module V = Vox_egraph_rule_store
module P = Vox_egraph_match_evidence
module Q = Vox_egraph_match_spec
module O = Vox_egraph_match_observation
module Intro = Vox_egraph_match_intro
module R = Vox_egraph_rule_spec
module I = Vox_iarray

let (constructor @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    (id : int) -> (node : Q.node) @ immutable ->
    {u : unit | V.valid state && 0 <= id && id < state.semantic.union.count &&
      I.at state.nodes id === Some (Some node) &&
      (match pat, node with
       | R.Int_lit a, Q.Int_lit b -> a = b
       | R.Bool_lit a, Q.Bool_lit b -> a = b
       | R.Int_input, Q.Int_input | R.Bool_input, Q.Bool_input -> true
       | R.Add (a, b), Q.Add (x, y) | R.Eq_int (a, b), Q.Eq_int (x, y) ->
         Q.matches (P.view state) a bindings x && Q.matches (P.view state) b bindings y
       | R.Int_if (c, a, b), Q.Int_if (z, x, y)
       | R.Bool_if (c, a, b), Q.Bool_if (z, x, y) ->
         Q.matches (P.view state) c bindings z &&
         Q.matches (P.view state) a bindings x && Q.matches (P.view state) b bindings y
       | _ -> false)} ->
    {u : unit | Q.matches (P.view state) pat bindings id} @ ghost =
  fun state pat bindings id node premise -> ghost_ (
    let graph = P.view state in
    P.bounds state ();
    P.view_def state;
    O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
    O.node_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
    O.same state.nodes state.semantic.union.parents state.semantic.union.count id id ();
    Intro.first_def graph pat bindings;
    Intro.second_def graph pat bindings;
    Intro.third_def graph pat bindings;
    Q.layer_def graph pat (Intro.first graph pat bindings)
      (Intro.second graph pat bindings) (Intro.third graph pat bindings) node;
    (match pat, node with
     | R.Add (a, b), Q.Add (x, y) | R.Eq_int (a, b), Q.Eq_int (x, y) ->
       Q.matches_def graph a bindings x;
       Q.matches_def graph b bindings y
     | R.Int_if (c, a, b), Q.Int_if (z, x, y)
     | R.Bool_if (c, a, b), Q.Bool_if (z, x, y) ->
       Q.matches_def graph c bindings z;
       Q.matches_def graph a bindings x;
       Q.matches_def graph b bindings y
     | _ -> ());
    Intro.introduce graph pat bindings id id node ())

let (variable @ total) : (state : V.t) @ immutable ->
    (bindings : int list) @ immutable -> (index : int) -> (id : int) ->
    {u : unit | V.valid state && 0 <= id && id < state.semantic.union.count &&
      Q.binding bindings index === Some id} ->
    {u : unit | Q.matches (P.view state) (R.Var index) bindings id} @ ghost =
  fun state bindings index id premise -> ghost_ (
    let graph = P.view state in
    P.bounds state ();
    P.view_def state;
    O.class_at state.nodes state.semantic.union.parents state.semantic.union.count id ();
    Q.matches_def graph (R.Var index) bindings id;
    Q.classes_def graph (R.Var index) bindings;
    Q.in_classes_def graph id (Q.classes graph (R.Var index) bindings);
    let label = V.M.root state.semantic.union.parents id in
    Q.member_def label [label];
    ())

let (same_match @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    (left : int) -> (right : int) ->
    {u : unit | V.valid state && 0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      V.M.root state.semantic.union.parents left = V.M.root state.semantic.union.parents right &&
      Q.matches (P.view state) pat bindings right} ->
    {u : unit | Q.matches (P.view state) pat bindings left} @ ghost =
  fun state pat bindings left right premise -> ghost_ (
    P.bounds state ();
    P.view_def state;
    O.class_at state.nodes state.semantic.union.parents state.semantic.union.count left ();
    O.class_at state.nodes state.semantic.union.parents state.semantic.union.count right ();
    let graph = P.view state in
    Q.matches_def graph pat bindings left;
    Q.matches_def graph pat bindings right;
    Q.in_classes_def graph left (Q.classes graph pat bindings);
    Q.in_classes_def graph right (Q.classes graph pat bindings);
    ())
