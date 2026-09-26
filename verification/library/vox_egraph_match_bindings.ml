module Q = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec
module P = Vox_egraph_match_evidence
module V = Vox_egraph_rule_store
module O = Vox_egraph_match_observation
module Intro = Vox_egraph_match_intro

let[@def] (has @ total) (graph : Q.graph @ immutable)
    (bindings : int list @ immutable) (index : int) = ghost_ (
  match Q.binding bindings index with
  | None -> false
  | Some id -> match Q.class_id graph id with None -> false | Some _ -> true)

let[@def] rec (present @ total) (graph : Q.graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable) = ghost_ (
  match pat with
  | R.Var index -> has graph bindings index
  | R.Add (a, b) | R.Eq_int (a, b) -> present graph a bindings && present graph b bindings
  | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
    present graph c bindings && present graph a bindings && present graph b bindings
  | _ -> true)

let rec (at @ total) : (graph : Q.graph) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable -> (index : int) ->
    {u : unit | present graph pat bindings && R.occurs index pat} ->
    {u : unit | has graph bindings index} @ ghost = fun graph pat bindings index premise -> ghost_ (
  present_def graph pat bindings;
  R.occurs_def index pat;
  (match pat with
   | R.Var _ -> ()
   | R.Add (a, b) | R.Eq_int (a, b) ->
     if R.occurs index a then at graph a bindings index () else at graph b bindings index ()
   | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
     if R.occurs index c then at graph c bindings index ()
     else if R.occurs index a then at graph a bindings index () else at graph b bindings index ()
   | _ -> ());
  ())

let rec (transfer @ total) : (graph : Q.graph) @ immutable ->
    (lhs : R.pat) @ immutable -> (rhs : R.pat) @ immutable ->
    (bindings : int list) @ immutable ->
    {u : unit | present graph lhs bindings && R.vars_in rhs lhs} ->
    {u : unit | present graph rhs bindings} @ ghost = fun graph lhs rhs bindings premise -> ghost_ (
  present_def graph rhs bindings;
  R.vars_in_def rhs lhs;
  (match rhs with
   | R.Var index -> at graph lhs bindings index ()
   | R.Add (a, b) | R.Eq_int (a, b) -> transfer graph lhs a bindings (); transfer graph lhs b bindings ()
   | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
     transfer graph lhs c bindings (); transfer graph lhs a bindings (); transfer graph lhs b bindings ()
   | _ -> ());
  ())

let rec (matched @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable -> (root : int) ->
    {u : unit | V.valid state && Q.matches (P.view state) pat bindings root} ->
    {u : unit | present (P.view state) pat bindings} @ ghost =
  fun state pat bindings root premise -> ghost_ (
    let graph = P.view state in
    present_def graph pat bindings;
    Q.matches_def graph pat bindings root;
    P.matched_id state root (Q.classes graph pat bindings) ();
    Q.classes_def graph pat bindings;
    P.view_def state;
    O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
    match pat with
    | R.Var index ->
      has_def graph bindings index;
      Q.member_def (V.M.root state.semantic.union.parents root) [];
      ()
    | R.Int_lit _ | R.Bool_lit _ | R.Int_input | R.Bool_input -> ()
    | _ ->
      let first = Intro.first graph pat bindings in
      let second = Intro.second graph pat bindings in
      let third = Intro.third graph pat bindings in
      Intro.first_def graph pat bindings;
      Intro.second_def graph pat bindings;
      Intro.third_def graph pat bindings;
      let id = P.witness state pat first second third root () in
      (match Q.node graph id with
       | None -> ()
       | Some node ->
         Q.layer_def graph pat first second third node;
         match pat, node with
         | R.Add (a, b), Q.Add (x, y) | R.Eq_int (a, b), Q.Eq_int (x, y) ->
           Q.matches_def graph a bindings x;
           Q.matches_def graph b bindings y;
           matched state a bindings x ();
           matched state b bindings y ()
         | R.Int_if (c, a, b), Q.Int_if (z, x, y)
         | R.Bool_if (c, a, b), Q.Bool_if (z, x, y) ->
           Q.matches_def graph c bindings z;
           Q.matches_def graph a bindings x;
           Q.matches_def graph b bindings y;
           matched state c bindings z ();
           matched state a bindings x ();
           matched state b bindings y ()
         | _ -> ());
      ())
