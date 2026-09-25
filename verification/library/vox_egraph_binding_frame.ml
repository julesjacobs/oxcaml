module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module P = Vox_egraph_match_evidence
module F = Vox_egraph_origin_frame
module S = Vox_egraph_rule_semantics

let[@def] rec (bounded @ total) (count : int) (bindings : int list @ immutable) = ghost_ (
  match bindings with [] -> true | id :: rest -> id < count && bounded count rest)

let rec (weaken @ total) : (before : int) -> (after : int) ->
    (bindings : int list) @ immutable ->
    {u : unit | before <= after && bounded before bindings} ->
    {u : unit | bounded after bindings} @ ghost = fun before after bindings premise -> ghost_ (
  bounded_def before bindings;
  bounded_def after bindings;
  (match bindings with [] -> () | _ :: rest -> weaken before after rest ());
  ())

let rec (preserve @ total) :
    (before : L.expr iarray) @ immutable -> (after : L.expr iarray) @ immutable ->
    (count : int) -> (bindings : int list) @ immutable -> (subst : R.subst) @ immutable ->
    {u : unit | F.preserved before after count && bounded count bindings &&
      P.agrees before bindings subst} ->
    {u : unit | P.agrees after bindings subst} @ ghost =
  fun before after count bindings subst premise -> ghost_ (
    bounded_def count bindings;
    P.agrees_def before bindings subst;
    P.agrees_def after bindings subst;
    (match bindings, subst with
     | id :: ids, _ :: rest ->
       if id >= 0 then (
         F.at before after count id ();
         S.origin_def before id;
         S.origin_def after id);
       preserve before after count ids rest ()
     | _ -> ());
    ())

let[@def] rec (available @ total) (count : int) (pat : R.pat @ immutable)
    (bindings : int list @ immutable) = ghost_ (
  match pat with
  | R.Var index ->
    (match Q.binding bindings index with None -> false | Some id -> 0 <= id && id < count)
  | R.Add (a, b) | R.Eq_int (a, b) -> available count a bindings && available count b bindings
  | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
    available count c bindings && available count a bindings && available count b bindings
  | _ -> true)

let rec (available_weaken @ total) : (before : int) -> (after : int) ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    {u : unit | before <= after && available before pat bindings} ->
    {u : unit | available after pat bindings} @ ghost = fun before after pat bindings premise -> ghost_ (
  available_def before pat bindings;
  available_def after pat bindings;
  (match pat with
   | R.Add (a, b) | R.Eq_int (a, b) ->
     available_weaken before after a bindings (); available_weaken before after b bindings ()
   | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
     available_weaken before after c bindings ();
     available_weaken before after a bindings (); available_weaken before after b bindings ()
   | _ -> ());
  ())

module V = Vox_egraph_rule_store
module B = Vox_egraph_match_subst
module MB = Vox_egraph_match_bindings
module O = Vox_egraph_match_observation

let rec (accepted_bound @ total) : (state : V.t) @ immutable ->
    (vars : L.sort list) @ immutable -> (bindings : int list) @ immutable ->
    {u : unit | B.accepts state vars bindings} ->
    {u : unit | bounded state.semantic.union.count bindings} @ ghost =
  fun state vars bindings premise -> ghost_ (
    B.accepts_def state vars bindings;
    bounded_def state.semantic.union.count bindings;
    (match vars, bindings with
     | _ :: vars, _ :: bindings -> accepted_bound state vars bindings ()
     | _ -> ());
    ())

let rec (present_available @ total) : (state : V.t) @ immutable ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    {u : unit | MB.present (P.view state) pat bindings} ->
    {u : unit | available state.semantic.union.count pat bindings} @ ghost =
  fun state pat bindings premise -> ghost_ (
    let graph = P.view state in
    MB.present_def graph pat bindings;
    available_def state.semantic.union.count pat bindings;
    (match pat with
     | R.Var index ->
       MB.has_def graph bindings index;
       (match Q.binding bindings index with
        | None -> ()
        | Some id ->
          P.view_def state;
          O.observe_def state.nodes state.semantic.union.parents state.semantic.union.count;
          Q.class_id_def graph id)
     | R.Add (a, b) | R.Eq_int (a, b) ->
       present_available state a bindings (); present_available state b bindings ()
     | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
       present_available state c bindings ();
       present_available state a bindings (); present_available state b bindings ()
     | _ -> ());
    ())
