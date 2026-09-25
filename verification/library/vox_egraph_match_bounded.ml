module Q = Vox_egraph_match_spec
module O = Vox_egraph_match_observation
module Scan = Vox_egraph_match_scan
module R = Vox_egraph_rule_spec
module M = Vox_egraph_union_spec
module B = Vox_egraph_match_budget_spec

let rec (positive @ total) : (count : {n : int | 0 <= n}) ->
    (pat : R.pat) @ immutable ->
    {u : unit | B.work count pat >= 1Z} @ ghost = fun count pat -> ghost_ (
  B.work_def count pat;
  (match pat with
   | R.Add (a, b) | R.Eq_int (a, b) -> positive count a; positive count b
   | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
     positive count c; positive count a; positive count b
   | _ -> ());
  ())

type result = Done of int list * int | Exhausted

let rec (classes @ total) :
    (nodes : Q.node option iarray) @ immutable ->
    (parents : int iarray) @ immutable ->
    (count : {n : int | 0 <= n && n <= Iarray.length nodes &&
      n <= Iarray.length parents}) ->
    (pat : R.pat) @ immutable -> (bindings : int list) @ immutable ->
    (fuel : int) ->
    {r : result | match r with
      | Exhausted -> Bigint.of_int fuel < B.work count pat
      | Done (found, remaining) -> 0 <= remaining && remaining < fuel &&
        Bigint.of_int fuel = Bigint.add (B.work count pat) (Bigint.of_int remaining) &&
        found === Q.classes (O.observe nodes parents count) pat bindings}
      @ immutable = fun nodes parents count pat bindings fuel ->
  ghost_ (positive count pat; B.work_def count pat);
  if fuel <= 0 then Exhausted
  else
    let graph = ghost_ (O.observe nodes parents count) in
    ghost_ (
      O.observe_def nodes parents count;
      Q.classes_def graph pat bindings);
    match pat with
    | R.Var _ ->
      let found = Scan.classes nodes parents count pat bindings in
      (Done (found, fuel - 1))
    | R.Add (left, right) | R.Eq_int (left, right) ->
      ghost_ (positive count left; positive count right);
      (match classes nodes parents count left bindings (fuel - 1) with
       | Exhausted -> Exhausted
       | Done (first, fuel1) ->
         match classes nodes parents count right bindings fuel1 with
         | Exhausted -> Exhausted
         | Done (second, fuel2) ->
           if fuel2 < count then Exhausted
           else
             let found = Scan.collect nodes parents count pat first second [] count in
             (Done (found, fuel2 - count)))
    | R.Int_if (condition, yes, no) | R.Bool_if (condition, yes, no) ->
      ghost_ (positive count condition; positive count yes; positive count no);
      (match classes nodes parents count condition bindings (fuel - 1) with
       | Exhausted -> Exhausted
       | Done (first, fuel1) ->
         match classes nodes parents count yes bindings fuel1 with
         | Exhausted -> Exhausted
         | Done (second, fuel2) ->
           match classes nodes parents count no bindings fuel2 with
           | Exhausted -> Exhausted
           | Done (third, fuel3) ->
             if fuel3 < count then Exhausted
             else
               let found = Scan.collect nodes parents count pat first second third count in
               (Done (found, fuel3 - count)))
    | R.Int_lit _ | R.Bool_lit _ | R.Int_input | R.Bool_input ->
      if fuel - 1 < count then Exhausted
      else
        let found = Scan.collect nodes parents count pat [] [] [] count in
        (Done (found, fuel - 1 - count))
