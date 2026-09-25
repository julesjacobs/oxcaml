module Q = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec

let rec (collect_contains @ total) :
    (graph : Q.graph) @ immutable -> (pat : R.pat) @ immutable ->
    (first : int list) @ immutable -> (second : int list) @ immutable ->
    (third : int list) @ immutable -> (count : int) ->
    (id : int) -> (label : int) -> (node : Q.node) @ immutable ->
    {u : unit | 0 <= id && id < count && Q.class_id graph id === Some label &&
      Q.node graph id === Some node && Q.layer graph pat first second third node} ->
    {u : unit | Q.member label (Q.collect graph pat first second third count)}
      @ ghost = fun graph pat first second third count id label node premise -> ghost_ (
  Q.collect_def graph pat first second third count;
  if id < count - 1 then
    collect_contains graph pat first second third (count - 1) id label node ();
  (match Q.node graph (count - 1), Q.class_id graph (count - 1) with
   | Some value, Some found ->
     if Q.layer graph pat first second third value then
       Q.member_def label (found :: Q.collect graph pat first second third (count - 1))
   | _ -> ());
  ())
  [@@decreases if count > 0 then count else 0]

let[@def] (first @ total) (graph : Q.graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable) = ghost_ (
  match pat with
  | R.Add (a, _) | R.Eq_int (a, _) | R.Int_if (a, _, _) | R.Bool_if (a, _, _) ->
    Q.classes graph a bindings
  | _ -> [])

let[@def] (second @ total) (graph : Q.graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable) = ghost_ (
  match pat with
  | R.Add (_, a) | R.Eq_int (_, a) | R.Int_if (_, a, _) | R.Bool_if (_, a, _) ->
    Q.classes graph a bindings
  | _ -> [])

let[@def] (third @ total) (graph : Q.graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable) = ghost_ (
  match pat with
  | R.Int_if (_, _, a) | R.Bool_if (_, _, a) -> Q.classes graph a bindings
  | _ -> [])

let (introduce @ total) :
    (graph : Q.graph) @ immutable -> (pat : R.pat) @ immutable ->
    (bindings : int list) @ immutable -> (id : int) -> (root : int) ->
    (node : Q.node) @ immutable ->
    {u : unit | 0 <= id && id < graph.count && Q.same graph root id &&
      Q.node graph id === Some node &&
      Q.layer graph pat (first graph pat bindings) (second graph pat bindings)
        (third graph pat bindings) node} ->
    {u : unit | Q.matches graph pat bindings root} @ ghost =
  fun graph pat bindings id root node premise -> ghost_ (
    first_def graph pat bindings;
    second_def graph pat bindings;
    third_def graph pat bindings;
    Q.layer_def graph pat (first graph pat bindings) (second graph pat bindings)
      (third graph pat bindings) node;
    Q.same_def graph root id;
    Q.matches_def graph pat bindings root;
    Q.in_classes_def graph root (Q.classes graph pat bindings);
    Q.classes_def graph pat bindings;
    (match Q.class_id graph id with
     | None -> ()
     | Some label ->
       collect_contains graph pat (first graph pat bindings) (second graph pat bindings)
         (third graph pat bindings) graph.count id label node ());
    ())
