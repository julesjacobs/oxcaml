module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec

type node =
  | Int_lit of int
  | Bool_lit of bool
  | Int_input
  | Bool_input
  | Add of int * int
  | Eq_int of int * int
  | Int_if of int * int * int
  | Bool_if of int * int * int
[@@inductive]

type graph = {
  count : int;
  nodes : node option iarray;
  classes : int iarray;
}

let[@def] (node @ total) (graph : graph @ immutable) (id : int) =
  if 0 <= id && id < graph.count && id < Iarray.length graph.nodes then
    Iarray.Refined.get graph.nodes (id)
  else None

let[@def] (class_id @ total) (graph : graph @ immutable) (id : int) =
  if 0 <= id && id < graph.count && id < Iarray.length graph.classes then
    Some (Iarray.Refined.get graph.classes (id))
  else None

let[@def] (same @ total) (graph : graph @ immutable) (a : int) (b : int) =
  match class_id graph a, class_id graph b with
  | Some x, Some y -> x = y
  | _ -> false

let[@def] rec (binding @ total) (bindings : int list @ immutable)
    (index : int) =
  match bindings with
  | [] -> None
  | id :: rest ->
    if index < 0 then None
    else if index = 0 then Some id
    else binding rest (index - 1)

let[@def] rec (member @ total) (value : int)
    (values : int list @ immutable) =
  match values with [] -> false | x :: rest -> value = x || member value rest

let[@def] (in_classes @ total) (graph : graph @ immutable) (id : int)
    (classes : int list @ immutable) =
  match class_id graph id with None -> false | Some c -> member c classes

let[@def] (layer @ total) (graph : graph @ immutable)
    (pat : R.pat @ immutable) (first : int list @ immutable)
    (second : int list @ immutable) (third : int list @ immutable)
    (value : node @ immutable) =
  match pat, value with
  | R.Int_lit a, Int_lit b -> a = b
  | R.Bool_lit a, Bool_lit b -> a = b
  | R.Int_input, Int_input | R.Bool_input, Bool_input -> true
  | R.Add _, Add (a, b) | R.Eq_int _, Eq_int (a, b) ->
    in_classes graph a first && in_classes graph b second
  | R.Int_if _, Int_if (c, a, b) | R.Bool_if _, Bool_if (c, a, b) ->
    in_classes graph c first && in_classes graph a second &&
    in_classes graph b third
  | _ -> false

let[@def] rec (collect @ total) (graph : graph @ immutable)
    (pat : R.pat @ immutable) (first : int list @ immutable)
    (second : int list @ immutable) (third : int list @ immutable)
    (count : int) =
  if count <= 0 then []
  else
    let rest = collect graph pat first second third (count - 1) in
    match node graph (count - 1), class_id graph (count - 1) with
    | Some value, Some label ->
      if layer graph pat first second third value then label :: rest else rest
    | _ -> rest
  [@@decreases if count > 0 then count else 0]

let[@def] rec (classes @ total) (graph : graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable) =
  match pat with
  | R.Var index ->
    (match binding bindings index with
     | None -> []
     | Some id ->
       match class_id graph id with None -> [] | Some label -> [label])
  | R.Add (left, right) | R.Eq_int (left, right) ->
    collect graph pat (classes graph left bindings)
      (classes graph right bindings) [] graph.count
  | R.Int_if (condition, yes, no) | R.Bool_if (condition, yes, no) ->
    collect graph pat (classes graph condition bindings)
      (classes graph yes bindings) (classes graph no bindings) graph.count
  | R.Int_lit _ | R.Bool_lit _ | R.Int_input | R.Bool_input ->
    collect graph pat [] [] [] graph.count

let[@def] (matches @ total) (graph : graph @ immutable)
    (pat : R.pat @ immutable) (bindings : int list @ immutable)
    (root : int) = in_classes graph root (classes graph pat bindings)
