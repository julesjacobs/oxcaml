module C = Vox_egraph_closure_spec
module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec

module Measure = Vox_egraph_quantifier_measure

let[@def] rec (quantify @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable -> (vars : L.sort list) @ immutable ->
    (prefix : int list) @ immutable ->
    (count : int) ->
    (budget : {b : Bigint.t | b = Measure.rank vars graph.count count}) @ ghost -> bool =
  fun graph rule vars prefix count budget ->
    ghost_ (Measure.length_def vars; Measure.nonnegative vars;
      Measure.rank_def vars graph.count count);
    match vars with
    | [] -> C.closed_roots graph rule prefix graph.count
    | _ :: rest ->
      ghost_ (Measure.nonnegative rest;
        Measure.rank_def rest graph.count graph.count;
        Measure.rank_nonnegative rest graph.count graph.count;
        Measure.rank_def vars graph.count (count - 1);
        Measure.rank_nonnegative vars graph.count (count - 1));
      if count <= 0 then quantify graph rule rest (C.snoc prefix (-1)) graph.count
        (ghost_ (Measure.rank rest graph.count graph.count))
      else quantify graph rule rest (C.snoc prefix (count - 1)) graph.count
        (ghost_ (Measure.rank rest graph.count graph.count)) &&
        quantify graph rule vars prefix (count - 1)
          (ghost_ (Measure.rank vars graph.count (count - 1)))
  [@@decreases budget]

let[@def] (run @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (vars : L.sort list @ immutable)
    (prefix : int list @ immutable) (count : int) =
  quantify graph rule vars prefix count (ghost_ (Measure.rank vars graph.count count))

let closed_bindings = run

let (closed_bindings_def @ total) : (graph : Q.graph) @ immutable -> (rule : R.rule) @ immutable ->
    (vars : L.sort list) @ immutable -> (prefix : int list) @ immutable -> (count : int) ->
    {u : unit | closed_bindings graph rule vars prefix count =
      (match vars with
       | [] -> C.closed_roots graph rule prefix graph.count
       | _ :: rest ->
         if count <= 0 then closed_bindings graph rule rest (C.snoc prefix (-1)) graph.count
         else closed_bindings graph rule rest (C.snoc prefix (count - 1)) graph.count &&
           closed_bindings graph rule vars prefix (count - 1))} @ ghost = fun graph rule vars prefix count -> ghost_ (

  run_def graph rule vars prefix count;
  quantify_def graph rule vars prefix count (Measure.rank vars graph.count count);
  match vars with
  | [] -> ()
  | _ :: rest ->
    if count <= 0 then run_def graph rule rest (C.snoc prefix (-1)) graph.count
    else (
      run_def graph rule rest (C.snoc prefix (count - 1)) graph.count;
      run_def graph rule vars prefix (count - 1)))
