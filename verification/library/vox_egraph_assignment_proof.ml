module C = Vox_egraph_closure_spec
module S = Vox_egraph_saturation_spec
module B = Vox_egraph_quantifier
module A = Vox_egraph_assignment_spec
module Q = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec
module L = Vox_egraph_language_spec

let[@def] rec (concat @ total) (first : int list @ immutable) (second : int list @ immutable) =
  match first with [] -> second | head :: rest -> head :: concat rest second

let rec (concat_empty @ total) : (prefix : int list) @ immutable ->
    {u : unit | concat prefix [] === prefix} @ ghost = fun prefix -> ghost_ (
  concat_def prefix [];
  match prefix with [] -> () | _ :: rest -> concat_empty rest)

let rec (concat_snoc @ total) : (prefix : int list) @ immutable -> (id : int) ->
    (suffix : int list) @ immutable ->
    {u : unit | concat prefix (id :: suffix) === concat (C.snoc prefix id) suffix}
      @ ghost = fun prefix id suffix -> ghost_ (
  concat_def prefix (id :: suffix);
  C.snoc_def prefix id;
  concat_def (C.snoc prefix id) suffix;
  match prefix with [] -> concat_def [] suffix | _ :: rest -> concat_snoc rest id suffix)

let[@def] rec (closed_prefixed @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (prefix : int list @ immutable) (cases : A.cases @ immutable) =
  match cases with
  | A.End -> true
  | A.Case (ids, rest) -> C.closed_roots graph rule (concat prefix ids) graph.count &&
      closed_prefixed graph rule prefix rest

let rec (append @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable ->
    (prefix : int list) @ immutable ->
    (first : A.cases) @ immutable ->
    (second : A.cases) @ immutable ->
    {u : unit | closed_prefixed graph rule prefix (A.append first second) =
      (closed_prefixed graph rule prefix first && closed_prefixed graph rule prefix second)}
      @ ghost = fun graph rule prefix first second -> ghost_ (
  A.append_def first second;
  closed_prefixed_def graph rule prefix first;
  closed_prefixed_def graph rule prefix (A.append first second);
  match first with A.End -> () | A.Case (_, rest) -> append graph rule prefix rest second)

let rec (prepend @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable ->
    (prefix : int list) @ immutable ->
    (id : int) ->
    (cases : A.cases) @ immutable ->
    {u : unit | closed_prefixed graph rule prefix (A.prepend id cases) =
      closed_prefixed graph rule (C.snoc prefix id) cases} @ ghost = fun graph rule prefix id cases -> ghost_ (
  A.prepend_def id cases;
  closed_prefixed_def graph rule prefix (A.prepend id cases);
  closed_prefixed_def graph rule (C.snoc prefix id) cases;
  match cases with
  | A.End -> ()
  | A.Case (ids, rest) -> concat_snoc prefix id ids; prepend graph rule prefix id rest)

let rec (assignments @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable -> (vars : L.sort list) @ immutable ->
    (prefix : int list) @ immutable ->
    {u : unit | closed_prefixed graph rule prefix (A.assignments graph.count vars) =
      B.closed_bindings graph rule vars prefix graph.count} @ ghost =
  fun graph rule vars prefix -> ghost_ (
  A.assignments_def graph.count vars;
  match vars with
  | [] ->
    B.closed_bindings_def graph rule vars prefix graph.count;
    closed_prefixed_def graph rule prefix (A.Case ([], A.End));
    closed_prefixed_def graph rule prefix A.End;
    concat_empty prefix
  | _ :: rest ->
    let rec (choices @ total) :
        (count : int) ->
        {u : unit | closed_prefixed graph rule prefix
          (A.choices count (A.assignments graph.count rest)) =
          B.closed_bindings graph rule vars prefix count} @ ghost = fun count -> ghost_ (
      let cases = A.assignments graph.count rest in
      B.closed_bindings_def graph rule vars prefix count;
      A.choices_def count cases;
      if count <= 0 then (
        prepend graph rule prefix (-1) cases;
        assignments graph rule rest (C.snoc prefix (-1));
        B.closed_bindings_def graph rule rest (C.snoc prefix (-1)) graph.count)
      else (
        prepend graph rule prefix (count - 1) cases;
        assignments graph rule rest (C.snoc prefix (count - 1));
        choices (count - 1);
        B.closed_bindings_def graph rule rest (C.snoc prefix (count - 1)) graph.count;
        B.closed_bindings_def graph rule vars prefix (count - 1);
        append graph rule prefix (A.prepend (count - 1) cases) (A.choices (count - 1) cases)))
      [@@decreases if count > 0 then count else 0]
    in choices graph.count)

let rec (unprefixed @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable ->
    (cases : A.cases) @ immutable ->
    {u : unit | closed_prefixed graph rule [] cases = A.closed_cases graph rule cases}
      @ ghost = fun graph rule cases -> ghost_ (
  closed_prefixed_def graph rule [] cases;
  A.closed_cases_def graph rule cases;
  match cases with
  | A.End -> ()
  | A.Case (ids, rest) -> concat_def [] ids; unprefixed graph rule rest)

let (closed_rule @ total) : (graph : Q.graph) @ immutable ->
    (rule : R.rule) @ immutable ->
    {u : unit | S.closed_rule graph rule =
      A.closed_cases graph rule (A.assignments graph.count rule.vars)} @ ghost = fun graph rule -> ghost_ (
  S.closed_rule_def graph rule;
  assignments graph rule rule.vars [];
  unprefixed graph rule (A.assignments graph.count rule.vars))
