module D = Hm_declarative
module T = Hmc_templates
module V = Hm_interpreter_typing
module S = Hmc_source_semantics

let[@def] (close @ total) (env : V.value @ immutable) (term : D.term @ immutable) = match term with
  | D.Lambda body -> V.Closure (body, env)
  | D.Recursive body -> V.Recursive_closure (body, env)
  | _ -> V.Empty
let[@def] rec (environment @ total) (catalog : T.catalog @ immutable) = match catalog with
  | T.Empty -> V.Empty
  | T.Declare (d, earlier) -> let env = environment earlier in V.Bind (close env d.T.source, env)
let[@def] rec (append @ total) (locals : V.value @ immutable) (globals : V.value @ immutable) = match locals with
  | V.Bind (v, rest) -> V.Bind (v, append rest globals)
  | _ -> globals

let rec (lookup @ total) : (catalog : T.catalog) @ immutable -> (index : D.index) @ immutable ->
    (selected : T.selected) @ immutable -> {u : unit | T.selection catalog index === Some selected} ->
    {u : unit | S.lookup (environment catalog) index ===
      Some (close (environment selected.T.earlier) selected.T.definition.T.source)} @ ghost =
  fun catalog index selected premise -> ghost_ (
    T.selection_def catalog index; environment_def catalog; S.lookup_def (environment catalog) index;
    match catalog, index with T.Declare (_, rest), D.S i -> lookup rest i selected () | _ -> ())

let[@def] rec (startup_steps @ total) (catalog : T.catalog @ immutable) = match catalog with
  | T.Empty -> D.Z | T.Declare (_, rest) -> D.add (startup_steps rest) (D.S (D.S (D.S D.Z)))
let rec (initialize @ total) : (catalog : T.catalog) @ immutable -> (entry : D.term) @ immutable ->
    (k : S.continuation) @ immutable -> {u : unit | T.valid catalog} ->
    {u : unit | S.advance (startup_steps catalog) (S.Running (S.Evaluate (V.Empty, T.rebuild catalog entry), k))
      === S.Running (S.Evaluate (environment catalog, entry), k)} @ ghost = fun catalog entry k premise -> ghost_ (
  T.valid_def catalog; T.rebuild_def catalog entry; startup_steps_def catalog; environment_def catalog;
  match catalog with
  | T.Empty -> S.advance_def D.Z (S.Running (S.Evaluate (V.Empty, entry), k))
  | T.Declare (d, earlier) ->
    let term = D.Let (d.T.source, entry) in
    initialize earlier term k ();
    let start = S.Running (S.Evaluate (V.Empty, T.rebuild earlier term), k) in
    let env = environment earlier in
    let mid = S.Running (S.Evaluate (env, term), k) in
    let three = D.S (D.S (D.S D.Z)) in
    S.advance_add (startup_steps earlier) three start;
    T.definition_valid_def earlier d; Hmc_admission.callable_def d.T.source;
    close_def env d.T.source;
    S.step_def mid; S.advance_def three mid;
    let next = S.step mid in S.step_def next; S.advance_def (D.S (D.S D.Z)) next;
    let next = S.step next in S.step_def next; S.advance_def (D.S D.Z) next;
    S.advance_def D.Z (S.step next))
