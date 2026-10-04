open Mode_solver_semantics
open Mode_solver_graph_semantics
open Mode_solver_guarded_semantics

let (regionality_adjunction @ total) :
    (a : elt) -> (b : elt) ->
    {u : unit | le (regional_to_local a) b = le a (regional_to_global b)}
    @ ghost =
 fun a b -> ghost_ (Mode_solver_three_qe_proof.regionality_adjunction a b)

let[@def] (eliminate @ total) f = Mode_solver_three_qe_proof.eliminate f

let (eliminate_exact @ total) :
    (env : elt list) -> (f : formula) ->
    {u : unit | eval_qf env (eliminate f) = eval env f} @ ghost =
 fun env f -> ghost_ (
  eliminate_def f;
  Mode_solver_three_qe_proof.eliminate_exact env f;
  ())

let (eliminate_scoped @ total) :
    (depth : int) -> (f : {f : formula | scoped depth f}) ->
    {u : unit | scoped_qf depth (eliminate f)} @ ghost =
 fun depth f -> ghost_ (
  eliminate_def f;
  Mode_solver_three_qe_proof.eliminate_scoped depth f;
  ())

let (decide_checked @ total) (f : formula) :
    {answer : bool option |
      match answer with
      | None -> not (scoped 0 f)
      | Some value -> scoped 0 f && value = eval [] f} =
  if scoped 0 f then begin
    ghost_ (eliminate_exact [] f);
    Some (eval_qf [] (eliminate f))
  end else None

let[@def] (project_graph @ total) (count : unit list) graph =
  Mode_solver_graph_qe_proof.project_many count graph

let (project_graph_exact @ total) :
    (count : unit list) -> (env : elt list) -> (graph : graph) ->
    {u : unit | eval_qf env (project_graph count graph) = models_exists count env graph}
    @ ghost =
 fun count env graph -> ghost_ (
  project_graph_def count graph;
  Mode_solver_graph_qe_proof.project_many_exact count env graph;
  ())

let (project_graph_scoped @ total) :
    (count : unit list) -> (depth : int) ->
    (graph : {graph : graph | scoped_exists count depth graph}) ->
    {u : unit | scoped_qf depth (project_graph count graph)} @ ghost =
 fun count depth graph -> ghost_ (
  project_graph_def count graph;
  Mode_solver_graph_qe_proof.project_many_scoped count depth graph;
  ())

let (decide_graph_checked @ total) (count : unit list) (graph : graph) :
    {answer : bool option |
      match answer with
      | None -> not (scoped_exists count 0 graph)
      | Some value -> scoped_exists count 0 graph && value = models_exists count [] graph} =
  Mode_solver_graph_qe_proof.decide_graph_checked count graph

let[@def] (subsumption_residual @ total) guard obligation =
  Mode_solver_three_qe_proof.subsumption_residual guard obligation

let (subsumption_residual_exact @ total) :
    (env : elt list) -> (guard : qf) -> (obligation : qf) ->
    {u : unit | eval_qf env (subsumption_residual guard obligation) =
      eval env (subsumption_formula guard obligation)} @ ghost =
 fun env guard obligation -> ghost_ (
  subsumption_residual_def guard obligation;
  Mode_solver_three_qe_proof.subsumption_residual_exact env guard obligation;
  ())

let (subsumption_residual_scoped @ total) :
    (depth : int) -> (guard : {g : qf | scoped_qf (depth + 1) g}) ->
    (obligation : {w : qf | scoped_qf (depth + 2) w}) ->
    {u : unit | scoped_qf depth (subsumption_residual guard obligation)}
    @ ghost =
 fun depth guard obligation -> ghost_ (
  subsumption_residual_def guard obligation;
  Mode_solver_three_qe_proof.subsumption_residual_scoped depth guard obligation;
  ())

let[@def] (assert_subsumption @ total) gamma guard obligation =
  And (gamma, subsumption_residual guard obligation)

let (assert_subsumption_exact @ total) :
    (env : elt list) -> (gamma : qf) -> (guard : qf) -> (obligation : qf) ->
    {u : unit | eval_qf env (assert_subsumption gamma guard obligation) =
      (eval_qf env gamma && eval env (subsumption_formula guard obligation))}
    @ ghost =
 fun env gamma guard obligation -> ghost_ (
  assert_subsumption_def gamma guard obligation;
  eval_qf_def env (And (gamma, subsumption_residual guard obligation));
  subsumption_residual_exact env guard obligation;
  ())

type projected : immutable_data = { domain : qf; winning : qf }

let[@def] (project_guarded @ total) prefix guard witness =
  let inner = Mode_solver_retained_symbolic.project prefix guard witness in
  { domain = inner.domain; winning = inner.winning }

let (project_guarded_exact @ total) :
    (prefix : quantifier list) -> (env : elt list) -> (guard : qf) -> (witness : qf) ->
    {u : unit |
      eval_qf env (project_guarded prefix guard witness).domain = admissible prefix env guard
      && eval_qf env (project_guarded prefix guard witness).winning = game prefix env guard witness}
    @ ghost =
 fun prefix env guard witness -> ghost_ (
  project_guarded_def prefix guard witness;
  Mode_solver_retained_symbolic.project_exact prefix env guard witness;
  ())

let[@def] (project_admissible @ total) prefix guard witness =
  (Mode_solver_retained_symbolic.project_normalized prefix guard witness).winning

let (project_admissible_exact @ total) :
    (prefix : quantifier list) -> (env : elt list) -> (guard : qf) -> (witness : qf) ->
    {u : unit | eval_qf env (project_admissible prefix guard witness) =
      normalized_game prefix env guard witness} @ ghost =
 fun prefix env guard witness -> ghost_ (
  project_admissible_def prefix guard witness;
  Mode_solver_retained_symbolic.project_normalized_exact prefix env guard witness;
  ())

let (project_scopes_compose @ total) :
    (outer : quantifier list) -> (inner : quantifier list) -> (guard : qf) -> (witness : qf) ->
    {u : unit | project_guarded (append_prefix outer inner) guard witness ===
      project_guarded outer (project_guarded inner guard witness).domain
        (project_guarded inner guard witness).winning} @ ghost =
 fun outer inner guard witness -> ghost_ (
  project_guarded_def (append_prefix outer inner) guard witness;
  project_guarded_def inner guard witness;
  project_guarded_def outer (project_guarded inner guard witness).domain
      (project_guarded inner guard witness).winning;
  Mode_solver_retained_symbolic.project_scopes_compose outer inner guard witness;
  ())

let (project_admissible_scoped @ total) :
    (prefix : quantifier list) -> (depth : int) ->
    (guard : {g : qf | scoped_prefix prefix depth g}) ->
    (witness : {w : qf | scoped_prefix prefix depth w}) ->
    {u : unit | scoped_qf depth (project_admissible prefix guard witness)}
    @ ghost =
 fun prefix depth guard witness -> ghost_ (
  project_admissible_def prefix guard witness;
  Mode_solver_retained_symbolic.project_normalized_scoped
    prefix depth guard witness;
  ())

let[@def] (assert_graph_subsumption @ total) gamma guard obligations =
  Mode_solver_graph_qe_proof.assert_graph_subsumption gamma guard obligations

let (assert_graph_subsumption_exact @ total) :
    (env : elt list) -> (gamma : qf) ->
    (guard : graph) -> (obligations : graph) ->
    {u : unit |
      eval_qf env (assert_graph_subsumption gamma guard obligations) =
        (eval_qf env gamma && subsumes env guard obligations)} @ ghost =
 fun env gamma guard obligations -> ghost_ (
  assert_graph_subsumption_def gamma guard obligations;
  subsumes_def env guard obligations;
  models_exists_def [()] (Global :: env) obligations;
  models_exists_def [()] (Regional :: env) obligations;
  models_exists_def [()] (Local :: env) obligations;
  models_exists_def [] (Global :: Global :: env) obligations;
  models_exists_def [] (Regional :: Global :: env) obligations;
  models_exists_def [] (Local :: Global :: env) obligations;
  models_exists_def [] (Global :: Regional :: env) obligations;
  models_exists_def [] (Regional :: Regional :: env) obligations;
  models_exists_def [] (Local :: Regional :: env) obligations;
  models_exists_def [] (Global :: Local :: env) obligations;
  models_exists_def [] (Regional :: Local :: env) obligations;
  models_exists_def [] (Local :: Local :: env) obligations;
  Mode_solver_graph_qe_proof.assert_graph_subsumption_exact env gamma guard obligations;
  ())

let (assert_graph_subsumption_scoped @ total) :
    (depth : int) ->
    (gamma : {gamma : qf | scoped_qf depth gamma}) ->
    (guard : {guard : graph | scoped_graph (depth + 1) guard}) ->
    (obligations : {obligations : graph |
      scoped_graph (depth + 2) obligations}) ->
    {u : unit |
      scoped_qf depth
        (assert_graph_subsumption gamma guard obligations)} @ ghost =
 fun depth gamma guard obligations -> ghost_ (
  assert_graph_subsumption_def gamma guard obligations;
  Mode_solver_graph_qe_proof.assert_graph_subsumption_scoped depth gamma guard obligations;
  ())
