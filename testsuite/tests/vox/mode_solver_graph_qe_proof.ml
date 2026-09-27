open Mode_solver_semantics
open Mode_solver_three_qe_proof

open Mode_solver_graph_semantics

let[@def] rec (compile @ total) graph =
  match graph with
  | [] -> Le (Const Global, Const Global)
  | edge :: rest ->
    And (Le (edge.left, edge.right), compile rest)

let rec (compile_scoped @ total) :
    (depth : int) ->
    (graph : {graph : graph | scoped_graph depth graph}) ->
    {u : unit | scoped_qf depth (compile graph)} @ ghost =
 fun depth graph -> ghost_ (
  scoped_graph_def depth graph;
  compile_def graph;
  scoped_qf_def depth (compile graph);
  match graph with
  | [] ->
    scoped_qf_def depth (Le (Const Global, Const Global));
    scoped_term_def depth (Const Global);
    ()
  | edge :: rest ->
    compile_scoped depth rest;
    scoped_qf_def depth (Le (edge.left, edge.right));
    ())

let rec (compile_exact @ total) :
    (env : elt list) -> (graph : graph) ->
    {u : unit | eval_qf env (compile graph) = models env graph} @ ghost =
 fun env graph -> ghost_ (
  compile_def graph;
  models_def env graph;
  eval_qf_def env (compile graph);
  match graph with
  | [] ->
    eval_qf_def env (Le (Const Global, Const Global));
    eval_term_def env (Const Global);
    le_def Global Global;
    rank_def Global;
    ()
  | edge :: rest ->
    compile_exact env rest;
    eval_qf_def env (Le (edge.left, edge.right));
    eval_qf_def env (compile rest);
    ())

let[@def] (project @ total) graph =
  eliminate (Exists (Plain (compile graph)))

let (project_exact @ total) :
    (env : elt list) -> (graph : graph) ->
    {u : unit |
      eval_qf env (project graph) =
        (models (Global :: env) graph
         || models (Regional :: env) graph
         || models (Local :: env) graph)} @ ghost =
 fun env graph -> ghost_ (
  project_def graph;
  eliminate_exact env (Exists (Plain (compile graph)));
  eval_def env (Exists (Plain (compile graph)));
  eval_def (Global :: env) (Plain (compile graph));
  eval_def (Regional :: env) (Plain (compile graph));
  eval_def (Local :: env) (Plain (compile graph));
  compile_exact (Global :: env) graph;
  compile_exact (Regional :: env) graph;
  compile_exact (Local :: env) graph;
  ())

let[@def] rec (exists_formula @ total) count body =
  match count with
  | [] -> Plain body
  | _ :: rest -> Exists (exists_formula rest body)

let rec (exists_formula_scoped @ total) :
    (count : unit list) -> (depth : int) ->
    (graph : {graph : graph | scoped_exists count depth graph}) ->
    {u : unit |
      scoped depth (exists_formula count (compile graph))} @ ghost =
 fun count depth graph -> ghost_ (
  scoped_exists_def count depth graph;
  exists_formula_def count (compile graph);
  scoped_def depth (exists_formula count (compile graph));
  match count with
  | [] ->
    compile_scoped depth graph;
    ()
  | _ :: rest ->
    exists_formula_scoped rest (depth + 1) graph;
    ())

let rec (exists_formula_exact @ total) :
    (count : unit list) -> (env : elt list) -> (graph : graph) ->
    {u : unit |
      eval env (exists_formula count (compile graph)) =
        models_exists count env graph} @ ghost =
 fun count env graph -> ghost_ (
  exists_formula_def count (compile graph);
  models_exists_def count env graph;
  eval_def env (exists_formula count (compile graph));
  match count with
  | [] ->
    compile_exact env graph;
    ()
  | _ :: rest ->
    exists_formula_exact rest (Global :: env) graph;
    exists_formula_exact rest (Regional :: env) graph;
    exists_formula_exact rest (Local :: env) graph;
    ())

let[@def] (project_many @ total) count graph =
  eliminate (exists_formula count (compile graph))

let (project_many_scoped @ total) :
    (count : unit list) -> (depth : int) ->
    (graph : {graph : graph | scoped_exists count depth graph}) ->
    {u : unit | scoped_qf depth (project_many count graph)} @ ghost =
 fun count depth graph -> ghost_ (
  project_many_def count graph;
  exists_formula_scoped count depth graph;
  eliminate_scoped depth (exists_formula count (compile graph));
  ())

let (project_many_exact @ total) :
    (count : unit list) -> (env : elt list) -> (graph : graph) ->
    {u : unit |
      eval_qf env (project_many count graph) =
        models_exists count env graph} @ ghost =
 fun count env graph -> ghost_ (
  project_many_def count graph;
  eliminate_exact env (exists_formula count (compile graph));
  exists_formula_exact count env graph;
  ())

let[@def] (decide_graph @ total) count graph =
  eval_qf [] (project_many count graph)

let (decide_graph_exact @ total) :
    (count : unit list) ->
    (graph : {graph : graph | scoped_exists count 0 graph}) ->
    {u : unit |
      decide_graph count graph = models_exists count [] graph
      && scoped_qf 0 (project_many count graph)} @ ghost =
 fun count graph -> ghost_ (
  decide_graph_def count graph;
  project_many_exact count [] graph;
  project_many_scoped count 0 graph;
  ())

let (decide_graph_checked @ total) (count : unit list) (graph : graph) :
    {answer : bool option |
      match answer with
      | None -> not (scoped_exists count 0 graph)
      | Some value ->
        scoped_exists count 0 graph
        && value = models_exists count [] graph} =
  if scoped_exists count 0 graph then begin
    ghost_ (decide_graph_exact count graph);
    Some (decide_graph count graph)
  end else None

let[@def] (assert_graph_subsumption @ total) gamma guard obligations =
  assert_subsumption gamma (compile guard) (compile obligations)

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
  compile_scoped (depth + 1) guard;
  compile_scoped (depth + 2) obligations;
  subsumption_residual_scoped depth (compile guard) (compile obligations);
  assert_graph_subsumption_def gamma guard obligations;
  assert_subsumption_def gamma (compile guard) (compile obligations);
  scoped_qf_def depth (assert_graph_subsumption gamma guard obligations);
  ())

let (guarded_at_exact @ total) :
    (env : elt list) -> (value : elt) ->
    (guard : graph) -> (obligations : graph) ->
    {u : unit |
      eval (value :: env)
        (Disj
           (Neg (Plain (compile guard)),
            Exists (Plain (compile obligations)))) =
        (not (models (value :: env) guard)
         || models (Global :: value :: env) obligations
         || models (Regional :: value :: env) obligations
         || models (Local :: value :: env) obligations)} @ ghost =
 fun env value guard obligations -> ghost_ (
  (eval_def (value :: env)
     (Disj
        (Neg (Plain (compile guard)),
         Exists (Plain (compile obligations)))));
  eval_def (value :: env) (Neg (Plain (compile guard)));
  eval_def (value :: env) (Plain (compile guard));
  eval_def (value :: env) (Exists (Plain (compile obligations)));
  eval_def (Global :: value :: env) (Plain (compile obligations));
  eval_def (Regional :: value :: env) (Plain (compile obligations));
  eval_def (Local :: value :: env) (Plain (compile obligations));
  compile_exact (value :: env) guard;
  compile_exact (Global :: value :: env) obligations;
  compile_exact (Regional :: value :: env) obligations;
  compile_exact (Local :: value :: env) obligations;
  ())

let (assert_graph_subsumption_exact @ total) :
    (env : elt list) -> (gamma : qf) ->
    (guard : graph) -> (obligations : graph) ->
    {u : unit |
      eval_qf env (assert_graph_subsumption gamma guard obligations) =
        (eval_qf env gamma
         && ((not (models (Global :: env) guard)
              || models (Global :: Global :: env) obligations
              || models (Regional :: Global :: env) obligations
              || models (Local :: Global :: env) obligations)
             && (not (models (Regional :: env) guard)
                 || models (Global :: Regional :: env) obligations
                 || models (Regional :: Regional :: env) obligations
                 || models (Local :: Regional :: env) obligations)
             && (not (models (Local :: env) guard)
                 || models (Global :: Local :: env) obligations
                 || models (Regional :: Local :: env) obligations
                 || models (Local :: Local :: env) obligations)))} @ ghost =
 fun env gamma guard obligations -> ghost_ (
  assert_graph_subsumption_def gamma guard obligations;
  (assert_subsumption_exact env gamma
     (compile guard) (compile obligations));
  (subsumption_formula_def
     (compile guard) (compile obligations));
  (eval_def env
     (subsumption_formula (compile guard) (compile obligations)));
  guarded_at_exact env Global guard obligations;
  guarded_at_exact env Regional guard obligations;
  guarded_at_exact env Local guard obligations;
  ())
