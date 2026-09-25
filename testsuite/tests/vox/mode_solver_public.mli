val regionality_adjunction :
  (a : Mode_solver_semantics.elt) ->
  ((b : Mode_solver_semantics.elt) ->
   {u : unit
     | (Mode_solver_semantics.le (Mode_solver_semantics.regional_to_local a)
          b)
         =
         (Mode_solver_semantics.le a
            (Mode_solver_semantics.regional_to_global b))}) @ total
  stateful @@ total

val eliminate : Mode_solver_semantics.formula -> Mode_solver_semantics.qf @@ total

val eliminate_exact :
  (env : Mode_solver_semantics.elt list) ->
  ((f : Mode_solver_semantics.formula) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env (eliminate f)) =
         (Mode_solver_semantics.eval env f)}) @ total
  stateful @@ total

val eliminate_scoped :
  (depth : int) ->
  ((f : {f : Mode_solver_semantics.formula
          | Mode_solver_semantics.scoped depth f}) ->
   {u : unit | Mode_solver_semantics.scoped_qf depth (eliminate f)}) @ total
  stateful @@ total

val decide_checked :
  (f : Mode_solver_semantics.formula) ->
  {answer : bool option
    | match answer with
      | None -> not (Mode_solver_semantics.scoped 0 f)
      | Some value ->
          (Mode_solver_semantics.scoped 0 f) &&
            (value = (Mode_solver_semantics.eval [] f))} @@ total

val project_graph :
  unit list ->
  Mode_solver_graph_semantics.inequality list -> Mode_solver_semantics.qf @@ total

val project_graph_exact :
  (count : unit list) ->
  ((env : Mode_solver_semantics.elt list) ->
   (graph : Mode_solver_graph_semantics.graph) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env (project_graph count graph)) =
         (Mode_solver_graph_semantics.models_exists count env graph)}) @ total
  stateful @@ total

val project_graph_scoped :
  (count : unit list) ->
  ((depth : int) ->
   (graph : {graph : Mode_solver_graph_semantics.graph
              | Mode_solver_graph_semantics.scoped_exists count depth graph}) ->
   {u : unit
     | Mode_solver_semantics.scoped_qf depth (project_graph count graph)}) @ total
  stateful @@ total

val decide_graph_checked :
  (count : unit list) ->
  (graph : Mode_solver_graph_semantics.graph) ->
  {answer : bool option
    | match answer with
      | None -> not (Mode_solver_graph_semantics.scoped_exists count 0 graph)
      | Some value ->
          (Mode_solver_graph_semantics.scoped_exists count 0 graph) &&
            (value =
               (Mode_solver_graph_semantics.models_exists count [] graph))} @@ total

val subsumption_residual :
  Mode_solver_semantics.qf ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf @@ total

val subsumption_residual_exact :
  (env : Mode_solver_semantics.elt list) ->
  ((guard : Mode_solver_semantics.qf) ->
   (obligation : Mode_solver_semantics.qf) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env
          (subsumption_residual guard obligation))
         =
         (Mode_solver_semantics.eval env
            (Mode_solver_semantics.subsumption_formula guard obligation))}) @ total
  stateful @@ total

val subsumption_residual_scoped :
  (depth : int) ->
  ((guard : {g : Mode_solver_semantics.qf
              | Mode_solver_semantics.scoped_qf (depth + 1) g}) ->
   (obligation : {w : Mode_solver_semantics.qf
                   | Mode_solver_semantics.scoped_qf (depth + 2) w}) ->
   {u : unit
     | Mode_solver_semantics.scoped_qf depth
         (subsumption_residual guard obligation)}) @ total
  stateful @@ total

val assert_subsumption :
  Mode_solver_semantics.qf ->
  Mode_solver_semantics.qf ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf @@ total

val assert_subsumption_exact :
  (env : Mode_solver_semantics.elt list) ->
  ((gamma : Mode_solver_semantics.qf) ->
   (guard : Mode_solver_semantics.qf) ->
   (obligation : Mode_solver_semantics.qf) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env
          (assert_subsumption gamma guard obligation))
         =
         ((Mode_solver_semantics.eval_qf env gamma) &&
            (Mode_solver_semantics.eval env
               (Mode_solver_semantics.subsumption_formula guard obligation)))}) @ total
  stateful @@ total

type projected : immutable_data = {
  domain : Mode_solver_semantics.qf;
  winning : Mode_solver_semantics.qf;
}
val project_guarded :
  Mode_solver_guarded_semantics.quantifier list ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf -> projected @@ total

val project_guarded_exact :
  (prefix : Mode_solver_guarded_semantics.quantifier list) ->
  ((env : Mode_solver_semantics.elt list) ->
   (guard : Mode_solver_semantics.qf) ->
   (witness : Mode_solver_semantics.qf) ->
   {u : unit
     | ((Mode_solver_semantics.eval_qf env
           (project_guarded prefix guard witness).domain)
          = (Mode_solver_guarded_semantics.admissible prefix env guard))
         &&
         ((Mode_solver_semantics.eval_qf env
             (project_guarded prefix guard witness).winning)
            = (Mode_solver_guarded_semantics.game prefix env guard witness))}) @ total
  stateful @@ total

val project_admissible :
  Mode_solver_guarded_semantics.quantifier list ->
  Mode_solver_semantics.qf ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf @@ total

val project_admissible_exact :
  (prefix : Mode_solver_guarded_semantics.quantifier list) ->
  ((env : Mode_solver_semantics.elt list) ->
   (guard : Mode_solver_semantics.qf) ->
   (witness : Mode_solver_semantics.qf) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env
          (project_admissible prefix guard witness))
         =
         (Mode_solver_guarded_semantics.normalized_game prefix env guard
            witness)}) @ total
  stateful @@ total

val project_scopes_compose :
  (outer : Mode_solver_guarded_semantics.quantifier list) ->
  ((inner : Mode_solver_guarded_semantics.quantifier list) ->
   (guard : Mode_solver_semantics.qf) ->
   (witness : Mode_solver_semantics.qf) ->
   {u : unit
     | (project_guarded
          (Mode_solver_guarded_semantics.append_prefix outer inner) guard
          witness)
         ===
         (project_guarded outer (project_guarded inner guard witness).domain
            (project_guarded inner guard witness).winning)}) @ total
  stateful @@ total

val project_admissible_scoped :
  (prefix : Mode_solver_guarded_semantics.quantifier list) ->
  ((depth : int) ->
   (guard : {g : Mode_solver_semantics.qf
              | Mode_solver_guarded_semantics.scoped_prefix prefix depth g}) ->
   (witness : {w : Mode_solver_semantics.qf
                | Mode_solver_guarded_semantics.scoped_prefix prefix depth w}) ->
   {u : unit
     | Mode_solver_semantics.scoped_qf depth
         (project_admissible prefix guard witness)}) @ total
  stateful @@ total

val assert_graph_subsumption :
  Mode_solver_semantics.qf ->
  Mode_solver_graph_semantics.inequality list ->
  Mode_solver_graph_semantics.inequality list -> Mode_solver_semantics.qf @@ total

val assert_graph_subsumption_exact :
  (env : Mode_solver_semantics.elt list) ->
  ((gamma : Mode_solver_semantics.qf) ->
   (guard : Mode_solver_graph_semantics.graph) ->
   (obligations : Mode_solver_graph_semantics.graph) ->
   {u : unit
     | (Mode_solver_semantics.eval_qf env
          (assert_graph_subsumption gamma guard obligations))
         =
         ((Mode_solver_semantics.eval_qf env gamma) &&
            (((not
                 (Mode_solver_graph_semantics.models
                    (Mode_solver_semantics.Global :: env) guard))
                ||
                ((Mode_solver_graph_semantics.models
                    (Mode_solver_semantics.Global ::
                    Mode_solver_semantics.Global :: env) obligations)
                   ||
                   ((Mode_solver_graph_semantics.models
                       (Mode_solver_semantics.Regional ::
                       Mode_solver_semantics.Global :: env) obligations)
                      ||
                      (Mode_solver_graph_semantics.models
                         (Mode_solver_semantics.Local ::
                         Mode_solver_semantics.Global :: env) obligations))))
               &&
               (((not
                    (Mode_solver_graph_semantics.models
                       (Mode_solver_semantics.Regional :: env) guard))
                   ||
                   ((Mode_solver_graph_semantics.models
                       (Mode_solver_semantics.Global ::
                       Mode_solver_semantics.Regional :: env) obligations)
                      ||
                      ((Mode_solver_graph_semantics.models
                          (Mode_solver_semantics.Regional ::
                          Mode_solver_semantics.Regional :: env) obligations)
                         ||
                         (Mode_solver_graph_semantics.models
                            (Mode_solver_semantics.Local ::
                            Mode_solver_semantics.Regional :: env)
                            obligations))))
                  &&
                  ((not
                      (Mode_solver_graph_semantics.models
                         (Mode_solver_semantics.Local :: env) guard))
                     ||
                     ((Mode_solver_graph_semantics.models
                         (Mode_solver_semantics.Global ::
                         Mode_solver_semantics.Local :: env) obligations)
                        ||
                        ((Mode_solver_graph_semantics.models
                            (Mode_solver_semantics.Regional ::
                            Mode_solver_semantics.Local :: env) obligations)
                           ||
                           (Mode_solver_graph_semantics.models
                              (Mode_solver_semantics.Local ::
                              Mode_solver_semantics.Local :: env) obligations)))))))}) @ total
  stateful @@ total

val assert_graph_subsumption_scoped :
  (depth : int) ->
  ((gamma : {gamma : Mode_solver_semantics.qf
              | Mode_solver_semantics.scoped_qf depth gamma}) ->
   (guard : {guard : Mode_solver_graph_semantics.graph
              | Mode_solver_graph_semantics.scoped_graph (depth + 1) guard}) ->
   (obligations : {obligations : Mode_solver_graph_semantics.graph
                    | Mode_solver_graph_semantics.scoped_graph (depth + 2)
                        obligations}) ->
   {u : unit
     | Mode_solver_semantics.scoped_qf depth
         (assert_graph_subsumption gamma guard obligations)}) @ total
  stateful @@ total
