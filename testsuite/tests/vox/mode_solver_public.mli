(** Quantifier elimination over the chain [Global < Regional < Local].

    Formulas, their evaluation and scoping are defined in
    [Mode_solver_semantics]; inequality graphs in
    [Mode_solver_graph_semantics]; guarded quantifier prefixes in
    [Mode_solver_guarded_semantics]. Environments are lists of values
    indexed by de Bruijn indices. Each operation comes with an [_exact]
    theorem giving its result's truth value in every environment and, for
    most, a [_scoped] theorem bounding the variables its result mentions
    ([scoped depth f]: every free variable of [f] is below [depth]).

    Every function is [total], so the specifications can apply it. The
    theorems return a refined [unit] [@ ghost]: their proofs are erased. *)

open Mode_solver_semantics
open Mode_solver_graph_semantics
open Mode_solver_guarded_semantics

(** {2 The chain} *)

(** [regional_to_local] is left adjoint to [regional_to_global]. *)
val regionality_adjunction : (a : elt) -> (b : elt) ->
  {u : unit | le (regional_to_local a) b = le a (regional_to_global b)}
  @ ghost @@ total

(** {2 Formulas} *)

(** A quantifier-free formula equivalent to [f]. *)
val eliminate : formula -> qf @@ total
val eliminate_exact : (env : elt list) -> (f : formula) ->
  {u : unit | eval_qf env (eliminate f) = eval env f} @ ghost @@ total
val eliminate_scoped : (depth : int) -> (f : {f : formula | scoped depth f}) ->
  {u : unit | scoped_qf depth (eliminate f)} @ ghost @@ total

(** The truth value of a closed formula; [None] if [f] has a free
    variable. *)
val decide_checked : (f : formula) ->
  {answer : bool option |
    match answer with
    | None -> not (scoped 0 f)
    | Some value -> scoped 0 f && value = eval [] f} @@ total

(** {2 Inequality graphs}

    [count] is a natural number written as a [unit list]. *)

(** [project_graph count graph] eliminates the [count] innermost variables
    of [graph], read existentially. *)
val project_graph : unit list -> graph -> qf @@ total
val project_graph_exact :
  (count : unit list) -> (env : elt list) -> (graph : graph) ->
  {u : unit | eval_qf env (project_graph count graph)
              = models_exists count env graph} @ ghost @@ total
val project_graph_scoped :
  (count : unit list) -> (depth : int) ->
  (graph : {graph : graph | scoped_exists count depth graph}) ->
  {u : unit | scoped_qf depth (project_graph count graph)} @ ghost @@ total

(** Whether [graph] holds for some values of its [count] variables; [None]
    if it has other free variables. *)
val decide_graph_checked : (count : unit list) -> (graph : graph) ->
  {answer : bool option |
    match answer with
    | None -> not (scoped_exists count 0 graph)
    | Some value ->
      scoped_exists count 0 graph && value = models_exists count [] graph}
  @@ total

(** {2 Subsumption}

    [subsumption_formula guard obligation] is
    [forall x. not guard || exists y. obligation]. *)

(** A quantifier-free formula equivalent to
    [subsumption_formula guard obligation]. *)
val subsumption_residual : qf -> qf -> qf @@ total
val subsumption_residual_exact :
  (env : elt list) -> (guard : qf) -> (obligation : qf) ->
  {u : unit | eval_qf env (subsumption_residual guard obligation)
              = eval env (subsumption_formula guard obligation)}
  @ ghost @@ total
val subsumption_residual_scoped :
  (depth : int) -> (guard : {g : qf | scoped_qf (depth + 1) g}) ->
  (obligation : {w : qf | scoped_qf (depth + 2) w}) ->
  {u : unit | scoped_qf depth (subsumption_residual guard obligation)}
  @ ghost @@ total

(** Adds the residual of a subsumption check to a context [gamma]. *)
val assert_subsumption : qf -> qf -> qf -> qf @@ total
val assert_subsumption_exact :
  (env : elt list) -> (gamma : qf) -> (guard : qf) -> (obligation : qf) ->
  {u : unit | eval_qf env (assert_subsumption gamma guard obligation)
              = (eval_qf env gamma
                 && eval env (subsumption_formula guard obligation))}
  @ ghost @@ total

(** The same for inequality graphs. The theorem spells out the subsumption
    over the three values of [x]: [guard] mentions [x] as variable [0], and
    [obligations] mentions [y] and [x] as variables [0] and [1]. *)
val assert_graph_subsumption :
  qf -> graph -> graph -> qf @@ total
val assert_graph_subsumption_exact :
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
               || models (Local :: Local :: env) obligations)))}
  @ ghost @@ total
val assert_graph_subsumption_scoped :
  (depth : int) ->
  (gamma : {gamma : qf | scoped_qf depth gamma}) ->
  (guard : {guard : graph | scoped_graph (depth + 1) guard}) ->
  (obligations : {obligations : graph |
    scoped_graph (depth + 2) obligations}) ->
  {u : unit | scoped_qf depth
                (assert_graph_subsumption gamma guard obligations)}
  @ ghost @@ total

(** {2 Guarded quantifier prefixes}

    A prefix of quantifiers whose variables must satisfy [guard], read as a
    game. [admissible] holds if some values of the prefix satisfy [guard];
    [game] holds if the existential player wins, where each move must leave
    [guard] satisfiable and the final values must satisfy [witness]. *)

type projected : immutable_data = {
  domain : qf;  (** Equivalent to [admissible]. *)
  winning : qf;  (** Equivalent to [game]. *)
}

val project_guarded : quantifier list -> qf -> qf -> projected @@ total
val project_guarded_exact :
  (prefix : quantifier list) -> (env : elt list) ->
  (guard : qf) -> (witness : qf) ->
  {u : unit |
    eval_qf env (project_guarded prefix guard witness).domain
      = admissible prefix env guard
    && eval_qf env (project_guarded prefix guard witness).winning
      = game prefix env guard witness} @ ghost @@ total

(** Projecting a prefix in two parts gives the same formulas, as syntax, as
    projecting it at once. *)
val project_scopes_compose :
  (outer : quantifier list) -> (inner : quantifier list) ->
  (guard : qf) -> (witness : qf) ->
  {u : unit |
    project_guarded (append_prefix outer inner) guard witness ===
      project_guarded outer (project_guarded inner guard witness).domain
        (project_guarded inner guard witness).winning} @ ghost @@ total

(** A formula equivalent to [admissible && game]. *)
val project_admissible : quantifier list -> qf -> qf -> qf @@ total
val project_admissible_exact :
  (prefix : quantifier list) -> (env : elt list) ->
  (guard : qf) -> (witness : qf) ->
  {u : unit | eval_qf env (project_admissible prefix guard witness)
              = normalized_game prefix env guard witness} @ ghost @@ total
val project_admissible_scoped :
  (prefix : quantifier list) -> (depth : int) ->
  (guard : {g : qf | scoped_prefix prefix depth g}) ->
  (witness : {w : qf | scoped_prefix prefix depth w}) ->
  {u : unit | scoped_qf depth (project_admissible prefix guard witness)}
  @ ghost @@ total
