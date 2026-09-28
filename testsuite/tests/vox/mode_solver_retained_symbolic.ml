(* TEST
 flags = "-extension refinement_types";
 has-z3;
 prebuilt_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_guarded_semantics.mli mode_solver_guarded_semantics.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

open Mode_solver_guarded_semantics

type relations : immutable_data = { domain : qf; winning : qf }

let[@def] rec (project @ total) prefix guard witness =
  match prefix with
  | [] -> { domain = guard; winning = witness }
  | quantifier :: rest ->
    let inner = project rest guard witness in
    let winning = match quantifier with
      | Existential -> Exists (Plain (And (inner.domain, inner.winning)))
      | Universal -> Forall (Plain (Or (Not inner.domain, inner.winning)))
    in
    { domain = eliminate (Exists (Plain inner.domain));
      winning = eliminate winning }

let (exists_step @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      eval_qf env (eliminate (Exists (Plain body))) =
      (eval_qf (Global :: env) body || eval_qf (Regional :: env) body
       || eval_qf (Local :: env) body)} @ ghost =
 fun env body -> ghost_ (
  eliminate_exact env (Exists (Plain body));
  eval_def env (Exists (Plain body));
  eval_def (Global :: env) (Plain body);
  eval_def (Regional :: env) (Plain body);
  eval_def (Local :: env) (Plain body);
  ())

let (forall_step @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      eval_qf env (eliminate (Forall (Plain body))) =
      (eval_qf (Global :: env) body && eval_qf (Regional :: env) body
       && eval_qf (Local :: env) body)} @ ghost =
 fun env body -> ghost_ (
  eliminate_exact env (Forall (Plain body));
  eval_def env (Forall (Plain body));
  eval_def (Global :: env) (Plain body);
  eval_def (Regional :: env) (Plain body);
  eval_def (Local :: env) (Plain body);
  ())

let (and_value @ total) :
    (env : elt list) -> (left : qf) -> (right : qf) ->
    {u : unit | eval_qf env (And (left, right)) =
      (eval_qf env left && eval_qf env right)} @ ghost =
 fun env left right -> ghost_ (
  eval_qf_def env (And (left, right));
  ())

let (implication_value @ total) :
    (env : elt list) -> (left : qf) -> (right : qf) ->
    {u : unit | eval_qf env (Or (Not left, right)) =
      (not (eval_qf env left) || eval_qf env right)} @ ghost =
 fun env left right -> ghost_ (
  eval_qf_def env (Or (Not left, right));
  eval_qf_def env (Not left);
  ())

let rec (project_exact @ total) :
    (prefix : quantifier list) -> (env : elt list) ->
    (guard : qf) -> (witness : qf) ->
    {u : unit |
      eval_qf env (project prefix guard witness).domain
        = admissible prefix env guard
      && eval_qf env (project prefix guard witness).winning
        = game prefix env guard witness} @ ghost =
 fun prefix env guard witness -> ghost_ (
  project_def prefix guard witness;
  admissible_def prefix env guard;
  game_def prefix env guard witness;
  match prefix with
  | [] -> ()
  | quantifier :: rest ->
    project_exact rest (Global :: env) guard witness;
    project_exact rest (Regional :: env) guard witness;
    project_exact rest (Local :: env) guard witness;
    let inner = project rest guard witness in
    exists_step env inner.domain;
    (match quantifier with
    | Existential ->
      exists_step env (And (inner.domain, inner.winning));
      and_value (Global :: env) inner.domain inner.winning;
      and_value (Regional :: env) inner.domain inner.winning;
      and_value (Local :: env) inner.domain inner.winning
    | Universal ->
      forall_step env (Or (Not inner.domain, inner.winning));
      implication_value (Global :: env) inner.domain inner.winning;
      implication_value (Regional :: env) inner.domain inner.winning;
      implication_value (Local :: env) inner.domain inner.winning);
    ())

let[@def] (retain_assertion @ total) state clause =
  { domain = state.domain; winning = And (state.winning, clause) }

let (successive_assertions_exact @ total) :
    (env : elt list) -> (state : relations) -> (first : qf) -> (second : qf) ->
    {u : unit |
      eval_qf env (retain_assertion (retain_assertion state first) second).winning
      = (eval_qf env state.winning && eval_qf env first && eval_qf env second)}
    @ ghost =
 fun env state first second -> ghost_ (
  retain_assertion_def state first;
  retain_assertion_def (retain_assertion state first) second;
  and_value env (And (state.winning, first)) second;
  and_value env state.winning first;
  ())

let rec (project_scopes_compose @ total) :
    (outer : quantifier list) -> (inner : quantifier list) ->
    (guard : qf) -> (witness : qf) ->
    {u : unit |
      project (append_prefix outer inner) guard witness ===
      project outer (project inner guard witness).domain
        (project inner guard witness).winning} @ ghost =
 fun outer inner guard witness -> ghost_ (
  append_prefix_def outer inner;
  project_def (append_prefix outer inner) guard witness;
  project_def outer (project inner guard witness).domain
      (project inner guard witness).winning;
  match outer with
  | [] -> ()
  | _ :: rest ->
    project_scopes_compose rest inner guard witness;
    ())

let[@def] rec (project_normalized @ total) prefix guard witness =
  match prefix with
  | [] -> { domain = guard; winning = And (guard, witness) }
  | quantifier :: rest ->
    let inner = project_normalized rest guard witness in
    let domain = eliminate (Exists (Plain inner.domain)) in
    let winning = match quantifier with
      | Existential -> Exists (Plain (And (inner.domain, inner.winning)))
      | Universal -> Forall (Plain (Or (Not inner.domain, inner.winning)))
    in
    { domain; winning = And (domain, eliminate winning) }

let rec (project_normalized_exact @ total) :
    (prefix : quantifier list) -> (env : elt list) ->
    (guard : qf) -> (witness : qf) ->
    {u : unit |
      eval_qf env (project_normalized prefix guard witness).domain
        = admissible prefix env guard
      && eval_qf env (project_normalized prefix guard witness).winning
        = normalized_game prefix env guard witness} @ ghost =
 fun prefix env guard witness -> ghost_ (
  project_normalized_def prefix guard witness;
  admissible_def prefix env guard;
  game_def prefix env guard witness;
  normalized_game_def prefix env guard witness;
  match prefix with
  | [] ->
    and_value env guard witness;
    ()
  | quantifier :: rest ->
    project_normalized_exact rest (Global :: env) guard witness;
    project_normalized_exact rest (Regional :: env) guard witness;
    project_normalized_exact rest (Local :: env) guard witness;
    normalized_game_def rest (Global :: env) guard witness;
    normalized_game_def rest (Regional :: env) guard witness;
    normalized_game_def rest (Local :: env) guard witness;
    let inner = project_normalized rest guard witness in
    let domain = eliminate (Exists (Plain inner.domain)) in
    exists_step env inner.domain;
    (match quantifier with
    | Existential ->
      and_value env domain
        (eliminate (Exists (Plain (And (inner.domain, inner.winning)))));
      exists_step env (And (inner.domain, inner.winning));
      and_value (Global :: env) inner.domain inner.winning;
      and_value (Regional :: env) inner.domain inner.winning;
      and_value (Local :: env) inner.domain inner.winning
    | Universal ->
      and_value env domain
        (eliminate (Forall (Plain (Or (Not inner.domain, inner.winning)))));
      forall_step env (Or (Not inner.domain, inner.winning));
      implication_value (Global :: env) inner.domain inner.winning;
      implication_value (Regional :: env) inner.domain inner.winning;
      implication_value (Local :: env) inner.domain inner.winning);
    ())

let rec (project_normalized_scoped @ total) :
    (prefix : quantifier list) -> (depth : int) ->
    (guard : {g : qf | scoped_prefix prefix depth g}) ->
    (witness : {w : qf | scoped_prefix prefix depth w}) ->
    {u : unit |
      scoped_qf depth (project_normalized prefix guard witness).domain
      && scoped_qf depth (project_normalized prefix guard witness).winning}
    @ ghost =
 fun prefix depth guard witness -> ghost_ (
  scoped_prefix_def prefix depth guard;
  scoped_prefix_def prefix depth witness;
  project_normalized_def prefix guard witness;
  match prefix with
  | [] ->
    scoped_qf_def depth (And (guard, witness));
    ()
  | quantifier :: rest ->
    project_normalized_scoped rest (depth + 1) guard witness;
    let inner = project_normalized rest guard witness in
    scoped_def depth (Exists (Plain inner.domain));
    scoped_def (depth + 1) (Plain inner.domain);
    eliminate_scoped depth (Exists (Plain inner.domain));
    let domain = eliminate (Exists (Plain inner.domain)) in
    (match quantifier with
    | Existential ->
      let body = And (inner.domain, inner.winning) in
      scoped_qf_def (depth + 1) body;
      scoped_def (depth + 1) (Plain body);
      scoped_def depth (Exists (Plain body));
      eliminate_scoped depth (Exists (Plain body));
      scoped_qf_def depth
        (And (domain, eliminate (Exists (Plain body))))
    | Universal ->
      let body = Or (Not inner.domain, inner.winning) in
      scoped_qf_def (depth + 1) (Not inner.domain);
      scoped_qf_def (depth + 1) body;
      scoped_def (depth + 1) (Plain body);
      scoped_def depth (Forall (Plain body));
      eliminate_scoped depth (Forall (Plain body));
      scoped_qf_def depth
        (And (domain, eliminate (Forall (Plain body)))));
    ())
