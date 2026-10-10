open Mode_solver_semantics

type quantifier : immutable_data = Universal | Existential

val admissible : quantifier list -> elt list -> qf -> bool @@ total
val admissible_def : (prefix : quantifier list) -> (env : elt list) ->
  (guard : qf) ->
  {u : unit | admissible prefix env guard ===
    (match prefix with
     | [] -> eval_qf env guard
     | _ :: rest ->
       admissible rest (Global :: env) guard
       || admissible rest (Regional :: env) guard
       || admissible rest (Local :: env) guard)} @@ total

val game : quantifier list -> elt list -> qf -> qf -> bool @@ total
val game_def : (prefix : quantifier list) -> (env : elt list) ->
  (guard : qf) -> (witness : qf) ->
  {u : unit | game prefix env guard witness ===
    (match prefix with
     | [] -> eval_qf env witness
     | Existential :: rest ->
       (admissible rest (Global :: env) guard
        && game rest (Global :: env) guard witness)
       || (admissible rest (Regional :: env) guard
           && game rest (Regional :: env) guard witness)
       || (admissible rest (Local :: env) guard
           && game rest (Local :: env) guard witness)
     | Universal :: rest ->
       (not (admissible rest (Global :: env) guard)
        || game rest (Global :: env) guard witness)
       && (not (admissible rest (Regional :: env) guard)
           || game rest (Regional :: env) guard witness)
       && (not (admissible rest (Local :: env) guard)
           || game rest (Local :: env) guard witness))} @@ total

val append_prefix : quantifier list -> quantifier list -> quantifier list
  @@ total
val append_prefix_def : (outer : quantifier list) ->
  (inner : quantifier list) ->
  {u : unit | append_prefix outer inner ===
    (match outer with
     | [] -> inner
     | head :: rest -> head :: append_prefix rest inner)} @@ total

val normalized_game : quantifier list -> elt list -> qf -> qf -> bool
  @@ total
val normalized_game_def : (prefix : quantifier list) -> (env : elt list) ->
  (guard : qf) -> (witness : qf) ->
  {u : unit | normalized_game prefix env guard witness ===
    (admissible prefix env guard && game prefix env guard witness)} @@ total

val scoped_prefix : quantifier list -> int -> qf -> bool @@ total
val scoped_prefix_def : (prefix : quantifier list) -> (depth : int) ->
  (q : qf) ->
  {u : unit | scoped_prefix prefix depth q ===
    (match prefix with
     | [] -> scoped_qf depth q
     | _ :: rest -> scoped_prefix rest (depth + 1) q)} @@ total
