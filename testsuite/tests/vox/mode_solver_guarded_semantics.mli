type quantifier : immutable_data = Universal | Existential
val admissible :
  quantifier list ->
  Mode_solver_semantics.elt list -> Mode_solver_semantics.qf -> bool @@ total

val admissible_def :
  (prefix : quantifier list) ->
  (env : Mode_solver_semantics.elt list) ->
  (guard : Mode_solver_semantics.qf) ->
  {u : unit
    | (admissible prefix env guard) ===
        (match prefix with
         | [] -> Mode_solver_semantics.eval_qf env guard
         | _::rest ->
             (admissible rest (Mode_solver_semantics.Global :: env) guard) ||
               ((admissible rest (Mode_solver_semantics.Regional :: env)
                   guard)
                  ||
                  (admissible rest (Mode_solver_semantics.Local :: env) guard)))} @@ total

val game :
  quantifier list ->
  Mode_solver_semantics.elt list ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf -> bool @@ total

val game_def :
  (prefix : quantifier list) ->
  (env : Mode_solver_semantics.elt list) ->
  (guard : Mode_solver_semantics.qf) ->
  (witness : Mode_solver_semantics.qf) ->
  {u : unit
    | (game prefix env guard witness) ===
        (match prefix with
         | [] -> Mode_solver_semantics.eval_qf env witness
         | (Existential)::rest' ->
             ((admissible rest' (Mode_solver_semantics.Global :: env) guard)
                &&
                (game rest' (Mode_solver_semantics.Global :: env) guard
                   witness))
               ||
               (((admissible rest' (Mode_solver_semantics.Regional :: env)
                    guard)
                   &&
                   (game rest' (Mode_solver_semantics.Regional :: env) guard
                      witness))
                  ||
                  ((admissible rest' (Mode_solver_semantics.Local :: env)
                      guard)
                     &&
                     (game rest' (Mode_solver_semantics.Local :: env) guard
                        witness)))
         | (Universal)::rest ->
             ((not
                 (admissible rest (Mode_solver_semantics.Global :: env) guard))
                ||
                (game rest (Mode_solver_semantics.Global :: env) guard
                   witness))
               &&
               (((not
                    (admissible rest (Mode_solver_semantics.Regional :: env)
                       guard))
                   ||
                   (game rest (Mode_solver_semantics.Regional :: env) guard
                      witness))
                  &&
                  ((not
                      (admissible rest (Mode_solver_semantics.Local :: env)
                         guard))
                     ||
                     (game rest (Mode_solver_semantics.Local :: env) guard
                        witness))))} @@ total

val append_prefix : quantifier list -> quantifier list -> quantifier list @@ total

val append_prefix_def :
  (outer : quantifier list) ->
  (inner : quantifier list) ->
  {u : unit
    | (append_prefix outer inner) ===
        (match outer with
         | [] -> inner
         | head::rest -> head :: (append_prefix rest inner))} @@ total

val normalized_game :
  quantifier list ->
  Mode_solver_semantics.elt list ->
  Mode_solver_semantics.qf -> Mode_solver_semantics.qf -> bool @@ total

val normalized_game_def :
  (prefix : quantifier list) ->
  (env : Mode_solver_semantics.elt list) ->
  (guard : Mode_solver_semantics.qf) ->
  (witness : Mode_solver_semantics.qf) ->
  {u : unit
    | (normalized_game prefix env guard witness) ===
        ((admissible prefix env guard) && (game prefix env guard witness))} @@ total

val scoped_prefix :
  quantifier list -> int -> Mode_solver_semantics.qf -> bool @@ total

val scoped_prefix_def :
  (prefix : quantifier list) ->
  (depth : int) ->
  (q : Mode_solver_semantics.qf) ->
  {u : unit
    | (scoped_prefix prefix depth q) ===
        (match prefix with
         | [] -> Mode_solver_semantics.scoped_qf depth q
         | _::rest -> scoped_prefix rest (depth + 1) q)} @@ total
