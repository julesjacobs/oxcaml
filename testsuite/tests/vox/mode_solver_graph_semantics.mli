type inequality : immutable_data = {
  left : Mode_solver_semantics.term;
  right : Mode_solver_semantics.term;
}
type graph : immutable_data = inequality list
val models : Mode_solver_semantics.elt list -> inequality list -> bool @@ total

val models_def :
  (env : Mode_solver_semantics.elt list) ->
  (graph : inequality list) ->
  {u : unit
    | (models env graph) ===
        (match graph with
         | [] -> true
         | edge::rest ->
             (Mode_solver_semantics.le
                (Mode_solver_semantics.eval_term env edge.left)
                (Mode_solver_semantics.eval_term env edge.right))
               && (models env rest))} @@ total

val scoped_graph : int -> inequality list -> bool @@ total

val scoped_graph_def :
  (depth : int) ->
  (graph : inequality list) ->
  {u : unit
    | (scoped_graph depth graph) ===
        (match graph with
         | [] -> true
         | edge::rest ->
             (Mode_solver_semantics.scoped_term depth edge.left) &&
               ((Mode_solver_semantics.scoped_term depth edge.right) &&
                  (scoped_graph depth rest)))} @@ total

val models_exists :
  unit list -> Mode_solver_semantics.elt list -> inequality list -> bool @@ total

val models_exists_def :
  (count : unit list) ->
  (env : Mode_solver_semantics.elt list) ->
  (graph : inequality list) ->
  {u : unit
    | (models_exists count env graph) ===
        (match count with
         | [] -> models env graph
         | _::rest ->
             (models_exists rest (Mode_solver_semantics.Global :: env) graph)
               ||
               ((models_exists rest (Mode_solver_semantics.Regional :: env)
                   graph)
                  ||
                  (models_exists rest (Mode_solver_semantics.Local :: env)
                     graph)))} @@ total

val scoped_exists : unit list -> int -> inequality list -> bool @@ total

val scoped_exists_def :
  (count : unit list) ->
  (depth : int) ->
  (graph : inequality list) ->
  {u : unit
    | (scoped_exists count depth graph) ===
        (match count with
         | [] -> scoped_graph depth graph
         | _::rest -> scoped_exists rest (depth + 1) graph)} @@ total
