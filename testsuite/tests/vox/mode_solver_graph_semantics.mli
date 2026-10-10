open Mode_solver_semantics

type inequality : immutable_data = { left : term; right : term }
type graph : immutable_data = inequality list

val models : elt list -> graph -> bool @@ total
val models_def : (env : elt list) -> (graph : graph) ->
  {u : unit | models env graph ===
    (match graph with
     | [] -> true
     | edge :: rest ->
       le (eval_term env edge.left) (eval_term env edge.right)
       && models env rest)} @@ total

val scoped_graph : int -> graph -> bool @@ total
val scoped_graph_def : (depth : int) -> (graph : graph) ->
  {u : unit | scoped_graph depth graph ===
    (match graph with
     | [] -> true
     | edge :: rest ->
       scoped_term depth edge.left && scoped_term depth edge.right
       && scoped_graph depth rest)} @@ total

val models_exists : unit list -> elt list -> graph -> bool @@ total
val models_exists_def : (count : unit list) -> (env : elt list) ->
  (graph : graph) ->
  {u : unit | models_exists count env graph ===
    (match count with
     | [] -> models env graph
     | _ :: rest ->
       models_exists rest (Global :: env) graph
       || models_exists rest (Regional :: env) graph
       || models_exists rest (Local :: env) graph)} @@ total

val scoped_exists : unit list -> int -> graph -> bool @@ total
val scoped_exists_def : (count : unit list) -> (depth : int) ->
  (graph : graph) ->
  {u : unit | scoped_exists count depth graph ===
    (match count with
     | [] -> scoped_graph depth graph
     | _ :: rest -> scoped_exists rest (depth + 1) graph)} @@ total

(** [forall x. guard x implies exists y. obligations x y], with [x] at
    index [0] in [guard], and [y], [x] at indices [0], [1] in [obligations]. *)
val subsumes : elt list -> graph -> graph -> bool @@ total
val subsumes_def : (env : elt list) -> (guard : graph) ->
  (obligations : graph) ->
  {u : unit | subsumes env guard obligations =
    ((not (models (Global :: env) guard)
      || models_exists [()] (Global :: env) obligations)
     && (not (models (Regional :: env) guard)
         || models_exists [()] (Regional :: env) obligations)
     && (not (models (Local :: env) guard)
         || models_exists [()] (Local :: env) obligations))} @@ total
