open Mode_solver_semantics

type inequality : immutable_data = { left : term; right : term }
type graph : immutable_data = inequality list

let[@def] rec (models @ total) env graph =
  match graph with
  | [] -> true
  | edge :: rest ->
    le (eval_term env edge.left) (eval_term env edge.right)
    && models env rest

let[@def] rec (scoped_graph @ total) depth graph =
  match graph with
  | [] -> true
  | edge :: rest ->
    scoped_term depth edge.left && scoped_term depth edge.right
    && scoped_graph depth rest

let[@def] rec (models_exists @ total) (count : unit list) env graph =
  match count with
  | [] -> models env graph
  | _ :: rest ->
    models_exists rest (Global :: env) graph
    || models_exists rest (Regional :: env) graph
    || models_exists rest (Local :: env) graph

let[@def] rec (scoped_exists @ total) (count : unit list) depth graph =
  match count with
  | [] -> scoped_graph depth graph
  | _ :: rest -> scoped_exists rest (depth + 1) graph
