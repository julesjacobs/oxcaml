open Mode_solver_semantics

type quantifier : immutable_data = Universal | Existential

let[@def] rec (admissible @ total) (prefix : quantifier list) env guard =
  match prefix with
  | [] -> eval_qf env guard
  | _ :: rest ->
    admissible rest (Global :: env) guard
    || admissible rest (Regional :: env) guard
    || admissible rest (Local :: env) guard

let[@def] rec (game @ total) prefix env guard witness =
  match prefix with
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
        || game rest (Local :: env) guard witness)

let[@def] rec (append_prefix @ total) (outer : quantifier list) (inner : quantifier list) =
  match outer with
  | [] -> inner
  | head :: rest -> head :: append_prefix rest inner

let[@def] (normalized_game @ total) prefix env guard witness =
  admissible prefix env guard && game prefix env guard witness

let[@def] rec (scoped_prefix @ total) (prefix : quantifier list) depth q =
  match prefix with
  | [] -> scoped_qf depth q
  | _ :: rest -> scoped_prefix rest (depth + 1) q
