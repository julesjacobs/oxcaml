module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module E = Hm_evaluation

type completion =
  | Halt
  | Pass of completion
  | Evaluate of V.value * E.execution * completion
  [@@inductive]

let[@def] rec (completes @ total) (k : S.continuation @ immutable)
    (value : V.value @ immutable) (result : V.value @ immutable)
    (proof : completion @ immutable) = ghost_ (
  match k, proof with
  | S.Halt, Halt -> value === result
  | S.Apply_function (env, arg, rest), Evaluate (v, e, next) ->
    E.evaluates env arg v e && completes (S.Apply_argument (value, rest)) v result next
  | S.Apply_argument (fn, rest), Evaluate (v, e, next) ->
    completes rest v result next && (match fn with
    | V.Closure (body, env) -> E.evaluates (V.Bind (value, env)) body v e
    | V.Recursive_closure (body, env) -> E.evaluates (V.Bind (value, V.Bind (fn, env))) body v e
    | _ -> false)
  | S.Let_body (env, body, rest), Evaluate (v, e, next) ->
    E.evaluates (V.Bind (value, env)) body v e && completes rest v result next
  | S.Cons_head (env, tail, rest), Evaluate (v, e, next) ->
    E.evaluates env tail v e && completes (S.Cons_tail (value, rest)) v result next
  | S.Cons_tail (head, rest), Pass next -> completes rest (V.Cons (head, value)) result next
  | S.List_cases (env, empty, nonempty, rest), Evaluate (v, e, next) ->
    completes rest v result next && (match value with
    | V.Nil -> E.evaluates env empty v e
    | V.Cons (head, tail) -> E.evaluates (V.Bind (head, V.Bind (tail, env))) nonempty v e
    | _ -> false)
  | S.Conditional (env, yes, no, rest), Evaluate (v, e, next) ->
    completes rest v result next && (match value with
    | V.True -> E.evaluates env yes v e | V.False -> E.evaluates env no v e
    | _ -> false)
  | S.Primitive_left (op, env, right, rest), Evaluate (v, e, next) ->
    E.evaluates env right v e && completes (S.Primitive_right (op, value, rest)) v result next
  | S.Primitive_right (op, left, rest), Pass next -> (match left, value with
    | V.Word a, V.Word b -> completes rest (S.primitive op a b) result next
    | _ -> false)
  | _ -> false)

type execution = Done | Return of completion | Evaluate_term of V.value * E.execution * completion
  [@@inductive]

let[@def] (finishes @ total) (state : S.state @ immutable)
    (result : V.value @ immutable) (proof : execution @ immutable) = ghost_ (
  match state, proof with
  | S.Done value, Done -> value === result
  | S.Running (S.Return value, k), Return p -> completes k value result p
  | S.Running (S.Evaluate (env, term), k), Evaluate_term (v, e, p) ->
    E.evaluates env term v e && completes k v result p
  | _ -> false)
