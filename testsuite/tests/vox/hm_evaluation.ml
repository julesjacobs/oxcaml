module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics

type execution =
  | Atomic
  | Cons of V.value * execution * execution
  | Branch of V.value * execution * execution
  | Primitive of V.value * V.value * execution * execution
  | Let of V.value * execution * execution
  | Apply of V.value * V.value * execution * execution * execution
  [@@inductive]

let[@def] rec (evaluates @ total) (env : V.value @ immutable)
    (term : D.term @ immutable) (value : V.value @ immutable)
    (execution : execution @ immutable) = ghost_ (
  match term, execution with
  | D.Truth, Atomic -> value === V.True
  | D.False, Atomic -> value === V.False
  | D.Word word, Atomic -> value === V.Word word
  | D.Nil, Atomic -> value === V.Nil
  | D.Bound index, Atomic -> S.lookup env index === Some value
  | D.Lambda body, Atomic -> value === V.Closure (body, env)
  | D.Recursive body, Atomic -> value === V.Recursive_closure (body, env)
  | D.Cons (head, tail), Cons (h, a, b) ->
    (match value with V.Cons (v, rest) -> v === h
      && evaluates env head h a && evaluates env tail rest b | _ -> false)
  | D.CaseList (scrutinee, empty, nonempty), Branch (v, a, b) ->
    evaluates env scrutinee v a && (match v with
      | V.Nil -> evaluates env empty value b
      | V.Cons (head, tail) -> evaluates (V.Bind (head, V.Bind (tail, env))) nonempty value b
      | _ -> false)
  | D.If (condition, yes, no), Branch (v, a, b) ->
    evaluates env condition v a && (match v with
      | V.True -> evaluates env yes value b
      | V.False -> evaluates env no value b
      | _ -> false)
  | D.Primitive (op, left, right), Primitive (l, r, a, b) ->
    evaluates env left l a && evaluates env right r b && (match l, r with
      | V.Word x, V.Word y -> value === S.primitive op x y
      | _ -> false)
  | D.Let (rhs, body), Let (v, a, b) ->
    evaluates env rhs v a && evaluates (V.Bind (v, env)) body value b
  | D.Apply (fn, arg), Apply (f, v, a, b, c) ->
    evaluates env fn f a && evaluates env arg v b && (match f with
      | V.Closure (body, captured) -> evaluates (V.Bind (v, captured)) body value c
      | V.Recursive_closure (body, captured) ->
        evaluates (V.Bind (v, V.Bind (f, captured))) body value c
      | _ -> false)
  | _ -> false)
