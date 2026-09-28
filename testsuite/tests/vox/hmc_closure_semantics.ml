module D = Hm_declarative
module C = Hmc_closure_ir
module P = Hmc_closure_program
module V = struct
  type value = True | False | Word of Hmc_word64.t | Nil | Cons of value * value
    | Closure of D.index * value
    | Empty | Bind of value * value [@@inductive]
end

type control = Evaluate of V.value * C.term | Return of V.value [@@inductive]
type continuation =
  | Halt
  | Apply_function of V.value * C.term * continuation
  | Apply_argument of V.value * continuation
  | Let_body of V.value * C.term * continuation
  | Cons_head of V.value * C.term * continuation
  | Cons_tail of V.value * continuation
  | List_cases of V.value * C.term * C.term * continuation
  | Conditional of V.value * C.term * C.term * continuation
  | Primitive_left of D.word_operation * V.value * C.term * continuation
  | Primitive_right of D.word_operation * V.value * continuation
  [@@inductive]
type state = Running of control * continuation | Done of V.value | Stuck [@@inductive]

let[@def] rec (lookup @ total) (env : V.value @ immutable)
    (index : D.index @ immutable) = match env with
  | V.Bind (head, tail) -> (match index with D.Z -> Some head | D.S i -> lookup tail i)
  | _ -> None

let[@def] (primitive @ total) (op : D.word_operation @ immutable)
    (a : Hmc_word64.t @ immutable) (b : Hmc_word64.t @ immutable) =
  match op with
  | D.Add -> V.Word (Hmc_word64.add a b)
  | D.Subtract -> V.Word (Hmc_word64.subtract a b)
  | D.Equal_word -> if Hmc_word64.equal a b then V.True else V.False
  | D.Unsigned_less -> if Hmc_word64.unsigned_less a b then V.True else V.False

let[@def] (initial @ total) (term : C.term @ immutable) =
  Running (Evaluate (V.Empty, term), Halt)

let[@def] (step @ total) (table : C.table @ immutable) (globals : P.globals @ immutable) (state : state @ immutable) = match state with
  | Done _ | Stuck -> state
  | Running (Evaluate (env, term), k) -> (match term with
    | C.Local index -> (match lookup env index with
      | None -> Stuck | Some v -> Running (Return v, k))
    | C.Global id -> (match P.lookup globals id with
      | None -> Stuck | Some code -> Running (Return (V.Closure (code, V.Empty)), k))
    | C.Closure id -> Running (Return (V.Closure (id, env)), k)
    | C.Truth -> Running (Return V.True, k)
    | C.False -> Running (Return V.False, k)
    | C.Word w -> Running (Return (V.Word w), k)
    | C.Nil -> Running (Return V.Nil, k)
    | C.Apply (f, a) -> Running (Evaluate (env, f), Apply_function (env, a, k))
    | C.Let (rhs, body) -> Running (Evaluate (env, rhs), Let_body (env, body, k))
    | C.Cons (head, tail) -> Running (Evaluate (env, head), Cons_head (env, tail, k))
    | C.CaseList (s, l, r) -> Running (Evaluate (env, s), List_cases (env, l, r, k))
    | C.If (c, a, b) -> Running (Evaluate (env, c), Conditional (env, a, b, k))
    | C.Primitive (op, a, b) -> Running (Evaluate (env, a), Primitive_left (op, env, b, k)))
  | Running (Return v, k) -> (match k with
    | Halt -> Done v
    | Apply_function (env, arg, rest) -> Running (Evaluate (env, arg), Apply_argument (v, rest))
    | Apply_argument (f, rest) -> (match f with
      | V.Closure (id, env) -> (match C.lookup table id with
        | None -> Stuck
        | Some entry ->
          let captured = if entry.C.recursive then V.Bind (f, env) else env in
          Running (Evaluate (V.Bind (v, captured), entry.C.body), rest))
      | _ -> Stuck)
    | Let_body (env, body, rest) -> Running (Evaluate (V.Bind (v, env), body), rest)
    | Cons_head (env, tail, rest) -> Running (Evaluate (env, tail), Cons_tail (v, rest))
    | Cons_tail (head, rest) -> Running (Return (V.Cons (head, v)), rest)
    | List_cases (env, empty, nonempty, rest) -> (match v with
      | V.Nil -> Running (Evaluate (env, empty), rest)
      | V.Cons (head, tail) -> Running (Evaluate (V.Bind (head, V.Bind (tail, env)), nonempty), rest)
      | _ -> Stuck)
    | Conditional (env, yes, no, rest) -> (match v with
      | V.True -> Running (Evaluate (env, yes), rest)
      | V.False -> Running (Evaluate (env, no), rest)
      | _ -> Stuck)
    | Primitive_left (op, env, right, rest) ->
      Running (Evaluate (env, right), Primitive_right (op, v, rest))
    | Primitive_right (op, left, rest) -> (match left, v with
      | V.Word a, V.Word b -> Running (Return (primitive op a b), rest)
      | _ -> Stuck))

let[@def] rec (advance @ total) (table : C.table @ immutable) (globals : P.globals @ immutable)
    (fuel : D.index @ immutable) (state : state @ immutable) = match fuel with
  | D.Z -> state | D.S n -> advance table globals n (step table globals state)
