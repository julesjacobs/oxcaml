module D = Hm_declarative
module V = Hm_interpreter_typing

type control = Evaluate of V.value * D.term | Return of V.value [@@inductive]
type continuation =
  | Halt
  | Apply_function of V.value * D.term * continuation
  | Apply_argument of V.value * continuation
  | Let_body of V.value * D.term * continuation
  | Cons_head of V.value * D.term * continuation
  | Cons_tail of V.value * continuation
  | List_cases of V.value * D.term * D.term * continuation
  | Conditional of V.value * D.term * D.term * continuation
  | Primitive_left of D.word_operation * V.value * D.term * continuation
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

let[@def] (initial @ total) (term : D.term @ immutable) =
  Running (Evaluate (V.Empty, term), Halt)

let[@def] (step @ total) (state : state @ immutable) = match state with
  | Done _ | Stuck -> state
  | Running (Evaluate (env, term), k) -> (match term with
    | D.Bound index -> (match lookup env index with
      | None -> Stuck | Some v -> Running (Return v, k))
    | D.Truth -> Running (Return V.True, k)
    | D.False -> Running (Return V.False, k)
    | D.Word w -> Running (Return (V.Word w), k)
    | D.Nil -> Running (Return V.Nil, k)
    | D.Lambda body -> Running (Return (V.Closure (body, env)), k)
    | D.Recursive body -> Running (Return (V.Recursive_closure (body, env)), k)
    | D.Apply (f, a) -> Running (Evaluate (env, f), Apply_function (env, a, k))
    | D.Let (rhs, body) -> Running (Evaluate (env, rhs), Let_body (env, body, k))
    | D.Cons (head, tail) -> Running (Evaluate (env, head), Cons_head (env, tail, k))
    | D.CaseList (s, l, r) -> Running (Evaluate (env, s), List_cases (env, l, r, k))
    | D.If (c, a, b) -> Running (Evaluate (env, c), Conditional (env, a, b, k))
    | D.Primitive (op, a, b) -> Running (Evaluate (env, a), Primitive_left (op, env, b, k)))
  | Running (Return v, k) -> (match k with
    | Halt -> Done v
    | Apply_function (env, arg, rest) -> Running (Evaluate (env, arg), Apply_argument (v, rest))
    | Apply_argument (f, rest) -> (match f with
      | V.Closure (body, env) -> Running (Evaluate (V.Bind (v, env), body), rest)
      | V.Recursive_closure (body, env) ->
        Running (Evaluate (V.Bind (v, V.Bind (f, env)), body), rest)
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

let[@def] rec (advance @ total) (fuel : D.index @ immutable)
    (state : state @ immutable) = match fuel with
  | D.Z -> state | D.S n -> advance n (step state)

let rec (advance_add @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> (state : state) @ immutable ->
    {u : unit | advance (D.add a b) state === advance b (advance a state)}
      @ ghost = fun a b state -> ghost_ (
  D.add_def a b; advance_def a state; advance_def (D.add a b) state;
  match a with D.Z -> () | D.S n -> advance_add n b (step state))

let rec (done_absorbing @ total) : (fuel : D.index) @ immutable ->
    (value : V.value) @ immutable ->
    {u : unit | advance fuel (Done value) === Done value} @ ghost =
  fun fuel value -> ghost_ (
    advance_def fuel (Done value); step_def (Done value);
    match fuel with D.Z -> () | D.S n -> done_absorbing n value)

let (normal_result_stable @ total) : (a : D.index) @ immutable ->
    (extra : D.index) @ immutable -> (state : state) @ immutable ->
    (v : V.value) @ immutable -> (w : V.value) @ immutable ->
    {u : unit | advance a state === Done v
      && advance (D.add a extra) state === Done w} ->
    {u : unit | v === w} @ ghost = fun a extra state v w premise -> ghost_ (
  advance_add a extra state; done_absorbing extra v; ())

type distance = Earlier of D.index | Later of D.index [@@inductive]

let rec (compare_steps @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> {r : distance | match r with
      | Earlier extra -> D.add a extra === b
      | Later extra -> D.add b extra === a} @ immutable = fun a b ->
  match a, b with
  | D.Z, _ -> ghost_ (D.add_def D.Z b); Earlier b
  | _, D.Z -> ghost_ (D.add_def D.Z a); Later a
  | D.S m, D.S n ->
    let r = compare_steps m n in
    ghost_ (match r with
      | Earlier extra -> D.add_def a extra
      | Later extra -> D.add_def b extra);
    r

let (normal_result_unique @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> (state : state) @ immutable ->
    (v : V.value) @ immutable -> (w : V.value) @ immutable ->
    {u : unit | advance a state === Done v && advance b state === Done w} ->
    {u : unit | v === w} @ ghost = fun a b state v w premise -> ghost_ (
  match compare_steps a b with
  | Earlier extra -> normal_result_stable a extra state v w ()
  | Later extra -> normal_result_stable b extra state w v ())

type observation = Finished of V.value | Invalid | Suspended of state [@@inductive]

let (observe @ total) : (fuel : D.index) @ immutable ->
    (state : state) @ immutable ->
    {r : observation | match r with
      | Finished v -> advance fuel state === Done v
      | Invalid -> advance fuel state === Stuck
      | Suspended s -> advance fuel state === s
        && (match s with Running _ -> true | Done _ | Stuck -> false)} @ immutable =
  fun fuel state ->
  match advance fuel state with
  | Done v -> Finished v | Stuck -> Invalid | Running _ as s -> Suspended s
