module C = Hmc_monomorphic
module K = Hmc_closure_ir
module Q = Hmc_monomorphic_semantics
module R = Hmc_closure_semantics
module W = Hmc_closure_values

type control = Evaluate of R.V.value * C.term * K.term | Return of R.V.value [@@inductive]
type continuation = Halt
  | Apply_function of R.V.value * C.term * K.term * continuation
  | Apply_argument of R.V.value * continuation
  | Let_body of R.V.value * C.term * K.term * continuation
  | Cons_head of R.V.value * C.term * K.term * continuation
  | Cons_tail of R.V.value * continuation
  | List_cases of R.V.value * C.term * K.term * C.term * K.term * continuation
  | Conditional of R.V.value * C.term * K.term * C.term * K.term * continuation
  | Primitive_left of Hm_declarative.word_operation * R.V.value * C.term * K.term * continuation
  | Primitive_right of Hm_declarative.word_operation * R.V.value * continuation
  [@@inductive]
type state = Running of control * continuation | Done of R.V.value | Stuck [@@inductive]

let[@def] (source_control @ total) (table : K.table @ immutable) (c : control @ immutable) = match c with
  | Evaluate (env, source, code) -> Q.Evaluate (W.source table env, source)
  | Return v -> Q.Return (W.source table v)
let[@def] rec (source_continuation @ total) (table : K.table @ immutable) (k : continuation @ immutable) = match k with
  | Halt -> Q.Halt
  | Apply_function (env, arg, arg_code, rest) -> Q.Apply_function (W.source table env, arg, source_continuation table rest)
  | Apply_argument (v, rest) -> Q.Apply_argument (W.source table v, source_continuation table rest)
  | Let_body (env, body, body_code, rest) -> Q.Let_body (W.source table env, body, source_continuation table rest)
  | Cons_head (env, tail, tail_code, rest) -> Q.Cons_head (W.source table env, tail, source_continuation table rest)
  | Cons_tail (v, rest) -> Q.Cons_tail (W.source table v, source_continuation table rest)
  | List_cases (env, empty, empty_code, nonempty, nonempty_code, rest) -> Q.List_cases (W.source table env, empty, nonempty, source_continuation table rest)
  | Conditional (env, yes, yes_code, no, no_code, rest) -> Q.Conditional (W.source table env, yes, no, source_continuation table rest)
  | Primitive_left (op, env, right, right_code, rest) -> Q.Primitive_left (op, W.source table env, right, source_continuation table rest)
  | Primitive_right (op, v, rest) -> Q.Primitive_right (op, W.source table v, source_continuation table rest)
let[@def] (source @ total) (table : K.table @ immutable) (state : state @ immutable) = match state with
  | Running (c, k) -> Q.Running (source_control table c, source_continuation table k)
  | Done v -> Q.Done (W.source table v) | Stuck -> Q.Stuck

let[@def] (target_control @ total) (c : control @ immutable) = match c with
  | Evaluate (env, source, code) -> R.Evaluate (env, code)
  | Return v -> R.Return (v)
let[@def] rec (target_continuation @ total) (k : continuation @ immutable) = match k with
  | Halt -> R.Halt
  | Apply_function (env, arg, arg_code, rest) -> R.Apply_function (env, arg_code, target_continuation rest)
  | Apply_argument (v, rest) -> R.Apply_argument (v, target_continuation rest)
  | Let_body (env, body, body_code, rest) -> R.Let_body (env, body_code, target_continuation rest)
  | Cons_head (env, tail, tail_code, rest) -> R.Cons_head (env, tail_code, target_continuation rest)
  | Cons_tail (v, rest) -> R.Cons_tail (v, target_continuation rest)
  | List_cases (env, empty, empty_code, nonempty, nonempty_code, rest) -> R.List_cases (env, empty_code, nonempty_code, target_continuation rest)
  | Conditional (env, yes, yes_code, no, no_code, rest) -> R.Conditional (env, yes_code, no_code, target_continuation rest)
  | Primitive_left (op, env, right, right_code, rest) -> R.Primitive_left (op, env, right_code, target_continuation rest)
  | Primitive_right (op, v, rest) -> R.Primitive_right (op, v, target_continuation rest)
let[@def] (target @ total) (state : state @ immutable) = match state with
  | Running (c, k) -> R.Running (target_control c, target_continuation k)
  | Done v -> R.Done (v) | Stuck -> R.Stuck

let[@def] (control_valid @ total) (table : K.table @ immutable) (c : control @ immutable) = ghost_ (match c with
  | Evaluate (env, source, code) -> W.valid table env && W.environment env && K.related table source code
  | Return v -> W.valid table v)
let[@def] rec (continuation_valid @ total) (table : K.table @ immutable) (k : continuation @ immutable) = ghost_ (
  match k with Halt -> true
  | Apply_function (env, arg, arg_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table arg arg_code
  | Apply_argument (v, rest) -> continuation_valid table rest && W.valid table v
  | Let_body (env, body, body_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table body body_code
  | Cons_head (env, tail, tail_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table tail tail_code
  | Cons_tail (v, rest) -> continuation_valid table rest && W.valid table v
  | List_cases (env, empty, empty_code, nonempty, nonempty_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table empty empty_code && K.related table nonempty nonempty_code
  | Conditional (env, yes, yes_code, no, no_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table yes yes_code && K.related table no no_code
  | Primitive_left (op, env, right, right_code, rest) -> continuation_valid table rest && W.valid table env && W.environment env && K.related table right right_code
  | Primitive_right (op, v, rest) -> continuation_valid table rest && W.valid table v)
let[@def] (valid @ total) (table : K.table @ immutable) (state : state @ immutable) = ghost_ (match state with
  | Running (c, k) -> control_valid table c && continuation_valid table k
  | Done v -> W.valid table v | Stuck -> true)
