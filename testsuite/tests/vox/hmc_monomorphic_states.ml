module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module L = Hmc_monomorphic_links
module E = Hmc_catalog_semantics
module W = Hmc_monomorphic_values
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics

let[@def] (source_environment @ total) (catalog : T.catalog @ immutable) (env : W.t @ immutable) =
  E.append (W.source env) (E.environment catalog)
let[@def] (environment_valid @ total) (table : M.table @ immutable) (env : W.t @ immutable) = ghost_ (
  W.valid table env && W.environment env)

type control = Evaluate of T.catalog * W.t * C.term | Return of W.t [@@inductive]
type continuation = Halt
  | Apply_function of T.catalog * W.t * C.term * continuation
  | Apply_argument of W.t * continuation
  | Let_body of T.catalog * W.t * C.term * continuation
  | Cons_head of T.catalog * W.t * C.term * continuation
  | Cons_tail of W.t * continuation
  | List_cases of T.catalog * W.t * C.term * C.term * continuation
  | Conditional of T.catalog * W.t * C.term * C.term * continuation
  | Primitive_left of D.word_operation * T.catalog * W.t * C.term * continuation
  | Primitive_right of D.word_operation * W.t * continuation
  [@@inductive]
type state = Running of control * continuation | Done of W.t | Stuck [@@inductive]

let[@def] (source_control @ total) (c : control @ immutable) = match c with
  | Evaluate (catalog, env, code) -> S.Evaluate (source_environment catalog env, C.erase code)
  | Return v -> S.Return (W.source v)
let[@def] rec (source_continuation @ total) (k : continuation @ immutable) = match k with
  | Halt -> S.Halt
  | Apply_function (catalog, env, arg, rest) -> S.Apply_function (source_environment catalog env, C.erase arg, source_continuation rest)
  | Apply_argument (v, rest) -> S.Apply_argument (W.source v, source_continuation rest)
  | Let_body (catalog, env, body, rest) -> S.Let_body (source_environment catalog env, C.erase body, source_continuation rest)
  | Cons_head (catalog, env, tail, rest) -> S.Cons_head (source_environment catalog env, C.erase tail, source_continuation rest)
  | Cons_tail (v, rest) -> S.Cons_tail (W.source v, source_continuation rest)
  | List_cases (catalog, env, empty, nonempty, rest) -> S.List_cases (source_environment catalog env, C.erase empty, C.erase nonempty, source_continuation rest)
  | Conditional (catalog, env, yes, no, rest) -> S.Conditional (source_environment catalog env, C.erase yes, C.erase no, source_continuation rest)
  | Primitive_left (op, catalog, env, right, rest) -> S.Primitive_left (op, source_environment catalog env, C.erase right, source_continuation rest)
  | Primitive_right (op, v, rest) -> S.Primitive_right (op, W.source v, source_continuation rest)
let[@def] (source @ total) (state : state @ immutable) = match state with
  | Running (c, k) -> S.Running (source_control c, source_continuation k)
  | Done v -> S.Done (W.source v) | Stuck -> S.Stuck
let[@def] (target_control @ total) (c : control @ immutable) = match c with
  | Evaluate (_, env, code) -> Q.Evaluate (W.target env, code)
  | Return v -> Q.Return (W.target v)
let[@def] rec (target_continuation @ total) (k : continuation @ immutable) = match k with
  | Halt -> Q.Halt
  | Apply_function (catalog, env, arg, rest) -> Q.Apply_function (W.target env, arg, target_continuation rest)
  | Apply_argument (v, rest) -> Q.Apply_argument (W.target v, target_continuation rest)
  | Let_body (catalog, env, body, rest) -> Q.Let_body (W.target env, body, target_continuation rest)
  | Cons_head (catalog, env, tail, rest) -> Q.Cons_head (W.target env, tail, target_continuation rest)
  | Cons_tail (v, rest) -> Q.Cons_tail (W.target v, target_continuation rest)
  | List_cases (catalog, env, empty, nonempty, rest) -> Q.List_cases (W.target env, empty, nonempty, target_continuation rest)
  | Conditional (catalog, env, yes, no, rest) -> Q.Conditional (W.target env, yes, no, target_continuation rest)
  | Primitive_left (op, catalog, env, right, rest) -> Q.Primitive_left (op, W.target env, right, target_continuation rest)
  | Primitive_right (op, v, rest) -> Q.Primitive_right (op, W.target v, target_continuation rest)
let[@def] (target @ total) (state : state @ immutable) = match state with
  | Running (c, k) -> Q.Running (target_control c, target_continuation k)
  | Done v -> Q.Done (W.target v) | Stuck -> Q.Stuck
let[@def] (control_valid @ total) (table : M.table @ immutable) (c : control @ immutable) = ghost_ ( match c with
  | Evaluate (catalog, env, code) -> environment_valid table env && L.linked table catalog (W.length env) code
  | Return v -> W.valid table v)

let[@def] rec (continuation_valid @ total) (table : M.table @ immutable) (k : continuation @ immutable) = ghost_ (
  match k with Halt -> true
  | Apply_function (catalog, env, arg, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (W.length env) arg
  | Apply_argument (v, rest) -> continuation_valid table rest && W.valid table v
  | Let_body (catalog, env, body, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (D.S (W.length env)) body
  | Cons_head (catalog, env, tail, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (W.length env) tail
  | Cons_tail (v, rest) -> continuation_valid table rest && W.valid table v
  | List_cases (catalog, env, empty, nonempty, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (W.length env) empty && L.linked table catalog (D.S (D.S (W.length env))) nonempty
  | Conditional (catalog, env, yes, no, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (W.length env) yes && L.linked table catalog (W.length env) no
  | Primitive_left (op, catalog, env, right, rest) -> continuation_valid table rest && environment_valid table env && L.linked table catalog (W.length env) right
  | Primitive_right (op, v, rest) -> continuation_valid table rest && W.valid table v)

let[@def] (valid @ total) (table : M.table @ immutable) (state : state @ immutable) = ghost_ ( match state with
  | Running (c, k) -> control_valid table c && continuation_valid table k
  | Done v -> W.valid table v | Stuck -> true)

