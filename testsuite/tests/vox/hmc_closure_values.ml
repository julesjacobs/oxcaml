module D = Hm_declarative
module K = Hmc_closure_ir
module Q = Hmc_monomorphic_semantics
module R = Hmc_closure_semantics

let[@def] rec (environment @ total) (env : R.V.value @ immutable) = match env with
  | R.V.Empty -> true | R.V.Bind (_, rest) -> environment rest | _ -> false
let[@def] rec (valid @ total) (table : K.table @ immutable) (v : R.V.value @ immutable) = ghost_ (match v with
  | R.V.True | R.V.False | R.V.Word _ | R.V.Nil | R.V.Empty -> true
  | R.V.Cons (a, b) | R.V.Bind (a, b) -> valid table a && valid table b
  | R.V.Closure (id, env) -> valid table env && environment env && not (K.lookup table id === None))
let[@def] rec (source @ total) (table : K.table @ immutable) (v : R.V.value @ immutable) = match v with
  | R.V.True -> Q.V.True | R.V.False -> Q.V.False | R.V.Word w -> Q.V.Word w
  | R.V.Nil -> Q.V.Nil | R.V.Empty -> Q.V.Empty
  | R.V.Cons (a, b) -> Q.V.Cons (source table a, source table b)
  | R.V.Bind (a, b) -> Q.V.Bind (source table a, source table b)
  | R.V.Closure (id, env) -> (match K.lookup table id with None -> Q.V.Empty | Some entry ->
    if entry.K.recursive then Q.V.Recursive_closure (entry.K.source, source table env)
    else Q.V.Closure (entry.K.source, source table env))
let rec (local @ total) : (table : K.table) @ immutable -> (env : R.V.value) @ immutable ->
    (id : D.index) @ immutable -> (v : R.V.value) @ immutable ->
    {u : unit | valid table env && R.lookup env id === Some v} ->
    {u : unit | valid table v && Q.lookup (source table env) id === Some (source table v)} @ ghost =
  fun table env id v premise -> ghost_ (
    valid_def table env; source_def table env; R.lookup_def env id; Q.lookup_def (source table env) id;
    match env, id with R.V.Bind (_, rest), D.S i -> local table rest i v () | _ -> ())
let rec (absent @ total) : (table : K.table) @ immutable -> (env : R.V.value) @ immutable ->
    (id : D.index) @ immutable -> {u : unit | R.lookup env id === None} ->
    {u : unit | Q.lookup (source table env) id === None} @ ghost = fun table env id premise -> ghost_ (
  source_def table env; R.lookup_def env id; Q.lookup_def (source table env) id;
  match env, id with R.V.Bind (_, rest), D.S i -> absent table rest i () | _ -> ())
let (primitive @ total) : (table : K.table) @ immutable -> (op : D.word_operation) @ immutable ->
    (a : Hmc_word64.t) @ immutable -> (b : Hmc_word64.t) @ immutable ->
    {u : unit | valid table (R.primitive op a b) && source table (R.primitive op a b) === Q.primitive op a b} @ ghost =
  fun table op a b -> ghost_ (
    R.primitive_def op a b; Q.primitive_def op a b;
    valid_def table (R.primitive op a b); source_def table (R.primitive op a b))
