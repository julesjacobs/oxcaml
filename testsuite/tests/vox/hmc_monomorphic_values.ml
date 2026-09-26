module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module L = Hmc_monomorphic_links
module E = Hmc_catalog_semantics
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
module V = Hm_interpreter_typing

type t = True | False | Word of Hmc_word64.t | Nil | Cons of t * t
  | Closure of T.catalog * C.term * t | Recursive_closure of T.catalog * C.term * t
  | Empty | Bind of t * t [@@inductive]
let[@def] rec (environment @ total) (v : t @ immutable) = match v with
  | Empty -> true | Bind (_, rest) -> environment rest | _ -> false
let[@def] rec (length @ total) (v : t @ immutable) = match v with
  | Bind (_, rest) -> D.S (length rest) | _ -> D.Z
let[@def] rec (valid @ total) (table : M.table @ immutable) (v : t @ immutable) = ghost_ (match v with
  | True | False | Word _ | Nil | Empty -> true
  | Cons (a, b) | Bind (a, b) -> valid table a && valid table b
  | Closure (catalog, code, env) -> valid table env && environment env
    && L.linked table catalog (D.S (length env)) code
  | Recursive_closure (catalog, code, env) -> valid table env && environment env
    && L.linked table catalog (D.S (D.S (length env))) code)
let[@def] rec (source @ total) (v : t @ immutable) = match v with
  | True -> V.True | False -> V.False | Word w -> V.Word w | Nil -> V.Nil | Empty -> V.Empty
  | Cons (a, b) -> V.Cons (source a, source b)
  | Bind (a, b) -> V.Bind (source a, source b)
  | Closure (catalog, code, env) -> V.Closure (C.erase code, E.append (source env) (E.environment catalog))
  | Recursive_closure (catalog, code, env) ->
    V.Recursive_closure (C.erase code, E.append (source env) (E.environment catalog))
let[@def] rec (target @ total) (v : t @ immutable) = match v with
  | True -> Q.V.True | False -> Q.V.False | Word w -> Q.V.Word w | Nil -> Q.V.Nil | Empty -> Q.V.Empty
  | Cons (a, b) -> Q.V.Cons (target a, target b)
  | Bind (a, b) -> Q.V.Bind (target a, target b)
  | Closure (_, code, env) -> Q.V.Closure (code, target env)
  | Recursive_closure (_, code, env) -> Q.V.Recursive_closure (code, target env)

let rec (local @ total) : (table : M.table) @ immutable -> (env : t) @ immutable ->
    (globals : V.value) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | environment env && valid table env && L.strip (length env) i === None} ->
    {v : t | valid table v
      && S.lookup (E.append (source env) globals) i === Some (source v)
      && Q.lookup (target env) i === Some (target v)} @ immutable = fun table env globals i premise ->
  ghost_ (environment_def env; valid_def table env; length_def env; L.strip_def (length env) i;
    source_def env; target_def env; E.append_def (source env) globals;
    S.lookup_def (E.append (source env) globals) i; Q.lookup_def (target env) i);
  match env, i with
  | Bind (head, _), D.Z -> head
  | Bind (_, tail), D.S i -> local table tail globals i ()
  | _ -> unreachable_ ()

let rec (global @ total) : (env : t) @ immutable -> (globals : V.value) @ immutable ->
    (i : D.index) @ immutable -> (j : D.index) @ immutable ->
    {u : unit | environment env && L.strip (length env) i === Some j} ->
    {u : unit | S.lookup (E.append (source env) globals) i === S.lookup globals j} @ ghost =
  fun env globals i j premise -> ghost_ (
    environment_def env; length_def env; L.strip_def (length env) i; source_def env;
    E.append_def (source env) globals; S.lookup_def (E.append (source env) globals) i;
    match env, i with Bind (_, tail), D.S i -> global tail globals i j () | _ -> ())

let (primitive @ total) : (table : M.table) @ immutable -> (op : D.word_operation) @ immutable ->
    (a : Hmc_word64.t) @ immutable -> (b : Hmc_word64.t) @ immutable ->
    {r : t | valid table r && source r === S.primitive op a b && target r === Q.primitive op a b} @ immutable =
  fun table op a b ->
    let r = match op with
    | D.Add -> Word (Hmc_word64.add a b)
    | D.Subtract -> Word (Hmc_word64.subtract a b)
    | D.Equal_word -> if Hmc_word64.equal a b then True else False
    | D.Unsigned_less -> if Hmc_word64.unsigned_less a b then True else False in
    ghost_ (source_def r; target_def r; valid_def table r; S.primitive_def op a b; Q.primitive_def op a b);
    r
