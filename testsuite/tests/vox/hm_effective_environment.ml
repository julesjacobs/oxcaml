open Copy_spec
open Hm_environment_spec
module E = Effective_level
module T = Effective_template
module D = Hm_declarative

let[@def] rec (effective_env @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (depth : int) (env : env @ immutable)
    (ts : templates @ immutable) = ghost_ (match env with
  | Empty -> (match ts with No_templates -> true | _ -> false)
  | Bind (p, rest) -> match ts with
    | Template_binding (s, tail) -> p === root s && T.valid_template h heads s
      && T.boundary_bound h heads depth s && effective_env h heads depth rest tail
    | _ -> false)

let rec (lookup_schema @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (depth : int) -> (env : env) @ immutable ->
    (schemas : templates) @ immutable -> (i : D.index) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | effective_env h heads depth env schemas && lookup env i === Some p} ->
    {s : template | template_lookup schemas i === Some s && root s === p
      && T.valid_template h heads s && T.boundary_bound h heads depth s} @ immutable ghost =
  fun h heads depth env schemas i p premise -> ghost_ (
    effective_env_def h heads depth env schemas;
    lookup_def env i; template_lookup_def schemas i;
    match env, schemas with
    | Bind (_, rest), Template_binding (s, tail) -> (match i with D.Z -> s
      | D.S j -> let s = lookup_schema h heads depth rest tail j p () in s)
    | _ -> let s = Boundary p in s)

let (bind_schema @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (depth : int) -> (env : env) @ immutable ->
    (schemas : templates) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | effective_env h heads depth env schemas && E.effective_below h heads p depth} ->
    {u : unit | effective_env h heads depth (Bind (p, env)) (Template_binding (Boundary p, schemas))} @ ghost =
  fun h heads depth env schemas p premise -> ghost_ (
    let schema = Boundary p in let env1 = Bind (p, env) in
    let ts = Template_binding (schema, schemas) in effective_env_def h heads depth env1 ts;
    root_def schema; T.valid_template_def h heads schema; T.boundary_bound_def h heads depth schema;
    T.finite_def h heads p; E.effective_below_def h heads p depth; ())

let rec (transport @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (after : node Pref.heap) @ immutable ->
    (next : E.heads) @ total -> (depth : int) ->
    (frame : ((p : node Pref.t) @ immutable ->
      {u : unit | T.protected h heads after next depth p})) @ total ->
    (env : env) @ immutable -> (schemas : templates) @ immutable ->
    {u : unit | effective_env h heads depth env schemas} ->
    {u : unit | effective_env after next depth env schemas} @ ghost =
  fun h heads after next depth frame env schemas premise -> ghost_ (
    effective_env_def h heads depth env schemas;
    effective_env_def after next depth env schemas; match env with Empty -> ()
    | Bind (_, rest) -> match schemas with No_templates -> ()
      | Template_binding (s, tail) ->
        T.transport h heads after next depth frame s ();
        transport h heads after next depth frame rest tail (); ())
