open Copy_spec
open Level_spec
module D = Hm_declarative

type env = Empty | Bind of node Pref.t * env [@@inductive]
type templates = No_templates | Template_binding of template * templates [@@inductive]
let[@def] rec (lookup @ total) (env : env @ immutable) (i : D.index @ immutable) =
  match env with Empty -> None | Bind (p, rest) ->
  match i with D.Z -> Some p | D.S i -> lookup rest i
let[@def] rec (template_lookup @ total) (env : templates @ immutable) (i : D.index @ immutable) =
  match env with No_templates -> None | Template_binding (s, rest) ->
  match i with D.Z -> Some s | D.S i -> template_lookup rest i
let[@def] rec (boundary_member @ total) (s : template @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (match s with Boundary q -> p === q | Parameter _ | Constant _ -> false
  | Product (_, a, b) -> boundary_member a p || boundary_member b p
  | Indirect (_, child) -> boundary_member child p)
let[@def] rec (boundary_bound @ total) (h : Pref.heap @ immutable)
    (depth : int) (s : template @ immutable) = ghost_ (match s with
  | Boundary p -> below h p depth | Parameter _ | Constant _ -> true
  | Product (_, a, b) -> boundary_bound h depth a && boundary_bound h depth b
  | Indirect (_, child) -> boundary_bound h depth child)
let[@def] rec (env_at @ total) (h : Pref.heap @ immutable) (depth : int)
    (env : env @ immutable) (ts : templates @ immutable) = ghost_ (match env with
  | Empty -> (match ts with No_templates -> true | _ -> false)
  | Bind (p, rest) -> match ts with
    | Template_binding (s, tail) -> p === root s
      && template h s && boundary_bound h depth s && env_at h depth rest tail
    | _ -> false)
let[@def] rec (aligned @ total) (g : D.context @ immutable) (ts : templates @ immutable) =
  match g with D.Empty_context -> (match ts with No_templates -> true | _ -> false)
  | D.Binding (_, rest) -> match ts with
    | Template_binding (_, tail) -> aligned rest tail | _ -> false


let[@def] (protected_at @ total) (h : Pref.heap @ immutable)
    (after : Pref.heap @ immutable) (depth : int) (p : node Pref.t @ immutable) = ghost_ (
  (not (H.mem h p) || H.mem after p)
  && (not (below h p depth) || below after p depth)
  && match H.at h p with
    | Some v -> (match v.level with Finite _ -> true | Generic ->
      match H.at after p with Some w -> w.level === Generic && w.desc === v.desc
      | None -> false)
    | None -> true)

let[@def] rec (environment_boundary @ total) (ts : templates @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (match ts with
  | No_templates -> false
  | Template_binding (s, rest) -> boundary_member s p || environment_boundary rest p)
