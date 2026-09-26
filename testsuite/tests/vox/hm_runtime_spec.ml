open Copy_spec
open Level_spec
open Generalize_spec

let[@def] (safe @ total) (h : node Pref.heap @ immutable) (x : node Pref.t @ immutable) = ghost_ (
  (if H.mem h x then source_ok h x else H.at h x === None)
  && ordered h x && match H.at h x with None -> true | Some v -> not v.visited && v.memo === Empty_memo)
let[@def] (depth_bound @ total) (h : node Pref.heap @ immutable) (depth : int) (x : node Pref.t @ immutable) = ghost_ (
  not (H.mem h x) || not (finite_node h x) || below h x depth)
let[@def] (runtime_at @ total) (h : node Pref.heap @ immutable) (depth : int)
    (pool : pool @ immutable) (x : node Pref.t @ immutable) = ghost_ (
  safe h x && depth_bound h depth x && covered h (depth - 1) pool x)

let[@def] rec (let_free @ total) (e : Hm_execution_spec.execution @ immutable) =
  match e with
  | Hm_execution_spec.RShared _ | Hm_execution_spec.RVar _ | Hm_execution_spec.RBool _ -> true
  | Hm_execution_spec.RLam (_, body, _, _, _) | Hm_execution_spec.RRec (_, _, _, body, _, _, _) -> let_free body
  | Hm_execution_spec.RApp_left (left, _) -> let_free left
  | Hm_execution_spec.RApp_right (left, right, _, _)
  | Hm_execution_spec.RApp (left, right, _, _, _, _, _, _, _, _) -> let_free left && let_free right
  | Hm_execution_spec.RLet_left _ | Hm_execution_spec.RLet _ -> false

let[@def] rec (term_let_free @ total) (e : Hm_declarative.term @ immutable) =
  match e with Hm_declarative.Bound _ | Hm_declarative.Truth
  | Hm_declarative.False | Hm_declarative.Word _ | Hm_declarative.Nil -> true
  | Hm_declarative.Lambda b | Hm_declarative.Recursive b -> term_let_free b
  | Hm_declarative.CaseList (a, b, c) | Hm_declarative.If (a, b, c) ->
    term_let_free a && term_let_free b && term_let_free c
  | Hm_declarative.Cons (a, b) | Hm_declarative.Primitive (_, a, b)
  | Hm_declarative.Apply (a, b) -> term_let_free a && term_let_free b
  | Hm_declarative.Let _ -> false
let[@def] rec (env_depth @ total) (env : Hm_environment_spec.env @ immutable) =
  match env with Hm_environment_spec.Empty -> Hm_declarative.Z
  | Hm_environment_spec.Bind (_, rest) -> Hm_declarative.S (env_depth rest)
let[@def] rec (env_owned @ total) (h : node Pref.heap @ immutable) (env : Hm_environment_spec.env @ immutable) = ghost_ (
  match env with Hm_environment_spec.Empty -> true
  | Hm_environment_spec.Bind (p, rest) -> H.mem h p && env_owned h rest)
