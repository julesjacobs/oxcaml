open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
module D = Hm_declarative

type finish = Aborted | Unified of bool * Level_unifier_spec.derivation [@@inductive]
type execution =
  | RVar of D.index * node Pref.t * node Pref.t * history
  | RBool of node Pref.t
  | RLam of node Pref.t * execution * node Pref.heap * pool * node Pref.t option
  | RApp_left of execution * D.term
  | RApp_right of execution * execution * node Pref.heap * pool
  | RApp of execution * execution * node Pref.heap * pool * node Pref.heap * pool
      * node Pref.t * node Pref.t * bool * Level_unifier_spec.derivation
  | RRec of node Pref.t * node Pref.t * node Pref.t * execution * node Pref.heap * pool * finish
  | RLet_left of execution * D.term
  | RLet of execution * execution * node Pref.heap * pool
  [@@inductive]

let[@def] rec (result @ total) (e : execution @ immutable) = match e with
  | RVar (_, p, _, _) | RBool p -> Some p
  | RLam (_, _, _, _, p) -> p
  | RApp_left _ | RApp_right _ | RLet_left _ -> None
  | RApp (_, _, _, _, _, _, p, _, ok, _) -> if ok then Some p else None
  | RRec (_, _, p, _, _, _, finish) -> (match finish with
    | Aborted -> None | Unified (ok, _) -> if ok then Some p else None)
  | RLet (_, body, _, _) -> result body
let[@def] rec (source @ total) (e : execution @ immutable) = match e with
  | RVar (i, _, _, _) -> D.Bound i | RBool _ -> D.Truth
  | RLam (_, body, _, _, _) -> D.Lambda (source body)
  | RApp_left (left, right) -> D.Apply (source left, right)
  | RApp_right (left, right, _, _) | RApp (left, right, _, _, _, _, _, _, _, _) ->
    D.Apply (source left, source right)
  | RRec (_, _, _, body, _, _, _) -> D.Recursive (source body)
  | RLet_left (rhs, body) -> D.Let (source rhs, body)
  | RLet (rhs, body, _, _) -> D.Let (source rhs, source body)
let[@def] (allocated @ total) (h : node Pref.heap @ immutable) (depth : int)
    (p : node Pref.t @ immutable) (desc : desc @ immutable) = ghost_ (
  not (H.mem h p) && depth >= 0 && children_below h desc depth)
let[@def] (copy_heap @ total) (h : node Pref.heap @ immutable) (epoch : node Pref.t @ immutable)
    (depth : int) (d : history @ immutable) = ghost_ (
  Copy_cleanup_spec.swept (heap h epoch depth d) (Pooled_spec.touched d))

let[@def] rec (ran @ total) (h : node Pref.heap @ immutable) (depth : int)
    (pool : pool @ immutable) (env : env @ immutable) (e : execution @ immutable)
    (after : node Pref.heap @ immutable) (final_pool : pool @ immutable) = ghost_ (
  depth >= 0 && pool_scoped h pool && match e with
  | RVar (i, p, epoch, d) -> (match lookup env i with None -> false | Some original ->
      valid h epoch depth d && target_for h d original p
      && after === copy_heap h epoch depth d
      && final_pool === Pooled_spec.registered pool epoch d)
  | RBool p -> allocated h depth p Bool
    && after === H.put h p (cell Bool depth) && final_pool === Entry (p, pool)
  | RLam (arg, body, middle, body_pool, out) -> allocated h depth arg Var
    && ran (H.put h arg (cell Var depth)) depth (Entry (arg, pool))
      (Bind (arg, env)) body middle body_pool
    && (match result body with
      | None -> out === None && after === middle && final_pool === body_pool
      | Some b -> match out with None -> false | Some p ->
        allocated middle depth p (Arrow (arg, b))
        && after === H.put middle p (cell (Arrow (arg, b)) depth)
        && final_pool === Entry (p, body_pool))
  | RApp_left (left, _) -> ran h depth pool env left after final_pool
    && result left === None
  | RApp_right (left, right, middle, left_pool) -> ran h depth pool env left middle left_pool
    && (match result left with None -> false | Some _ -> true)
    && ran middle depth left_pool env right after final_pool && result right === None
  | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
    ran h depth pool env left h1 pool1 && ran h1 depth pool1 env right h2 pool2
    && (match result left with None -> false | Some f ->
      match result right with None -> false | Some a ->
      allocated h2 depth p Var
      && allocated (H.put h2 p (cell Var depth)) depth arrow (Arrow (a, p))
      && Level_unifier_spec.unified
        (H.put (H.put h2 p (cell Var depth)) arrow (cell (Arrow (a, p)) depth))
        f arrow ok after d)
    && final_pool === Entry (arrow, Entry (p, pool2))
  | RRec (arg, res, self, body, middle, body_pool, finish) ->
    allocated h depth arg Var
    && allocated (H.put h arg (cell Var depth)) depth res Var
    && allocated (H.put (H.put h arg (cell Var depth)) res (cell Var depth))
      depth self (Arrow (arg, res))
    && ran (H.put (H.put (H.put h arg (cell Var depth)) res (cell Var depth))
      self (cell (Arrow (arg, res)) depth)) depth (Entry (self, Entry (res, Entry (arg, pool))))
      (Bind (arg, Bind (self, env))) body middle body_pool
    && final_pool === body_pool
    && (match result body with
      | None -> finish === Aborted && after === middle
      | Some b -> match finish with Aborted -> false | Unified (ok, d) ->
        Level_unifier_spec.unified middle b res ok after d)
  | RLet_left (rhs, _) -> ran h (depth + 1) Empty env rhs after final_pool
    && result rhs === None
  | RLet (rhs, body, middle, child_pool) ->
    ran h (depth + 1) Empty env rhs middle child_pool
    && (match result rhs with None -> false | Some p ->
      pool_scoped middle child_pool && pool_scoped middle pool
      && ran (closed_heap middle depth child_pool) depth
        (Nested_pool_spec.transfer (closed_heap middle depth child_pool) child_pool pool)
        (Bind (p, env)) body after final_pool))

type inference = #{value : node Pref.t option @@ aliased; state : node Pref.token;
  pool : pool @@ aliased; execution : execution @@ ghost}
