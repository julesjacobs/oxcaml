open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_runtime_spec
open Hm_runtime_proofs
open Hm_let_runtime_proofs
module D = Hm_declarative

let rec lookup_node : (h : Pref.heap) @ immutable ghost ->
    (env : env) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | env_owned h env && D.present (env_depth env) i} @ ghost ->
    {p : node Pref.t | Hm_environment_spec.lookup env i === Some p && H.mem h p} @ immutable =
  fun h env i premise ->
    ghost_ (let refine_ premise = premise in env_owned_def h env; env_depth_def env;
      let n = env_depth env in D.present_def n i; lookup_def env i);
    match env with
    | Empty -> ghost_ (let _impossible : {u : unit | false} = refine_ () in ()); assert false
    | Bind (p, rest) -> match i with D.Z -> refine_ p
      | D.S i -> let proof : {u : unit | env_owned h rest && D.present (env_depth rest) i} @ ghost = ghost_ (let u = () in refine_ u) in
        let refine_ p = lookup_node h rest i proof in refine_ p

let rec infer : (h : Pref.heap) @ immutable ghost -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ghost ->
    (env : env) @ immutable -> (e : D.term) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h && depth >= 0 && pool_scoped h pool
      && env_owned h env && D.scoped_term (env_depth env) e}) @ unique ->
    {r : inference | ran h depth pool env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free e) || let_free r.#execution) && source r.#execution === e && r.#value === result r.#execution} @ unique =
  fun h depth pool facts env e state ->
    let refine_ state = state in
    ghost_ (term_let_free_def e; let n = env_depth env in D.scoped_term_def n e; ());
    match e with
    | D.Bound i ->
      let proof : {u : unit | env_owned h env && D.present (env_depth env) i} @ ghost = ghost_ (let u = () in refine_ u) in
      let refine_ p = lookup_node h env i proof in
      let scope : ((x : node Pref.t) @ immutable ->
          {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total ghost = ghost_ (fun x ->
        facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u) in
      let p : {p : node Pref.t | H.mem h p} = refine_ p in
      let level : {n : int | n >= 0} = refine_ depth in
      let state : {t : Pref.token | Pref.own t === h && pool_scoped h pool} = refine_ state in
      let refine_ out = Clean_copy.instantiate h scope pool level p state in
      let refine_ level = level in let refine_ p = p in
      let execution = ghost_ (RVar (i, out.#value, out.#epoch, out.#history)) in
      let after = ghost_ (Pref.own (borrow_ out.#state)) in
      ghost_ (copy_heap_def h out.#epoch depth out.#history;
        ran_def h depth pool env execution after out.#pool;
        let_free_def execution; source_def execution; result_def execution);
      let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in refine_ r
    | D.Truth ->
      let desc : desc = Bool in ghost_ (children_below_def h desc depth);
      let state : {t : Pref.token | Pref.own t === h && pool_scoped h pool && depth >= 0 && children_below h desc depth} = refine_ state in
      let refine_ out = Pooled_allocator.allocate h depth desc pool state in
      let execution = ghost_ (RBool out.#value) in let after = ghost_ (Pref.own (borrow_ out.#state)) in
      ghost_ (allocated_def h depth out.#value desc; ran_def h depth pool env execution after out.#pool;
        let_free_def execution; source_def execution; result_def execution);
      let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in refine_ r
    | D.Lambda body ->
      let var : desc = Var in ghost_ (children_below_def h var depth);
      let state : {t : Pref.token | Pref.own t === h && pool_scoped h pool && depth >= 0 && children_below h var depth} = refine_ state in
      let refine_ allocated = Pooled_allocator.allocate h depth var pool state in
      let arg = allocated.#value in let pool1 = allocated.#pool in let state = allocated.#state in
      let h1 = ghost_ (Pref.own (borrow_ state)) in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in let refine_ u = allocate_runtime h depth pool arg var x (refine_ u) in refine_ u) in
      let next_env = Bind (arg, env) in
      ghost_ (let v = cell var depth in let u = () in allocation_env h arg v env (refine_ u);
        below_def h1 arg depth; env_owned_def h1 next_env; env_depth_def next_env);
      let state : {t : Pref.token | Pref.own t === h1 && depth >= 0 && pool_scoped h1 pool1
        && env_owned h1 next_env && D.scoped_term (env_depth next_env) body} = refine_ state in
      let refine_ inferred = infer h1 depth pool1 facts1 next_env body state in
      let middle = ghost_ (Pref.own (borrow_ inferred.#state)) in let body_pool = inferred.#pool in
      let body_run = ghost_ inferred.#execution in
      let state = inferred.#state in
      (match inferred.#value with
      | None ->
        let execution = ghost_ (RLam (arg, body_run, middle, body_pool, None)) in
        ghost_ (allocated_def h depth arg var; ran_def h depth pool env execution middle body_pool;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = None; state; pool = body_pool; execution} in refine_ r
      | Some b ->
        ghost_ (let u = () in run_pool_scoped h1 depth pool1 facts1 next_env body_run middle body_pool (refine_ u);
          run_result_below h1 depth pool1 facts1 next_env body_run middle body_pool b (refine_ u);
          fresh_active h depth arg var (refine_ u);
          run_active h1 depth pool1 facts1 next_env body_run middle body_pool arg (refine_ u);
          run_runtime h1 depth pool1 facts1 next_env body_run middle body_pool arg (refine_ u);
          runtime_at_def middle depth body_pool arg; depth_bound_def middle depth arg;
          active_def middle arg; finite_node_def middle arg; at_level_def middle arg);
        let desc = Arrow (arg, b) in ghost_ (children_below_def middle desc depth);
        let state : {t : Pref.token | Pref.own t === middle && pool_scoped middle body_pool
          && depth >= 0 && children_below middle desc depth} = refine_ state in
        let refine_ out = Pooled_allocator.allocate middle depth desc body_pool state in
        let after = ghost_ (Pref.own (borrow_ out.#state)) in
        let execution = ghost_ (RLam (arg, body_run, middle, body_pool, Some out.#value)) in
        ghost_ (allocated_def h depth arg var; allocated_def middle depth out.#value desc;
          ran_def h depth pool env execution after out.#pool;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in refine_ r)
    | D.Apply (left, right) ->
      let state : {t : Pref.token | Pref.own t === h && depth >= 0 && pool_scoped h pool
        && env_owned h env && D.scoped_term (env_depth env) left} = refine_ state in
      let refine_ first = infer h depth pool facts env left state in
      let h1 = ghost_ (Pref.own (borrow_ first.#state)) in let pool1 = first.#pool in
      let left_run = ghost_ first.#execution in let state = first.#state in
      (match first.#value with
      | None ->
        let execution = ghost_ (RApp_left (left_run, right)) in
        ghost_ (ran_def h depth pool env execution h1 pool1;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = None; state; pool = pool1; execution} in refine_ r
      | Some f ->
        let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
          let u = () in let refine_ u = run_runtime h depth pool facts env left_run h1 pool1 x (refine_ u) in refine_ u) in
        ghost_ (let u = () in run_pool_scoped h depth pool facts env left_run h1 pool1 (refine_ u);
          run_env_owned h depth pool env left_run h1 pool1 (refine_ u));
        let state : {t : Pref.token | Pref.own t === h1 && depth >= 0 && pool_scoped h1 pool1
          && env_owned h1 env && D.scoped_term (env_depth env) right} = refine_ state in
        let refine_ second = infer h1 depth pool1 facts1 env right state in
        let h2 = ghost_ (Pref.own (borrow_ second.#state)) in let pool2 = second.#pool in
        let right_run = ghost_ second.#execution in let state = second.#state in
        match second.#value with
        | None ->
          let execution = ghost_ (RApp_right (left_run, right_run, h1, pool1)) in
          ghost_ (ran_def h depth pool env execution h2 pool2;
            let_free_def execution; source_def execution; result_def execution);
          let r = #{value = None; state; pool = pool2; execution} in refine_ r
        | Some a ->
          let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 x}) @ total ghost = ghost_ (fun x ->
            let u = () in let refine_ u = run_runtime h1 depth pool1 facts1 env right_run h2 pool2 x (refine_ u) in refine_ u) in
          ghost_ (let u = () in run_pool_scoped h1 depth pool1 facts1 env right_run h2 pool2 (refine_ u);
            run_result_below h1 depth pool1 facts1 env right_run h2 pool2 a (refine_ u);
            run_result_active h depth pool facts env left_run h1 pool1 f (refine_ u);
            run_active h1 depth pool1 facts1 env right_run h2 pool2 f (refine_ u));
          let var : desc = Var in ghost_ (children_below_def h2 var depth);
          let state : {t : Pref.token | Pref.own t === h2 && pool_scoped h2 pool2 && depth >= 0 && children_below h2 var depth} = refine_ state in
          let refine_ reserved = Pooled_allocator.allocate h2 depth var pool2 state in
          let p = reserved.#value in let pool3 = reserved.#pool in let state = reserved.#state in
          let h3 = ghost_ (Pref.own (borrow_ state)) in
          let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 x}) @ total ghost = ghost_ (fun x ->
            facts2 x; let u = () in let refine_ u = allocate_runtime h2 depth pool2 p var x (refine_ u) in refine_ u) in
          ghost_ (let v = cell var depth in let u = () in
            Pooled_allocation_proofs.allocation_below h2 p v a depth (refine_ u);
            allocation_active h2 p v f (refine_ u); ());
          let desc = Arrow (a, p) in ghost_ (children_below_def h3 desc depth);
          let state : {t : Pref.token | Pref.own t === h3 && pool_scoped h3 pool3 && depth >= 0 && children_below h3 desc depth} = refine_ state in
          let refine_ expected = Pooled_allocator.allocate h3 depth desc pool3 state in
          let arrow = expected.#value in let pool4 = expected.#pool in let state = expected.#state in
          let h4 = ghost_ (Pref.own (borrow_ state)) in
          let facts4 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h4 depth pool4 x}) @ total ghost = ghost_ (fun x ->
            facts3 x; let u = () in let refine_ u = allocate_runtime h3 depth pool3 arrow desc x (refine_ u) in refine_ u) in
          let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h4 x) || finite_scope h4 x}) @ total ghost = ghost_ (fun x ->
            facts4 x; runtime_at_def h4 depth pool4 x;
            let u = () in let refine_ u = safe_finite_scope h4 x (refine_ u) in refine_ u) in
          let unmarked : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at h4 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            facts4 x; runtime_at_def h4 depth pool4 x; safe_def h4 x; let u = () in refine_ u) in
          ghost_ (let v = cell desc depth in let u = () in allocation_active h3 arrow v f (refine_ u);
            below_def h4 arrow depth; active_def h4 arrow; active_def h4 f);
          let state : {t : Pref.token | Pref.own t === h4 && H.mem h4 f && H.mem h4 arrow && active h4 f && active h4 arrow} = refine_ state in
          let refine_ solved = Level_unifier.unify h4 scope unmarked f arrow state in
          let after = ghost_ (Pref.own (borrow_ solved.#state)) in
          let execution = ghost_ (RApp (left_run, right_run, h1, pool1, h2, pool2, p, arrow, solved.#ok, solved.#derivation)) in
          ghost_ (allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
            ran_def h depth pool env execution after pool4; let_free_def execution;
            source_def execution; result_def execution);
          let value = if solved.#ok then Some p else None in
          let r = #{value; state = solved.#state; pool = pool4; execution} in refine_ r)
    | D.Recursive body ->
      let var : desc = Var in ghost_ (children_below_def h var depth);
      let state : {t : Pref.token | Pref.own t === h && pool_scoped h pool && depth >= 0 && children_below h var depth} = refine_ state in
      let refine_ first = Pooled_allocator.allocate h depth var pool state in
      let arg = first.#value in let pool1 = first.#pool in let state = first.#state in
      let h1 = ghost_ (Pref.own (borrow_ state)) in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in let refine_ u = allocate_runtime h depth pool arg var x (refine_ u) in refine_ u) in
      ghost_ (children_below_def h1 var depth);
      let state : {t : Pref.token | Pref.own t === h1 && pool_scoped h1 pool1 && depth >= 0 && children_below h1 var depth} = refine_ state in
      let refine_ second = Pooled_allocator.allocate h1 depth var pool1 state in
      let res = second.#value in let pool2 = second.#pool in let state = second.#state in
      let h2 = ghost_ (Pref.own (borrow_ state)) in
      let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 x}) @ total ghost = ghost_ (fun x ->
        facts1 x; let u = () in let refine_ u = allocate_runtime h1 depth pool1 res var x (refine_ u) in refine_ u) in
      ghost_ (let v = cell var depth in let u = () in Pooled_allocation_proofs.allocation_below h1 res v arg depth (refine_ u); ());
      let desc = Arrow (arg, res) in ghost_ (children_below_def h2 desc depth);
      let state : {t : Pref.token | Pref.own t === h2 && pool_scoped h2 pool2 && depth >= 0 && children_below h2 desc depth} = refine_ state in
      let refine_ third = Pooled_allocator.allocate h2 depth desc pool2 state in
      let self = third.#value in let pool3 = third.#pool in let state = third.#state in
      let h3 = ghost_ (Pref.own (borrow_ state)) in
      let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 x}) @ total ghost = ghost_ (fun x ->
        facts2 x; let u = () in let refine_ u = allocate_runtime h2 depth pool2 self desc x (refine_ u) in refine_ u) in
      let self_env = Bind (self, env) in let next_env = Bind (arg, self_env) in
      ghost_ (let v = cell var depth in let w = cell desc depth in let u = () in
        allocation_env h arg v env (refine_ u); allocation_env h1 res v env (refine_ u); allocation_env h2 self w env (refine_ u);
        Copy_heap_proofs.put_frame h1 res v arg; Copy_heap_proofs.put_frame h2 self w arg;
        below_def h1 arg depth; below_def h3 self depth;
        env_owned_def h3 self_env; env_owned_def h3 next_env;
        env_depth_def self_env; env_depth_def next_env);
      let state : {t : Pref.token | Pref.own t === h3 && depth >= 0 && pool_scoped h3 pool3
        && env_owned h3 next_env && D.scoped_term (env_depth next_env) body} = refine_ state in
      let refine_ inferred = infer h3 depth pool3 facts3 next_env body state in
      let middle = ghost_ (Pref.own (borrow_ inferred.#state)) in let body_pool = inferred.#pool in
      let body_run = ghost_ inferred.#execution in let state = inferred.#state in
      (match inferred.#value with
      | None ->
        let execution = ghost_ (RRec (arg, res, self, body_run, middle, body_pool, Aborted)) in
        ghost_ (allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
          ran_def h depth pool env execution middle body_pool; let_free_def execution;
          source_def execution; result_def execution);
        let r = #{value = None; state; pool = body_pool; execution} in refine_ r
      | Some b ->
        let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth body_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in let refine_ u = run_runtime h3 depth pool3 facts3 next_env body_run middle body_pool x (refine_ u) in refine_ u) in
        let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost = ghost_ (fun x ->
          middle_facts x; runtime_at_def middle depth body_pool x;
          let u = () in let refine_ u = safe_finite_scope middle x (refine_ u) in refine_ u) in
        let unmarked : ((x : node Pref.t) @ immutable ->
          {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
          middle_facts x; runtime_at_def middle depth body_pool x; safe_def middle x; let u = () in refine_ u) in
        ghost_ (let u = () in run_result_active h3 depth pool3 facts3 next_env body_run middle body_pool b (refine_ u);
          fresh_active h1 depth res var (refine_ u); let w = cell desc depth in
          allocation_active h2 self w res (refine_ u);
          run_active h3 depth pool3 facts3 next_env body_run middle body_pool res (refine_ u);
          active_def middle b; active_def middle res);
        let state : {t : Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle res && active middle b && active middle res} = refine_ state in
        let refine_ solved = Level_unifier.unify middle scope unmarked b res state in
        let after = ghost_ (Pref.own (borrow_ solved.#state)) in
        let execution = ghost_ (RRec (arg, res, self, body_run, middle, body_pool, Unified (solved.#ok, solved.#derivation))) in
        ghost_ (allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
          ran_def h depth pool env execution after body_pool; let_free_def execution;
          source_def execution; result_def execution);
        let value = if solved.#ok then Some self else None in
        let r = #{value; state = solved.#state; pool = body_pool; execution} in refine_ r)

    | D.Let (rhs, body) ->
      let child_depth = depth + 1 in
      if child_depth < 0 then assert false else (
      let child_pool : pool = Generalize_spec.Empty in
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth child_pool x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in enter_runtime h depth pool x (refine_ u); refine_ u) in
      ghost_ (pool_scoped_def h child_pool);
      let state : {t : Pref.token | Pref.own t === h && child_depth >= 0 && pool_scoped h child_pool
        && env_owned h env && D.scoped_term (env_depth env) rhs} = refine_ state in
      let refine_ first = infer h child_depth child_pool child_facts env rhs state in
      let middle = ghost_ (Pref.own (borrow_ first.#state)) in let rhs_pool = first.#pool in
      let rhs_run = ghost_ first.#execution in let state = first.#state in
      (match first.#value with
      | None ->
        let execution = ghost_ (RLet_left (rhs_run, body)) in
        ghost_ (ran_def h depth pool env execution middle rhs_pool;
          source_def execution; result_def execution; let_free_def execution);
        let r = #{value = None; state; pool = rhs_pool; execution} in refine_ r
      | Some p ->
        let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) rhs_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in run_runtime h child_depth child_pool child_facts env rhs_run middle rhs_pool x (refine_ u); refine_ u) in
        ghost_ (let u = () in run_pool_scoped h child_depth child_pool child_facts env rhs_run middle rhs_pool (refine_ u);
          run_saved_pool h child_depth child_pool child_facts env rhs_run middle rhs_pool pool (refine_ u);
          run_env_owned h child_depth child_pool env rhs_run middle rhs_pool (refine_ u);
          Hm_execution_proofs.run_result h child_depth child_pool env rhs_run middle rhs_pool p (refine_ u));
        let state : {t : Pref.token | Pref.own t === middle && pool_scoped middle rhs_pool && pool_scoped middle pool} = refine_ state in
        let refine_ closed = Nested_pool.close middle depth rhs_pool pool state in
        let start = ghost_ (Pref.own (borrow_ closed.#state)) in let parent_pool = closed.#parent in let state = closed.#state in
        let parent_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at start depth parent_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in close_runtime h depth pool facts env rhs_run middle rhs_pool middle_facts x (refine_ u); refine_ u) in
        let next_env = Bind (p, env) in
        ghost_ (let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || H.mem start x}) @ total = fun x ->
            let u = () in Generalize_proofs.closed_observe middle depth rhs_pool x (refine_ u);
            closed_at_def middle start depth rhs_pool x; refine_ u in
          let u = () in env_extend middle start frame env (refine_ u); frame p;
          env_owned_def start next_env; env_depth_def next_env);
        let state : {t : Pref.token | Pref.own t === start && depth >= 0 && pool_scoped start parent_pool
          && env_owned start next_env && D.scoped_term (env_depth next_env) body} = refine_ state in
        let refine_ out = infer start depth parent_pool parent_facts next_env body state in
        let after = ghost_ (Pref.own (borrow_ out.#state)) in
        let execution = ghost_ (RLet (rhs_run, out.#execution, middle, rhs_pool)) in
        ghost_ (ran_def h depth pool env execution after out.#pool;
          source_def execution; result_def execution; let_free_def execution);
        let r = #{value = out.#value; state = out.#state; pool = out.#pool; execution} in refine_ r)

)

let closed_hm : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : inference | let refine_ e = e in
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free e) || let_free r.#execution) && source r.#execution === e && r.#value === result r.#execution} @ unique = fun e ->
  let refine_ e = e in let refine_ state = Pref.empty () in let h = ghost_ (Pref.own (borrow_ state)) in
  let pool : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
  let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 pool x}) @ total ghost = ghost_ (fun x ->
    runtime_at_def h 0 pool x; safe_def h x; depth_bound_def h 0 x; covered_def h (-1) pool x;
    ordered_def h x; let u = () in refine_ u) in
  ghost_ (pool_scoped_def h pool; env_owned_def h env; env_depth_def env);
  let state : {t : Pref.token | Pref.own t === h && 0 >= 0 && pool_scoped h pool && env_owned h env
    && D.scoped_term (env_depth env) e} = refine_ state in
  let refine_ out = infer h 0 pool facts env e state in refine_ out

let closed : (e : {e : D.term | D.scoped_term D.Z e && term_let_free e}) @ immutable ->
    {r : inference | let refine_ e = e in
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty r.#execution (Pref.own r.#state) r.#pool
      && let_free r.#execution && source r.#execution === e && r.#value === result r.#execution} @ unique = fun e ->
    let refine_ e = e in let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
    let refine_ out = closed_hm input in let refine_ input = input in refine_ out
