open Level_unifier_spec
open Level_finite_spec
open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_runtime_spec
open Hm_runtime_proofs
open Hm_let_runtime_proofs
module D = Hm_declarative
module F = Fast_environment
module T = Fast_term

external[@layout_poly] raise_any : ('a : any).
  exn -> 'a @ portable unique = "%raise"

type infer_goal = { heap : Pref.heap @@ ghost; depth : int @@ ghost;
  pool : pool @@ ghost; env : env @@ ghost; term : D.term @@ ghost }

let rec infer_work : (goal : infer_goal) @ immutable -> (h : (Pref.heap) Ghost.t) @ immutable   -> (depth : int)  -> (pool : pool) @ immutable -> (facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost depth pool x})) Ghost.t) @ total   -> (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total   ->
    (env : F.forest) @ immutable -> (e : T.term) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool
      && F.valid_forest env && T.valid e && env_owned h.Ghost.ghost (F.flatten env) && D.scoped_term (env_depth (F.flatten env)) (T.source e)}) @ unique  ->
    (use : ((r : {r : inference | ran h.Ghost.ghost depth pool (F.flatten env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source e)) || let_free r.#execution) && source r.#execution === T.source e && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique)) ->
    {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun goal h depth pool facts trees env e state use ->
  let runtime_env = env in let runtime_term = e in
  let env = ghost_ (F.flatten runtime_env) in let e = ghost_ (T.source runtime_term) in

  let facts = ghost_ facts.Ghost.ghost in
  let trees = ghost_ trees.Ghost.ghost in
    ghost_ (term_let_free_def e; let n = env_depth env in D.scoped_term_def n e; ());
    ghost_ (T.valid_def runtime_term; T.source_def runtime_term);
    match runtime_term with
    | T.Bound index ->
      let i = ghost_ index.T.original in
      ghost_ (let u = () in env_lookup_owned h.Ghost.ghost env i (u);
        F.lookup_encoded env i index.T.number; ());
      let premise : ({u : unit | F.valid_forest runtime_env && index.T.number >= 0}) Ghost.t =
        {Ghost.ghost = ghost_ (())} in
      let found = F.lookup runtime_env index.T.number premise in
      let p : {p : node Pref.t | Hm_environment_spec.lookup env i === Some p && H.mem h.Ghost.ghost p} =
        match found with Some p -> p | None ->
          ghost_ (let _impossible : {u : unit | false} = () in ()); assert false in
      let scope : ((x : node Pref.t) @ immutable ->
          {u : unit | if H.mem h.Ghost.ghost x then source_ok h.Ghost.ghost x else H.at h.Ghost.ghost x === None}) @ total ghost = ghost_ (fun x ->
        facts x; runtime_at_def h.Ghost.ghost depth pool x; safe_def h.Ghost.ghost x; let u = () in u) in
      let p : {p : node Pref.t | H.mem h.Ghost.ghost p} = p in
      let level : {n : int | n >= 0} = depth in
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool} = state in
      let current : {v : node | H.at h.Ghost.ghost p === Some v} =
        let b = borrow_ state in let b : {t : Pref.token | H.mem (Pref.own t) p} = b in
        let v = Pref.read p b in v in
      (match current.level with
      | Finite _ ->
        ghost_ (facts p; runtime_at_def h.Ghost.ghost depth pool p; safe_def h.Ghost.ghost p;
          ordered_def h.Ghost.ghost p; active_def h.Ghost.ghost p; at_level_def h.Ghost.ghost p);
        let execution = ghost_ (RShared (i, p)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution h.Ghost.ghost pool; source_def execution;
          result_def execution; let_free_def execution);
        let out = #{value = Some p; state; pool; execution} in use (out)
      | Generic ->
      let p : {p : node Pref.t | H.mem h.Ghost.ghost p} = p in
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool} = state in
      let clean : ((x : node Pref.t) @ immutable ->
        {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo}) @ total ghost = ghost_ (fun x ->
        facts x; runtime_at_def h.Ghost.ghost depth pool x; safe_def h.Ghost.ghost x; let u = () in u) in
      let saved_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let clean_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness1.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (clean)} in
      let scope_witness3 : (((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved_witness1.Ghost.ghost p then source_ok saved_witness1.Ghost.ghost p
        else H.at saved_witness1.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
      let copy_source1 : {p : node Pref.t | H.mem saved_witness1.Ghost.ghost p} =
        p in
      let out = Clean_copy.instantiate saved_witness1 clean_witness2 scope_witness3 pool level copy_source1 (state) in
      let execution = ghost_ (RVar (i, out.#value, out.#epoch, out.#history)) in
      let after = ghost_ (Pref.own (borrow_ out.#state)) in
      ghost_ (copy_heap_def h.Ghost.ghost out.#epoch depth out.#history;
        ran_def h.Ghost.ghost depth pool env execution after out.#pool;
        let_free_def execution; source_def execution; result_def execution);
      let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in use (r))
    | T.Truth ->
      let desc : desc = Bool in ghost_ (children_below_def h.Ghost.ghost desc depth);
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool && depth >= 0 && children_below h.Ghost.ghost desc depth} = state in
      let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let out = Pooled_allocator.allocate allocation_heap depth desc pool (state) in
      let execution = ghost_ (RBool out.#value) in let after = ghost_ (Pref.own (borrow_ out.#state)) in
      ghost_ (allocated_def h.Ghost.ghost depth out.#value desc; ran_def h.Ghost.ghost depth pool env execution after out.#pool;
        let_free_def execution; source_def execution; result_def execution);
      let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in use (r)
    | T.Lambda runtime_body ->
      let body = ghost_ (T.source runtime_body) in
      let var : desc = Var in ghost_ (children_below_def h.Ghost.ghost var depth);
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool && depth >= 0 && children_below h.Ghost.ghost var depth} = state in
      let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let allocated = Pooled_allocator.allocate allocation_heap depth var pool (state) in
      let arg = allocated.#value in let pool1 = allocated.#pool in let state = allocated.#state in
      let h1 = ghost_ (Pref.own (borrow_ state)) in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in let u = allocate_runtime h.Ghost.ghost depth pool arg var x (u) in u) in
      let next_env = ghost_ (Bind (arg, env)) in
      let input : {f : F.forest | F.valid_forest f} = runtime_env in
      let next_runtime_env = F.cons arg input in ghost_ (let v = cell var depth in let u = () in allocation_env h.Ghost.ghost arg v env (u);
        below_def h1 arg depth; env_owned_def h1 next_env; env_depth_def next_env);
      let state : {t : Pref.token | Pref.own t === h1 && depth >= 0 && pool_scoped h1 pool1
        && env_owned h1 next_env && D.scoped_term (env_depth next_env) body} = state in
      let trees1 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h.Ghost.ghost depth arg var; let u = () in
      let next = Hm_forest_proofs.allocated_forest h.Ghost.ghost trees depth arg var (u) in
      let t = next x in t) in
      let h1_witness : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h1)} in
      let facts1_witness : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1_witness.Ghost.ghost depth pool1 x})) Ghost.t = {Ghost.ghost = ghost_ (facts1)} in
      let trees1_witness : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h1_witness.Ghost.ghost x then finite h1_witness.Ghost.ghost t else observe h1_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees1)} in
      let resume_inferred : (inferred : {r : inference | ran h1_witness.Ghost.ghost depth pool1 (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_body)) || let_free r.#execution) && source r.#execution === (T.source runtime_body) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun inferred ->
        let middle = ghost_ (Pref.own (borrow_ inferred.#state)) in let body_pool = inferred.#pool in
      let body_run = ghost_ inferred.#execution in
      let state = inferred.#state in
      (match inferred.#value with
      | None ->
        let execution = ghost_ (RLam (arg, body_run, middle, body_pool, None)) in
        ghost_ (allocated_def h.Ghost.ghost depth arg var; ran_def h.Ghost.ghost depth pool env execution middle body_pool;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = None; state; pool = body_pool; execution} in use (r)
      | Some b ->
        ghost_ (let u = () in run_pool_scoped h1 depth pool1 facts1 next_env body_run middle body_pool (u);
          run_result_below h1 depth pool1 facts1 next_env body_run middle body_pool b (u);
          fresh_active h.Ghost.ghost depth arg var (u);
          run_active h1 depth pool1 facts1 next_env body_run middle body_pool arg (u);
          run_runtime h1 depth pool1 facts1 next_env body_run middle body_pool arg (u);
          runtime_at_def middle depth body_pool arg; depth_bound_def middle depth arg;
          active_def middle arg; finite_node_def middle arg; at_level_def middle arg);
        let desc = Arrow (arg, b) in ghost_ (children_below_def middle desc depth);
        let state : {t : Pref.token | Pref.own t === middle && pool_scoped middle body_pool
          && depth >= 0 && children_below middle desc depth} = state in
        let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let out = Pooled_allocator.allocate allocation_heap depth desc body_pool (state) in
        let after = ghost_ (Pref.own (borrow_ out.#state)) in
        let execution = ghost_ (RLam (arg, body_run, middle, body_pool, Some out.#value)) in
        ghost_ (allocated_def h.Ghost.ghost depth arg var; allocated_def middle depth out.#value desc;
          ran_def h.Ghost.ghost depth pool env execution after out.#pool;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = Some out.#value; state = out.#state; pool = out.#pool; execution} in use (r)) in
      infer_work goal h1_witness depth pool1 facts1_witness trees1_witness next_runtime_env runtime_body (state) resume_inferred
    | T.Apply (runtime_left, runtime_right) ->
      let left = ghost_ (T.source runtime_left) in let right = ghost_ (T.source runtime_right) in
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool
        && env_owned h.Ghost.ghost env && D.scoped_term (env_depth env) left} = state in
      let h_witness3 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let facts_witness : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h_witness3.Ghost.ghost depth pool x})) Ghost.t = {Ghost.ghost = ghost_ (facts)} in
      let trees_witness : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness3.Ghost.ghost x then finite h_witness3.Ghost.ghost t else observe h_witness3.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees)} in
      let resume_first : (first : {r : inference | ran h_witness3.Ghost.ghost depth pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_left)) || let_free r.#execution) && source r.#execution === (T.source runtime_left) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun first ->
        let h1 = ghost_ (Pref.own (borrow_ first.#state)) in let pool1 = first.#pool in
      let left_run = ghost_ first.#execution in let state = first.#state in
      (match first.#value with
      | None ->
        let execution = ghost_ (RApp_left (left_run, right)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution h1 pool1;
          let_free_def execution; source_def execution; result_def execution);
        let r = #{value = None; state; pool = pool1; execution} in use (r)
      | Some f ->
        let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
          let u = () in let u = run_runtime h.Ghost.ghost depth pool facts env left_run h1 pool1 x (u) in u) in
        ghost_ (let u = () in run_pool_scoped h.Ghost.ghost depth pool facts env left_run h1 pool1 (u);
          run_env_owned h.Ghost.ghost depth pool env left_run h1 pool1 (u));
        let state : {t : Pref.token | Pref.own t === h1 && depth >= 0 && pool_scoped h1 pool1
          && env_owned h1 env && D.scoped_term (env_depth env) right} = state in
        let trees1 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let u = () in let t = Hm_forest_proofs.run_forest h.Ghost.ghost trees depth pool env left_run h1 pool1 x (u) in t) in
      let h1_witness2 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h1)} in
      let facts1_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1_witness2.Ghost.ghost depth pool1 x})) Ghost.t = {Ghost.ghost = ghost_ (facts1)} in
      let trees1_witness2 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h1_witness2.Ghost.ghost x then finite h1_witness2.Ghost.ghost t else observe h1_witness2.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees1)} in
      let resume_second : (second : {r : inference | ran h1_witness2.Ghost.ghost depth pool1 (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_right)) || let_free r.#execution) && source r.#execution === (T.source runtime_right) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun second ->
        let h2 = ghost_ (Pref.own (borrow_ second.#state)) in let pool2 = second.#pool in
        let right_run = ghost_ second.#execution in let state = second.#state in
        match second.#value with
        | None ->
          let execution = ghost_ (RApp_right (left_run, right_run, h1, pool1)) in
          ghost_ (ran_def h.Ghost.ghost depth pool env execution h2 pool2;
            let_free_def execution; source_def execution; result_def execution);
          let r = #{value = None; state; pool = pool2; execution} in use (r)
        | Some a ->
          let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 x}) @ total ghost = ghost_ (fun x ->
            let u = () in let u = run_runtime h1 depth pool1 facts1 env right_run h2 pool2 x (u) in u) in
          ghost_ (let u = () in run_pool_scoped h1 depth pool1 facts1 env right_run h2 pool2 (u);
            run_result_below h1 depth pool1 facts1 env right_run h2 pool2 a (u);
            run_result_active h.Ghost.ghost depth pool facts env left_run h1 pool1 f (u);
            run_active h1 depth pool1 facts1 env right_run h2 pool2 f (u));
          let var : desc = Var in ghost_ (children_below_def h2 var depth);
          let state : {t : Pref.token | Pref.own t === h2 && pool_scoped h2 pool2 && depth >= 0 && children_below h2 var depth} = state in
          let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h2)} in
          let reserved = Pooled_allocator.allocate allocation_heap depth var pool2 (state) in
          let p = reserved.#value in let pool3 = reserved.#pool in let state = reserved.#state in
          let h3 = ghost_ (Pref.own (borrow_ state)) in
          let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 x}) @ total ghost = ghost_ (fun x ->
            facts2 x; let u = () in let u = allocate_runtime h2 depth pool2 p var x (u) in u) in
          ghost_ (let v = cell var depth in let u = () in
            Pooled_allocation_proofs.allocation_below h2 p v a depth (u);
            allocation_active h2 p v f (u); ());
          let desc = Arrow (a, p) in ghost_ (children_below_def h3 desc depth);
          let state : {t : Pref.token | Pref.own t === h3 && pool_scoped h3 pool3 && depth >= 0 && children_below h3 desc depth} = state in
          let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h3)} in
          let expected = Pooled_allocator.allocate allocation_heap depth desc pool3 (state) in
          let arrow = expected.#value in let pool4 = expected.#pool in let state = expected.#state in
          let h4 = ghost_ (Pref.own (borrow_ state)) in
          let facts4 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h4 depth pool4 x}) @ total ghost = ghost_ (fun x ->
            facts3 x; let u = () in let u = allocate_runtime h3 depth pool3 arrow desc x (u) in u) in
          let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h4 x) || finite_scope h4 x}) @ total ghost = ghost_ (fun x ->
            facts4 x; runtime_at_def h4 depth pool4 x;
            let u = () in let u = safe_finite_scope h4 x (u) in u) in
          let unmarked : ((x : node Pref.t) @ immutable ->
            {u : unit | match H.at h4 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
            facts4 x; runtime_at_def h4 depth pool4 x; safe_def h4 x; let u = () in u) in
          ghost_ (let v = cell desc depth in let u = () in allocation_active h3 arrow v f (u);
            below_def h4 arrow depth; active_def h4 arrow; active_def h4 f);
          let state : {t : Pref.token | Pref.own t === h4 && H.mem h4 f && H.mem h4 arrow && active h4 f && active h4 arrow} = state in
          let trees2 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let u = () in let t = Hm_forest_proofs.run_forest h1 trees1 depth pool1 env right_run h2 pool2 x (u) in t) in
      let trees3 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h2 depth p var; let u = () in
      let next = Hm_forest_proofs.allocated_forest h2 trees2 depth p var (u) in
      let t = next x in t) in
      let trees4 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h4 x then finite h4 t else observe h4 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h3 depth arrow desc; let u = () in
      let next = Hm_forest_proofs.allocated_forest h3 trees3 depth arrow desc (u) in
      let t = next x in t) in
      let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h4 x}) @ total ghost = ghost_ (fun x ->
        facts4 x; runtime_at_def h4 depth pool4 x; safe_def h4 x; let u = () in u) in
      let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h4)} in
      let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
      let unmarked_witness3 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness1.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked)} in
      let order_witness4 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (order)} in
      let trees_witness5 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness1.Ghost.ghost x then finite h_witness1.Ghost.ghost t else observe h_witness1.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees4)} in
      let state_argument6 = state in
      let solved = Optimized_unifier.unify h_witness1 scope_witness2 unmarked_witness3 order_witness4 trees_witness5 f arrow (state_argument6) in
          let after = ghost_ (Pref.own (borrow_ solved.#state)) in
          let execution = ghost_ (RApp (left_run, right_run, h1, pool1, h2, pool2, p, arrow, solved.#ok, solved.#derivation)) in
          ghost_ (allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
            ran_def h.Ghost.ghost depth pool env execution after pool4; let_free_def execution;
            source_def execution; result_def execution);
          let value = if solved.#ok then Some p else None in
          let r = #{value; state = solved.#state; pool = pool4; execution} in use (r) in
      infer_work goal h1_witness2 depth pool1 facts1_witness2 trees1_witness2 runtime_env runtime_right (state) resume_second) in
      infer_work goal h_witness3 depth pool facts_witness trees_witness runtime_env runtime_left (state) resume_first
    | T.Recursive runtime_body ->
      let body = ghost_ (T.source runtime_body) in
      let var : desc = Var in ghost_ (children_below_def h.Ghost.ghost var depth);
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && pool_scoped h.Ghost.ghost pool && depth >= 0 && children_below h.Ghost.ghost var depth} = state in
      let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let first = Pooled_allocator.allocate allocation_heap depth var pool (state) in
      let arg = first.#value in let pool1 = first.#pool in let state = first.#state in
      let h1 = ghost_ (Pref.own (borrow_ state)) in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in let u = allocate_runtime h.Ghost.ghost depth pool arg var x (u) in u) in
      ghost_ (children_below_def h1 var depth);
      let state : {t : Pref.token | Pref.own t === h1 && pool_scoped h1 pool1 && depth >= 0 && children_below h1 var depth} = state in
      let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h1)} in
      let second = Pooled_allocator.allocate allocation_heap depth var pool1 (state) in
      let res = second.#value in let pool2 = second.#pool in let state = second.#state in
      let h2 = ghost_ (Pref.own (borrow_ state)) in
      let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 x}) @ total ghost = ghost_ (fun x ->
        facts1 x; let u = () in let u = allocate_runtime h1 depth pool1 res var x (u) in u) in
      ghost_ (let v = cell var depth in let u = () in Pooled_allocation_proofs.allocation_below h1 res v arg depth (u); ());
      let desc = Arrow (arg, res) in ghost_ (children_below_def h2 desc depth);
      let state : {t : Pref.token | Pref.own t === h2 && pool_scoped h2 pool2 && depth >= 0 && children_below h2 desc depth} = state in
      let allocation_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h2)} in
      let third = Pooled_allocator.allocate allocation_heap depth desc pool2 (state) in
      let self = third.#value in let pool3 = third.#pool in let state = third.#state in
      let h3 = ghost_ (Pref.own (borrow_ state)) in
      let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 x}) @ total ghost = ghost_ (fun x ->
        facts2 x; let u = () in let u = allocate_runtime h2 depth pool2 self desc x (u) in u) in
      let self_env = ghost_ (Bind (self, env)) in let next_env = ghost_ (Bind (arg, self_env)) in
      let input : {f : F.forest | F.valid_forest f} = runtime_env in
      let self_runtime_env = F.cons self input in let input : {f : F.forest | F.valid_forest f} = self_runtime_env in
      let next_runtime_env = F.cons arg input in ghost_ (let v = cell var depth in let w = cell desc depth in let u = () in
        allocation_env h.Ghost.ghost arg v env (u); allocation_env h1 res v env (u); allocation_env h2 self w env (u);
        Copy_heap_proofs.put_frame h1 res v arg; Copy_heap_proofs.put_frame h2 self w arg;
        below_def h1 arg depth; below_def h3 self depth;
        env_owned_def h3 self_env; env_owned_def h3 next_env;
        env_depth_def self_env; env_depth_def next_env);
      let state : {t : Pref.token | Pref.own t === h3 && depth >= 0 && pool_scoped h3 pool3
        && env_owned h3 next_env && D.scoped_term (env_depth next_env) body} = state in
      let trees1 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h.Ghost.ghost depth arg var; let u = () in
      let next = Hm_forest_proofs.allocated_forest h.Ghost.ghost trees depth arg var (u) in
      let t = next x in t) in
      let trees2 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h1 depth res var; let u = () in
      let next = Hm_forest_proofs.allocated_forest h1 trees1 depth res var (u) in
      let t = next x in t) in
      let trees3 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      allocated_def h2 depth self desc; let u = () in
      let next = Hm_forest_proofs.allocated_forest h2 trees2 depth self desc (u) in
      let t = next x in t) in
      let h3_witness : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h3)} in
      let facts3_witness : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3_witness.Ghost.ghost depth pool3 x})) Ghost.t = {Ghost.ghost = ghost_ (facts3)} in
      let trees3_witness : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h3_witness.Ghost.ghost x then finite h3_witness.Ghost.ghost t else observe h3_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees3)} in
      let resume_inferred : (inferred : {r : inference | ran h3_witness.Ghost.ghost depth pool3 (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_body)) || let_free r.#execution) && source r.#execution === (T.source runtime_body) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun inferred ->
        let middle = ghost_ (Pref.own (borrow_ inferred.#state)) in let body_pool = inferred.#pool in
      let body_run = ghost_ inferred.#execution in let state = inferred.#state in
      (match inferred.#value with
      | None ->
        let execution = ghost_ (RRec (arg, res, self, body_run, middle, body_pool, Aborted)) in
        ghost_ (allocated_def h.Ghost.ghost depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
          ran_def h.Ghost.ghost depth pool env execution middle body_pool; let_free_def execution;
          source_def execution; result_def execution);
        let r = #{value = None; state; pool = body_pool; execution} in use (r)
      | Some b ->
        let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth body_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in let u = run_runtime h3 depth pool3 facts3 next_env body_run middle body_pool x (u) in u) in
        let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost = ghost_ (fun x ->
          middle_facts x; runtime_at_def middle depth body_pool x;
          let u = () in let u = safe_finite_scope middle x (u) in u) in
        let unmarked : ((x : node Pref.t) @ immutable ->
          {u : unit | match H.at middle x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
          middle_facts x; runtime_at_def middle depth body_pool x; safe_def middle x; let u = () in u) in
        ghost_ (let u = () in run_result_active h3 depth pool3 facts3 next_env body_run middle body_pool b (u);
          fresh_active h1 depth res var (u); let w = cell desc depth in
          allocation_active h2 self w res (u);
          run_active h3 depth pool3 facts3 next_env body_run middle body_pool res (u);
          active_def middle b; active_def middle res);
        let state : {t : Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle res && active middle b && active middle res} = state in
        let middle_trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let u = () in let t = Hm_forest_proofs.run_forest h3 trees3 depth pool3 next_env body_run middle body_pool x (u) in t) in
      let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered middle x}) @ total ghost = ghost_ (fun x ->
        middle_facts x; runtime_at_def middle depth body_pool x; safe_def middle x; let u = () in u) in
      let h_witness7 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
      let scope_witness8 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness7.Ghost.ghost x) || finite_scope h_witness7.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
      let unmarked_witness9 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness7.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked)} in
      let order_witness10 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness7.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (order)} in
      let trees_witness11 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness7.Ghost.ghost x then finite h_witness7.Ghost.ghost t else observe h_witness7.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (middle_trees)} in
      let state_argument12 = state in
      let solved = Optimized_unifier.unify h_witness7 scope_witness8 unmarked_witness9 order_witness10 trees_witness11 b res (state_argument12) in
        let after = ghost_ (Pref.own (borrow_ solved.#state)) in
        let execution = ghost_ (RRec (arg, res, self, body_run, middle, body_pool, Unified (solved.#ok, solved.#derivation))) in
        ghost_ (allocated_def h.Ghost.ghost depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
          ran_def h.Ghost.ghost depth pool env execution after body_pool; let_free_def execution;
          source_def execution; result_def execution);
        let value = if solved.#ok then Some self else None in
        let r = #{value; state = solved.#state; pool = body_pool; execution} in use (r))
 in
      infer_work goal h3_witness depth pool3 facts3_witness trees3_witness next_runtime_env runtime_body (state) resume_inferred
    | T.Let (runtime_rhs, runtime_body) ->
      let rhs = ghost_ (T.source runtime_rhs) in let body = ghost_ (T.source runtime_body) in
      let child_depth = depth + 1 in
      if child_depth < 0 then
        raise_any (Failure "type inference level capacity") else (
      let child_pool : pool = Generalize_spec.Empty in
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost child_depth child_pool x}) @ total ghost = ghost_ (fun x ->
        facts x; let u = () in enter_runtime h.Ghost.ghost depth pool x (u); u) in
      ghost_ (pool_scoped_def h.Ghost.ghost child_pool);
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && child_depth >= 0 && pool_scoped h.Ghost.ghost child_pool
        && env_owned h.Ghost.ghost env && D.scoped_term (env_depth env) rhs} = state in
      let h_witness4 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let child_facts_witness : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h_witness4.Ghost.ghost child_depth child_pool x})) Ghost.t = {Ghost.ghost = ghost_ (child_facts)} in
      let trees_witness2 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness4.Ghost.ghost x then finite h_witness4.Ghost.ghost t else observe h_witness4.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees)} in
      let resume_first : (first : {r : inference | ran h_witness4.Ghost.ghost child_depth child_pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_rhs)) || let_free r.#execution) && source r.#execution === (T.source runtime_rhs) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun first ->
        let middle = ghost_ (Pref.own (borrow_ first.#state)) in let rhs_pool = first.#pool in
      let rhs_run = ghost_ first.#execution in let state = first.#state in
      (match first.#value with
      | None ->
        let execution = ghost_ (RLet_left (rhs_run, body)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution middle rhs_pool;
          source_def execution; result_def execution; let_free_def execution);
        let r = #{value = None; state; pool = rhs_pool; execution} in use (r)
      | Some p ->
        let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) rhs_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in run_runtime h.Ghost.ghost child_depth child_pool child_facts env rhs_run middle rhs_pool x (u); u) in
        ghost_ (let u = () in run_pool_scoped h.Ghost.ghost child_depth child_pool child_facts env rhs_run middle rhs_pool (u);
          run_saved_pool h.Ghost.ghost child_depth child_pool child_facts env rhs_run middle rhs_pool pool (u);
          run_env_owned h.Ghost.ghost child_depth child_pool env rhs_run middle rhs_pool (u);
          Hm_execution_proofs.run_result h.Ghost.ghost child_depth child_pool env rhs_run middle rhs_pool p (u));
        let state : {t : Pref.token | Pref.own t === middle && pool_scoped middle rhs_pool && pool_scoped middle pool} = state in
        let close_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let closed = Nested_pool.close close_heap depth rhs_pool pool (state) in
        let start = ghost_ (Pref.own (borrow_ closed.#state)) in let parent_pool = closed.#parent in let state = closed.#state in
        let parent_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at start depth parent_pool x}) @ total ghost = ghost_ (fun x ->
          let u = () in close_runtime h.Ghost.ghost depth pool facts env rhs_run middle rhs_pool middle_facts x (u); u) in
        let next_env = ghost_ (Bind (p, env)) in
        let input : {f : F.forest | F.valid_forest f} = runtime_env in
        let next_runtime_env = F.cons p input in ghost_ (let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || H.mem start x}) @ total = fun x ->
            let u = () in Generalize_proofs.closed_observe middle depth rhs_pool x (u);
            closed_at_def middle start depth rhs_pool x; u in
          let u = () in env_extend middle start frame env (u); frame p;
          env_owned_def start next_env; env_depth_def next_env);
        let state : {t : Pref.token | Pref.own t === start && depth >= 0 && pool_scoped start parent_pool
          && env_owned start next_env && D.scoped_term (env_depth next_env) body} = state in
        let middle_trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let u = () in let t = Hm_forest_proofs.run_forest h.Ghost.ghost trees child_depth child_pool env rhs_run middle rhs_pool x (u) in t) in
      let closed_trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem start x then finite start t else observe start x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let u = () in let t = Forest_transport.closed_forest_at middle middle_trees depth rhs_pool x (u) in t) in
      let start_witness : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (start)} in
      let parent_facts_witness : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at start_witness.Ghost.ghost depth parent_pool x})) Ghost.t = {Ghost.ghost = ghost_ (parent_facts)} in
      let closed_trees_witness : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem start_witness.Ghost.ghost x then finite start_witness.Ghost.ghost t else observe start_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (closed_trees)} in
      let resume_out : (out : {r : inference | ran start_witness.Ghost.ghost depth parent_pool (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_body)) || let_free r.#execution) && source r.#execution === (T.source runtime_body) && r.#value === result r.#execution}) @ unique ->
        {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun out ->
        let after = ghost_ (Pref.own (borrow_ out.#state)) in
        let execution = ghost_ (RLet (rhs_run, out.#execution, middle, rhs_pool)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution after out.#pool;
          source_def execution; result_def execution; let_free_def execution);
        let r = #{value = out.#value; state = out.#state; pool = out.#pool; execution} in use (r) in
      infer_work goal start_witness depth parent_pool parent_facts_witness closed_trees_witness next_runtime_env runtime_body (state) resume_out) in
      infer_work goal h_witness4 child_depth child_pool child_facts_witness trees_witness2 runtime_env runtime_rhs (state) resume_first)
let infer_compiled : (h : (Pref.heap) Ghost.t) @ immutable   -> (depth : int)  -> (pool : pool) @ immutable -> (facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost depth pool x})) Ghost.t) @ total   -> (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total   ->
    (env : F.forest) @ immutable -> (e : T.term) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool
      && F.valid_forest env && T.valid e && env_owned h.Ghost.ghost (F.flatten env) && D.scoped_term (env_depth (F.flatten env)) (T.source e)}) @ unique  ->
    {r : inference | ran h.Ghost.ghost depth pool (F.flatten env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source e)) || let_free r.#execution) && source r.#execution === T.source e && r.#value === result r.#execution} @ unique  = fun h depth pool facts trees env e state ->
    let goal = {heap = h.Ghost.ghost; depth; pool; env = F.flatten env; term = T.source e} in
    let use : (r : {r : inference | ran h.Ghost.ghost depth pool (F.flatten env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source e)) || let_free r.#execution) && source r.#execution === T.source e && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (goal.term)) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun r ->
      r in
    let out = infer_work goal h depth pool facts trees env e state use in out

let infer : (h : (Pref.heap) Ghost.t) @ immutable   -> (depth : int)  -> (pool : pool) @ immutable -> (facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost depth pool x})) Ghost.t) @ total   -> (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total   ->
    (env : env) @ immutable -> (e : D.term) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool
      && env_owned h.Ghost.ghost env && D.scoped_term (env_depth env) e}) @ unique  ->
    {r : inference | ran h.Ghost.ghost depth pool env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free e) || let_free r.#execution) && source r.#execution === e && r.#value === result r.#execution} @ unique  = fun h depth pool facts trees env e state ->
    let goal = {heap = h.Ghost.ghost; depth; pool; env; term = e} in
    let runtime_env = F.compile env in
    let runtime_term = T.compile e in
    let use : (r : {r : inference | ran h.Ghost.ghost depth pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source runtime_term)) || let_free r.#execution) && source r.#execution === T.source runtime_term && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free goal.term) || let_free r.#execution) && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun r ->
      r in
    let out = infer_work goal h depth pool facts trees runtime_env runtime_term (state) use in out

let closed_compiled : (input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)}) @ immutable ->
    {r : inference | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free (T.source input)) || let_free r.#execution) && source r.#execution === T.source input && r.#value === result r.#execution} @ unique = fun input ->
  let e = ghost_ (T.source input) in let state = Pref.empty () in let h = ghost_ (Pref.own (borrow_ state)) in
  let pool : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
  let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 pool x}) @ total ghost = ghost_ (fun x ->
    runtime_at_def h 0 pool x; safe_def h x; depth_bound_def h 0 x; covered_def h (-1) pool x;
    ordered_def h x; let u = () in u) in
  ghost_ (pool_scoped_def h pool; env_owned_def h env; env_depth_def env);
  let state : {t : Pref.token | Pref.own t === h && 0 >= 0 && pool_scoped h pool && env_owned h env
    && D.scoped_term (env_depth env) e} = state in
  let trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let t = Free x in tree_root_def t; observe_def h x; t) in
      let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
      let facts_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h_witness5.Ghost.ghost 0 pool x})) Ghost.t = {Ghost.ghost = ghost_ (facts)} in
      let trees_witness3 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness5.Ghost.ghost x then finite h_witness5.Ghost.ghost t else observe h_witness5.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (trees)} in
      let runtime_env = F.Nil in
      ghost_ (F.valid_forest_def runtime_env; F.flatten_def runtime_env);
      let out = infer_compiled h_witness5 0 pool facts_witness2 trees_witness3 runtime_env input (state) in out

let closed_hm : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : inference | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty r.#execution (Pref.own r.#state) r.#pool
      && (not (term_let_free e) || let_free r.#execution) && source r.#execution === e && r.#value === result r.#execution} @ unique = fun e ->
  let input = T.compile e in
  let input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)} = input in
  let out = closed_compiled input in out

let closed : (e : {e : D.term | D.scoped_term D.Z e && term_let_free e}) @ immutable ->
    {r : inference | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty r.#execution (Pref.own r.#state) r.#pool
      && let_free r.#execution && source r.#execution === e && r.#value === result r.#execution} @ unique = fun e ->
    let input : {e : D.term | D.scoped_term D.Z e} = e in
    let out = closed_hm input in out
