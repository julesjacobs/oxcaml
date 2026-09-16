open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_runtime_spec
open Hm_runtime_proofs

let (enter_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | depth >= 0 && depth + 1 >= 0 && runtime_at h depth pool x} ->
    {u : unit | runtime_at h (depth + 1) Generalize_spec.Empty x} @ ghost = fun h depth pool x premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
    runtime_at_def h depth pool x; depth_bound_def h depth x;
    runtime_at_def h child_depth empty x; depth_bound_def h child_depth x;
    covered_def h depth empty x; let cut = child_depth - 1 in covered_def h cut empty x;
    finite_node_def h x; below_def h x depth; below_def h x child_depth; at_level_def h x;
    let u = () in refine_ u)

let (close_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (parent : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth parent x})) @ total ->
    (env : env) @ immutable -> (rhs : execution) @ immutable -> (middle : Pref.heap) @ immutable -> (child : pool) @ immutable ->
    (middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) child x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && ran h (depth + 1) Generalize_spec.Empty env rhs middle child
      && not (result rhs === None) && pool_scoped middle child} ->
    {u : unit | runtime_at (closed_heap middle depth child) depth
      (Nested_pool_spec.transfer (closed_heap middle depth child) child parent) x} @ ghost =
  fun h depth parent facts env rhs middle child middle_facts x premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
    ran_def h child_depth empty env rhs middle child;
    facts x; runtime_at_def h depth parent x; safe_def h x;
    middle_facts x; runtime_at_def middle child_depth child x; safe_def middle x;
    let cut = child_depth - 1 in covered_def middle cut child x; covered_def middle depth child x;
    let closed = closed_heap middle depth child in let transferred = Nested_pool_spec.transfer closed child parent in
    let scope : ((y : node Pref.t) @ immutable -> {u : unit | if H.mem middle y then source_ok middle y else H.at middle y === None}) @ total = fun y ->
      middle_facts y; runtime_at_def middle child_depth child y; safe_def middle y; let u = () in refine_ u in
    let u = () in Generalize_scheme_proofs.closed_scope middle scope depth child x (refine_ u);
    Generalize_proofs.closed_observe middle depth child x (refine_ u); closed_at_def middle closed depth child x;
    if H.mem middle x then (
      Generalize_proofs.closed_ordered middle depth child x (refine_ u);
      Generalize_proofs.closed_level middle depth child x (refine_ u); ())
    else (ordered_def closed x; ());
    runtime_at_def closed depth transferred x; safe_def closed x;
    depth_bound_def closed depth x; finite_node_def closed x; below_def closed x depth;
    at_level_def middle x; at_level_def closed x;
    (match H.at middle x with None -> () | Some v -> close_level_def depth v.level; ());
    ordered_def closed x;
    
    let outer = depth - 1 in covered_def closed outer transferred x;
    Nested_pool_proofs.transfer_listed closed child parent x; Nested_pool_spec.retained_def closed x;
    if H.mem closed x && finite_node closed x && not (listed transferred x) then (
      finite_node_def middle x;
      Hm_registration_proofs.run_unlisted h child_depth empty env rhs middle child x (refine_ u);
      covered_def h outer parent x; at_level_def h x; finite_node_def h x;
      ordered_def h x; below_def h x outer;
      Hm_protected_proofs.run_member h child_depth empty env rhs middle child outer x (refine_ u);
      protected_at_def h middle outer x;
      Generalize_proofs.closed_below middle depth child x outer (refine_ u);
      below_def closed x outer; ()); refine_ u)

let rec (run_invariant @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | safe after x && (result e === None || runtime_at after depth final_pool x)} @ ghost =
  fun h depth pool facts env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; result_def e; runtime_at_def after depth final_pool x;
    let u = () in match e with
    | RVar (i, _, epoch, d) -> (match Hm_environment_spec.lookup env i with
      None -> refine_ u | Some _ -> copy_runtime h depth pool facts epoch d x (refine_ u); refine_ u)
    | RBool p -> facts x; let desc : desc = Bool in allocated_def h depth p desc;
      allocate_runtime h depth pool p desc x (refine_ u); refine_ u
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let start = H.put h arg (cell var depth) in
      let next_pool = Entry (arg, pool) in let next_env = Hm_environment_spec.Bind (arg, env) in
      allocated_def h depth arg var;
      let start_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at start depth next_pool y}) @ total = fun y ->
        facts y; let u = () in let refine_ u = allocate_runtime h depth pool arg var y (refine_ u) in refine_ u in
      run_invariant start depth next_pool start_facts next_env body middle body_pool x (refine_ u);
      (match result body with None -> refine_ u | Some b -> match out with None -> refine_ u
        | Some p -> let desc = Arrow (arg, b) in allocated_def middle depth p desc;
          allocate_runtime middle depth body_pool p desc x (refine_ u); refine_ u)
    | RApp_left (left, _) -> run_invariant h depth pool facts env left after final_pool x (refine_ u); refine_ u
    | RApp_right (left, right, middle, left_pool) ->
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth left_pool y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h depth pool facts env left middle left_pool y (refine_ u) in refine_ u in
      run_invariant middle depth left_pool middle_facts env right after final_pool x (refine_ u); refine_ u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let facts1 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h depth pool facts env left h1 pool1 y (refine_ u) in refine_ u in
      let facts2 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h1 depth pool1 facts1 env right h2 pool2 y (refine_ u) in refine_ u in
      (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some a ->
        let var : desc = Var in let h3 = H.put h2 p (cell var depth) in let pool3 = Entry (p, pool2) in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in let pool4 = Entry (arrow, pool3) in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        let facts3 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 y}) @ total = fun y ->
          facts2 y; let u = () in let refine_ u = allocate_runtime h2 depth pool2 p var y (refine_ u) in refine_ u in
        let facts4 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h4 depth pool4 y}) @ total = fun y ->
          facts3 y; let u = () in let refine_ u = allocate_runtime h3 depth pool3 arrow desc y (refine_ u) in refine_ u in
        unify_runtime h4 depth pool4 facts4 f arrow ok after d x (refine_ u); refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in let pool3 = Entry (self, pool2) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      let facts1 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 y}) @ total = fun y ->
        facts y; let u = () in let refine_ u = allocate_runtime h depth pool arg var y (refine_ u) in refine_ u in
      let facts2 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 y}) @ total = fun y ->
        facts1 y; let u = () in let refine_ u = allocate_runtime h1 depth pool1 res var y (refine_ u) in refine_ u in
      let facts3 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 y}) @ total = fun y ->
        facts2 y; let u = () in let refine_ u = allocate_runtime h2 depth pool2 self desc y (refine_ u) in refine_ u in
      let next_env = Hm_environment_spec.Bind (arg, Hm_environment_spec.Bind (self, env)) in
      (match result body with None ->
        run_invariant h3 depth pool3 facts3 next_env body middle body_pool x (refine_ u); refine_ u
      | Some b -> match finish with Aborted -> refine_ u | Unified (ok, d) ->
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth body_pool y}) @ total = fun y ->
        let u = () in run_invariant h3 depth pool3 facts3 next_env body middle body_pool y (refine_ u); refine_ u in
      unify_runtime middle depth body_pool middle_facts b res ok after d x (refine_ u); refine_ u)
    | RLet_left (rhs, _) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
        facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
      run_invariant h child_depth empty child_facts env rhs after final_pool x (refine_ u); refine_ u
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
        facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) child_pool y}) @ total = fun y ->
        let u = () in run_invariant h child_depth empty child_facts env rhs middle child_pool y (refine_ u); refine_ u in
      let closed = closed_heap middle depth child_pool in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in
      let closed_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at closed depth transferred y}) @ total = fun y ->
        let u = () in close_runtime h depth pool facts env rhs middle child_pool middle_facts y (refine_ u); refine_ u in
      (match result rhs with None -> refine_ u | Some p -> let next_env = Hm_environment_spec.Bind (p, env) in
        run_invariant closed depth transferred closed_facts next_env body after final_pool x (refine_ u); refine_ u))


let (run_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | ran h depth pool env e after final_pool && not (result e === None)} ->
    {u : unit | runtime_at after depth final_pool x} @ ghost = fun h depth pool facts env e after final_pool x premise -> ghost_ (
      let refine_ premise = premise in let u = () in run_invariant h depth pool facts env e after final_pool x (refine_ u); refine_ u)
let (run_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | ran h depth pool env e after final_pool && active h x} ->
    {u : unit | active after x} @ ghost = fun h depth pool facts env e after final_pool x premise -> ghost_ (
      let refine_ premise = premise in facts x; runtime_at_def h depth pool x; depth_bound_def h depth x;
      active_def h x; finite_node_def h x; at_level_def h x;
      let u = () in Hm_protected_proofs.run_member h depth pool env e after final_pool depth x (refine_ u);
      protected_at_def h after depth x; below_def after x depth; active_def after x; at_level_def after x; refine_ u)

let rec (run_result_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {u : unit | active after p} @ ghost = fun h depth pool facts env e after final_pool p premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;  result_def e;
    let u = () in match e with
    | RVar (i, q, epoch, d) -> (match Hm_environment_spec.lookup env i with None -> refine_ u
      | Some original -> copy_target_active h depth pool facts epoch d original q (refine_ u); refine_ u)
    | RBool q -> let desc : desc = Bool in fresh_active h depth q desc (refine_ u); refine_ u
    | RLam (arg, body, middle, _, out) -> (match result body with None -> refine_ u | Some b ->
      match out with None -> refine_ u | Some q -> let desc = Arrow (arg, b) in fresh_active middle depth q desc (refine_ u); refine_ u)
    | RApp_left _ | RApp_right _ | RLet_left _ -> refine_ u
    | RApp (left, right, _, _, h2, _, q, arrow, ok, d) ->
      (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 q v in
        fresh_active h2 depth q var (refine_ u);
        let desc = Arrow (a, q) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h3 depth arrow desc; allocation_active h3 arrow w q (refine_ u);
        Optimized_metadata.unified_active h4 f arrow ok after d q (refine_ u); refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      fresh_active h2 depth self desc (refine_ u);
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Hm_environment_spec.Bind (arg, Hm_environment_spec.Bind (self, env)) in
      cell_def desc depth;
      below_def h3 self depth; at_level_def h3 self;
      Hm_protected_proofs.run_member h3 depth next_pool next_env body middle body_pool depth self (refine_ u);
      protected_at_def h3 middle depth self; below_def middle self depth;
      active_def middle self; at_level_def middle self;
      (match result body with None -> refine_ u | Some b -> match finish with Aborted -> refine_ u
        | Unified (ok, d) -> Optimized_metadata.unified_active middle b res ok after d self (refine_ u); refine_ u)
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
        facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) child_pool y}) @ total = fun y ->
        let u = () in run_runtime h child_depth empty child_facts env rhs middle child_pool y (refine_ u); refine_ u in
      let closed = closed_heap middle depth child_pool in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in
      let closed_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at closed depth transferred y}) @ total = fun y ->
        let u = () in close_runtime h depth pool facts env rhs middle child_pool middle_facts y (refine_ u); refine_ u in
      (match result rhs with None -> refine_ u | Some q -> let next_env = Hm_environment_spec.Bind (q, env) in
        run_result_active closed depth transferred closed_facts next_env body after final_pool p (refine_ u); refine_ u))

let (run_result_below @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {u : unit | below after p depth} @ ghost = fun h depth pool facts env e after final_pool p premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    run_runtime h depth pool facts env e after final_pool p (refine_ u);
    run_result_active h depth pool facts env e after final_pool p (refine_ u);
    runtime_at_def after depth final_pool p; depth_bound_def after depth p;
    active_def after p; finite_node_def after p; at_level_def after p; refine_ u)

let (run_pool_scoped @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | pool_scoped after final_pool} @ ghost = fun h depth pool facts env e after final_pool premise -> ghost_ (
    let refine_ premise = premise in
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total = fun x ->
      let u = () in run_invariant h depth pool facts env e after final_pool x (refine_ u);
      runtime_at_def after depth final_pool x; safe_def after x; refine_ u in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed final_pool x) || H.mem after x}) @ total = fun x ->
      if listed final_pool x then (
        let u = () in Hm_execution_proofs.run_pool_member h depth pool env e after final_pool x (refine_ u); refine_ u)
      else let u = () in refine_ u in
    let refine_ u = Pooled_proofs.pool_from_members after scope final_pool members in refine_ u)


let (run_saved_pool @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (saved : pool) @ immutable -> {u : unit | ran h depth pool env e after final_pool && pool_scoped h saved} ->
    {u : unit | pool_scoped after saved} @ ghost = fun h depth pool facts env e after final_pool saved premise -> ghost_ (
      let refine_ premise = premise in
      let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total = fun x ->
        let u = () in run_invariant h depth pool facts env e after final_pool x (refine_ u); safe_def after x; refine_ u in
      let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed saved x) || H.mem after x}) @ total = fun x ->
        let u = () in if listed saved x then (Pooled_proofs.pool_member h saved x (refine_ u);
        Hm_execution_proofs.run_extends h depth pool env e after final_pool x (refine_ u); refine_ u) else refine_ u in
      let refine_ u = Pooled_proofs.pool_from_members after scope saved members in refine_ u)
