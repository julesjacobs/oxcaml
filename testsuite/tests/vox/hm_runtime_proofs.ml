open Copy_spec
open Level_spec
open Generalize_spec
open Hm_runtime_spec

let (safe_finite_scope @ total) : (h : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | safe h x} -> {u : unit | not (H.mem h x) || finite_scope h x} @ ghost =
  fun h x premise -> ghost_ (
    let refine_ premise = premise in safe_def h x; let u = () in
    if H.mem h x then (Generalize_proofs.ordered_scope h x (refine_ u); refine_ u) else refine_ u)

let (allocate_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && depth >= 0 && children_below h desc depth && runtime_at h depth pool x} ->
    {u : unit | runtime_at (H.put h p (cell desc depth)) depth (Entry (p, pool)) x} @ ghost =
  fun h depth pool p desc x premise -> ghost_ (
    let refine_ premise = premise in runtime_at_def h depth pool x; safe_def h x;
    depth_bound_def h depth x; let v = cell desc depth in cell_def desc depth;
    children_below_def h desc depth; payload_scoped_def h v;
    (match desc with Var | Bool -> () | Link q -> below_def h q depth; ()
    | Arrow (a, b) -> below_def h a depth; below_def h b depth; ());
    let u = () in Pooled_allocation_proofs.allocation_source h p v x (refine_ u);
    Pooled_allocation_proofs.allocation_ordered h p desc depth x (refine_ u);
    let cut = depth - 1 in Pooled_allocation_proofs.allocation_coverage h p v pool cut x (refine_ u);
    let after = H.put h p v in Copy_heap_proofs.put_frame h p v x;
    let next = Entry (p, pool) in runtime_at_def after depth next x;
    safe_def after x; depth_bound_def after depth x;
    finite_node_def h x; finite_node_def after x;
    below_def h x depth; below_def after x depth;
    at_level_def h x; at_level_def after x; refine_ u)

let (unify_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : Optimized_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | Optimized_unifier_spec.unified h p q ok after d} ->
    {u : unit | runtime_at after depth pool x} @ ghost =
  fun h depth pool facts p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in facts x; runtime_at_def h depth pool x;
    safe_def h x; depth_bound_def h depth x;
    let scope : ((y : node Pref.t) @ immutable ->
        {u : unit | not (H.mem h y) || finite_scope h y}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y;
      let u = () in let refine_ u = safe_finite_scope h y (refine_ u) in refine_ u in
    let u = () in Optimized_metadata.unified_scope h scope p q ok after d x (refine_ u);
    Optimized_metadata.unified_frame h p q ok after d x (refine_ u);
    let order : ((y : node Pref.t) @ immutable -> {u : unit | ordered h y}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; safe_def h y; let u = () in refine_ u in
    Optimized_metadata.unified_ordered h order p q ok after d x (refine_ u);
    Optimized_metadata.unified_scratch h p q ok after d x (refine_ u);
    Level_unifier_metadata.scratch_frame_def h after x;
    let cut = depth - 1 in covered_def h cut pool x; covered_def after cut pool x;
    (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
    runtime_at_def after depth pool x; safe_def after x; finite_scope_def after x;
    depth_bound_def after depth x; finite_node_def h x; finite_node_def after x;
    below_def h x depth; below_def after x depth; at_level_def h x; at_level_def after x;
    (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
    refine_ u)

let (copy_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid h epoch depth d} ->
    {u : unit | runtime_at (Hm_execution_spec.copy_heap h epoch depth d) depth
      (Pooled_spec.registered pool epoch d) x} @ ghost =
  fun h depth pool facts epoch d x premise -> ghost_ (
    let refine_ premise = premise in facts x; runtime_at_def h depth pool x; safe_def h x;
    let scope : ((y : node Pref.t) @ immutable ->
      {u : unit | if H.mem h y then source_ok h y else H.at h y === None}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; safe_def h y; let u = () in refine_ u in
    let order : ((y : node Pref.t) @ immutable -> {u : unit | ordered h y}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; safe_def h y; let u = () in refine_ u in
    let bounds : ((y : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h y) || not (finite_node h y) || below h y depth}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; depth_bound_def h depth y; let u = () in refine_ u in
    let unmarked : ((y : node Pref.t) @ immutable ->
      {u : unit | match H.at h y with None -> true | Some v -> not v.visited}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; safe_def h y; let u = () in refine_ u in
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in let next = Pooled_spec.registered pool epoch d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | Copy_cleanup_spec.swept_at raw after trail y}) @ total = fun y ->
      let u = () in let refine_ u = Clean_copy.result_at h epoch depth d y (refine_ u) in refine_ u in
    let u = () in Copy_model_proofs.history_scope h scope epoch depth d x (refine_ u);
    Copy_order_proofs.copy_ordered h depth bounds order epoch d x (refine_ u);
    Copy_order_proofs.copy_bounds h depth bounds epoch d x (refine_ u);
    Copy_heap_proofs.history_unmarked h unmarked epoch depth d x (refine_ u);
    Clean_copy.clean_result h epoch depth d x (refine_ u);
    let cut = depth - 1 in Pooled_proofs.registered_covers h pool epoch depth d cut x (refine_ u);
    Copy_cleanup_proofs.sweep_scope raw after trail frame x (refine_ u);
    Copy_cleanup_proofs.sweep_order raw after trail frame x (refine_ u);
    frame x; Copy_cleanup_proofs.sweep_below raw after trail x depth (refine_ u);
    Copy_cleanup_spec.swept_at_def raw after trail x;
    Hm_execution_spec.copy_heap_def h epoch depth d;
    runtime_at_def after depth next x; safe_def after x; depth_bound_def after depth x;
    finite_node_def raw x; finite_node_def after x;
    covered_def raw cut next x; covered_def after cut next x;
    at_level_def raw x; at_level_def after x; refine_ u)

open Hm_execution_spec

let rec (run_runtime @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e} ->
    {u : unit | runtime_at after depth final_pool x} @ ghost =
  fun h depth pool facts env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; let_free_def e;
    let u = () in match e with
    | RShared _ -> facts x; refine_ u
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
      run_runtime start depth next_pool start_facts next_env body middle body_pool x (refine_ u);
      (match result body with None -> refine_ u | Some b -> match out with None -> refine_ u
        | Some p -> let desc = Arrow (arg, b) in allocated_def middle depth p desc;
          allocate_runtime middle depth body_pool p desc x (refine_ u); refine_ u)
    | RApp_left (left, _) -> run_runtime h depth pool facts env left after final_pool x (refine_ u); refine_ u
    | RApp_right (left, right, middle, left_pool) ->
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth left_pool y}) @ total = fun y ->
        let u = () in let refine_ u = run_runtime h depth pool facts env left middle left_pool y (refine_ u) in refine_ u in
      run_runtime middle depth left_pool middle_facts env right after final_pool x (refine_ u); refine_ u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let facts1 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 y}) @ total = fun y ->
        let u = () in let refine_ u = run_runtime h depth pool facts env left h1 pool1 y (refine_ u) in refine_ u in
      let facts2 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 y}) @ total = fun y ->
        let u = () in let refine_ u = run_runtime h1 depth pool1 facts1 env right h2 pool2 y (refine_ u) in refine_ u in
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
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth body_pool y}) @ total = fun y ->
        let u = () in let refine_ u = run_runtime h3 depth pool3 facts3 next_env body middle body_pool y (refine_ u) in refine_ u in
      (match result body with None -> middle_facts x; refine_ u | Some b -> match finish with
        Aborted -> refine_ u | Unified (ok, d) ->
          unify_runtime middle depth body_pool middle_facts b res ok after d x (refine_ u); refine_ u)
    | RLet_left _ | RLet _ -> refine_ u)

let (allocation_active @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && active h x} ->
    {u : unit | active (H.put h p v) x} @ ghost = fun h p v x premise -> ghost_ (
    let refine_ premise = premise in let after = H.put h p v in
    Copy_heap_proofs.put_frame h p v x; active_def h x; active_def after x;
    at_level_def h x; at_level_def after x; let u = () in refine_ u)

let (copy_active @ total) : (h : Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && active h x} ->
    {u : unit | active (copy_heap h epoch depth d) x} @ ghost = fun h epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in Copy_heap_proofs.history_at h epoch depth d x (refine_ u);
    Clean_copy.result_at h epoch depth d x (refine_ u);
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in let after = Copy_cleanup_spec.swept raw trail in
    Copy_cleanup_spec.swept_at_def raw after trail x; copy_heap_def h epoch depth d;
    active_def h x; active_def after x; at_level_def h x; at_level_def after x; refine_ u)

let rec (run_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && active h x} ->
    {u : unit | active after x} @ ghost = fun h depth pool env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; let_free_def e;
    let u = () in match e with
    | RShared _ -> refine_ u
    | RVar (i, _, epoch, d) -> (match Hm_environment_spec.lookup env i with None -> refine_ u
      | Some _ -> copy_active h epoch depth d x (refine_ u); refine_ u)
    | RBool p -> let desc : desc = Bool in allocated_def h depth p desc;
      let v = cell desc depth in allocation_active h p v x (refine_ u); refine_ u
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in allocated_def h depth arg var;
      let start = H.put h arg v in allocation_active h arg v x (refine_ u);
      let next_pool = Entry (arg, pool) in let next_env = Hm_environment_spec.Bind (arg, env) in
      run_active start depth next_pool next_env body middle body_pool x (refine_ u);
      (match result body with None -> refine_ u | Some b -> match out with None -> refine_ u
        | Some p -> let desc = Arrow (arg, b) in allocated_def middle depth p desc;
          let v = cell desc depth in allocation_active middle p v x (refine_ u); refine_ u)
    | RApp_left (left, _) -> run_active h depth pool env left after final_pool x (refine_ u); refine_ u
    | RApp_right (left, right, middle, left_pool) ->
      run_active h depth pool env left middle left_pool x (refine_ u);
      run_active middle depth left_pool env right after final_pool x (refine_ u); refine_ u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run_active h depth pool env left h1 pool1 x (refine_ u);
      run_active h1 depth pool1 env right h2 pool2 x (refine_ u);
      (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        allocation_active h2 p v x (refine_ u); allocation_active h3 arrow w x (refine_ u);
        Optimized_metadata.unified_active h4 f arrow ok after d x (refine_ u); refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let h2 = H.put h1 res v in let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      allocation_active h arg v x (refine_ u); allocation_active h1 res v x (refine_ u); allocation_active h2 self w x (refine_ u);
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Hm_environment_spec.Bind (arg, Hm_environment_spec.Bind (self, env)) in
      run_active h3 depth next_pool next_env body middle body_pool x (refine_ u);
      (match result body with None -> refine_ u | Some b -> match finish with Aborted -> refine_ u
        | Unified (ok, d) -> Optimized_metadata.unified_active middle b res ok after d x (refine_ u); refine_ u)
    | RLet_left _ | RLet _ -> refine_ u)

let (copy_target_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && target_for h d p q} ->
    {u : unit | active (copy_heap h epoch depth d) q} @ ghost =
  fun h depth pool facts epoch d p q premise -> ghost_ (
    let refine_ premise = premise in
    let order : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || ordered h x}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u in
    let u = () in Copy_heap_proofs.extends_def d d;
    Level_copy_proofs.target_active_at h order epoch depth d d p q (refine_ u);
    Clean_copy.result_at h epoch depth d q (refine_ u);
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in
    Copy_cleanup_spec.swept_at_def raw after trail q; copy_heap_def h epoch depth d;
    active_def raw q; active_def after q; at_level_def raw q; at_level_def after q; refine_ u)

let (fresh_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable -> {u : unit | depth >= 0} ->
    {u : unit | active (H.put h p (cell desc depth)) p} @ ghost = fun h depth p desc premise -> ghost_ (
    let refine_ premise = premise in let v = cell desc depth in cell_def desc depth;
    let after = H.put h p v in Copy_heap_proofs.put_frame h p v p;
    active_def after p; at_level_def after p; let u = () in refine_ u)

let (run_result_active @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && result e === Some p} ->
    {u : unit | active after p} @ ghost = fun h depth pool facts env e after final_pool p premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; let_free_def e; result_def e;
    let u = () in match e with
    | RShared _ -> refine_ u
    | RVar (i, q, epoch, d) -> (match Hm_environment_spec.lookup env i with None -> refine_ u
      | Some original -> copy_target_active h depth pool facts epoch d original q (refine_ u); refine_ u)
    | RBool q -> let desc : desc = Bool in fresh_active h depth q desc (refine_ u); refine_ u
    | RLam (arg, body, middle, _, out) -> (match result body with None -> refine_ u | Some b ->
      match out with None -> refine_ u | Some q -> let desc = Arrow (arg, b) in fresh_active middle depth q desc (refine_ u); refine_ u)
    | RApp_left _ | RApp_right _ | RLet_left _ | RLet _ -> refine_ u
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
      run_active h3 depth next_pool next_env body middle body_pool self (refine_ u);
      (match result body with None -> refine_ u | Some b -> match finish with Aborted -> refine_ u
        | Unified (ok, d) -> Optimized_metadata.unified_active middle b res ok after d self (refine_ u); refine_ u))

let (run_result_below @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && result e === Some p} ->
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
    {u : unit | ran h depth pool env e after final_pool && let_free e} ->
    {u : unit | pool_scoped after final_pool} @ ghost = fun h depth pool facts env e after final_pool premise -> ghost_ (
    let refine_ premise = premise in
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total = fun x ->
      let u = () in run_runtime h depth pool facts env e after final_pool x (refine_ u);
      runtime_at_def after depth final_pool x; safe_def after x; refine_ u in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed final_pool x) || H.mem after x}) @ total = fun x ->
      if listed final_pool x then (
        let u = () in Hm_execution_proofs.run_pool_member h depth pool env e after final_pool x (refine_ u); refine_ u)
      else let u = () in refine_ u in
    let refine_ u = Pooled_proofs.pool_from_members after scope final_pool members in refine_ u)

let rec (env_lookup_owned @ total) : (h : Pref.heap) @ immutable ->
    (env : Hm_environment_spec.env) @ immutable -> (i : Hm_declarative.index) @ immutable ->
    {u : unit | env_owned h env && Hm_declarative.present (env_depth env) i} ->
    {u : unit | match Hm_environment_spec.lookup env i with None -> false | Some p -> H.mem h p} @ ghost =
  fun h env i premise -> ghost_ (
    let refine_ premise = premise in env_owned_def h env; env_depth_def env;
    let n = env_depth env in Hm_declarative.present_def n i; Hm_environment_spec.lookup_def env i;
    let u = () in match env with Hm_environment_spec.Empty -> refine_ u
    | Hm_environment_spec.Bind (_, rest) -> match i with Hm_declarative.Z -> refine_ u
      | Hm_declarative.S i -> env_lookup_owned h rest i (refine_ u); refine_ u)

let rec (env_extend @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> {u : unit | env_owned h env} ->
    {u : unit | env_owned after env} @ ghost = fun h after frame env premise -> ghost_ (
    let refine_ premise = premise in env_owned_def h env; env_owned_def after env;
    let u = () in match env with Hm_environment_spec.Empty -> refine_ u
    | Hm_environment_spec.Bind (p, rest) -> frame p; env_extend h after frame rest (refine_ u); refine_ u)

let (run_env_owned @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && env_owned h env} ->
    {u : unit | env_owned after env} @ ghost = fun h depth pool env e after final_pool premise -> ghost_ (
    let refine_ premise = premise in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x}) @ total = fun x ->
      let u = () in let refine_ u = Hm_execution_proofs.run_extends h depth pool env e after final_pool x (refine_ u) in refine_ u in
    let u = () in env_extend h after frame env (refine_ u); refine_ u)

let (allocation_env @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    {u : unit | env_owned h env} -> {u : unit | env_owned (H.put h p v) env} @ ghost = fun h p v env premise -> ghost_ (
    let refine_ premise = premise in let after = H.put h p v in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x}) @ total = fun x ->
      Copy_heap_proofs.put_frame h p v x; let u = () in refine_ u in
    let u = () in env_extend h after frame env (refine_ u); refine_ u)
