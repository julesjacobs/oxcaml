open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_environment_proofs
open Hm_execution_spec

let (allocation_member @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && H.mem h x} ->
    {u : unit | protected_at h (H.put h p v) bound x} @ ghost = fun h p v bound x premise -> ghost_ (
    let after = H.put h p v in Copy_heap_proofs.put_frame h p v x;
    protected_at_def h after bound x; below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x; let u = () in u)
let (copy_member @ total) : (h : Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && H.mem h x} ->
    {u : unit | protected_at h (copy_heap h epoch depth d) bound x} @ ghost = fun h epoch depth d bound x premise -> ghost_ (
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
    let u = () in Copy_heap_proofs.history_at h epoch depth d x (u);
    Clean_copy.result_at h epoch depth d x (u); Copy_cleanup_spec.swept_at_def raw after trail x;
    protected_at_def h after bound x; below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x; u)

let rec (run_member @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && bound <= depth && H.mem h x} ->
    {u : unit | protected_at h after bound x} @ ghost = fun h depth pool env e after final_pool bound x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    let u = () in match e with
    | RShared _ -> protected_at_def h h bound x; u
    | RVar (i, _, epoch, d) -> (match lookup env i with None -> u | Some _ ->
      copy_member h epoch depth d bound x (u); u)
    | RBool p -> let desc : desc = Bool in let v = cell desc depth in allocated_def h depth p desc;
      allocation_member h p v bound x (u); u
    | RApp_left (left, _) -> run_member h depth pool env left after final_pool bound x (u); u
    | RLet_left (rhs, _) -> let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs after final_pool;
      run_member h child_depth empty env rhs after final_pool bound x (u); u
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      allocated_def h depth arg var; allocation_member h arg v bound x (u);
      protected_at_def h h1 bound x;
      run_member h1 depth pool1 env1 body middle body_pool bound x (u);
      protected_trans h h1 middle bound x (u); protected_at_def h middle bound x;
      (match result body with None -> u | Some b -> match out with None -> u | Some p ->
        let desc = Arrow (arg, b) in let w = cell desc depth in allocated_def middle depth p desc;
        allocation_member middle p w bound x (u);
        protected_trans h middle after bound x (u); u)
    | RApp_right (left, right, h1, pool1) ->
      run_member h depth pool env left h1 pool1 bound x (u); protected_at_def h h1 bound x;
      run_member h1 depth pool1 env right after final_pool bound x (u);
      protected_trans h h1 after bound x (u); u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run_member h depth pool env left h1 pool1 bound x (u); protected_at_def h h1 bound x;
      run_member h1 depth pool1 env right h2 pool2 bound x (u);
      protected_trans h h1 h2 bound x (u); protected_at_def h h2 bound x;
      (match result left with None -> u | Some f -> match result right with None -> u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        allocation_member h2 p v bound x (u); protected_at_def h2 h3 bound x;
        allocation_member h3 arrow w bound x (u);
        protected_trans h2 h3 h4 bound x (u); protected_trans h h2 h4 bound x (u);
        unify_protected h4 f arrow ok after d bound x (u);
        protected_trans h h4 after bound x (u); u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in let env3 = Bind (arg, Bind (self, env)) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      allocation_member h arg v bound x (u); protected_at_def h h1 bound x;
      allocation_member h1 res v bound x (u); protected_at_def h1 h2 bound x;
      allocation_member h2 self w bound x (u);
      protected_trans h h1 h2 bound x (u); protected_trans h h2 h3 bound x (u);
      protected_at_def h h3 bound x;
      run_member h3 depth pool3 env3 body middle body_pool bound x (u);
      protected_trans h h3 middle bound x (u);
      (match result body with None -> u | Some b -> match finish with Aborted -> u | Unified (ok, d) ->
        unify_protected middle b res ok after d bound x (u);
        protected_trans h middle after bound x (u); u)
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      run_member h child_depth empty env rhs middle child_pool bound x (u);
      protected_at_def h middle bound x;
      let closed = closed_heap middle depth child_pool in
      close_protected middle depth child_pool bound x (u);
      protected_trans h middle closed bound x (u); protected_at_def h closed bound x;
      let transferred = Nested_pool_spec.transfer closed child_pool pool in
      (match result rhs with None -> u | Some p -> let env1 = Bind (p, env) in
        run_member closed depth transferred env1 body after final_pool bound x (u);
        protected_trans h closed after bound x (u); u))
