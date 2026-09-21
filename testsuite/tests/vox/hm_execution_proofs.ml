open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec

let (copy_extends @ total) : (h : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid h epoch depth d} ->
    {u : unit | not (H.mem h x) || H.mem (copy_heap h epoch depth d) x} @ ghost =
  fun h epoch depth d x premise -> ghost_ (
    let u = () in
    Copy_heap_proofs.history_grows h epoch depth d x (u);
    Clean_copy.result_at h epoch depth d x (u);
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in
    Copy_cleanup_spec.swept_at_def raw after trail x;
    copy_heap_def h epoch depth d; u)

let (copy_result @ total) : (h : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && target_for h d p q} ->
    {u : unit | H.mem (copy_heap h epoch depth d) q} @ ghost =
  fun h epoch depth d p q premise -> ghost_ (
    let u = () in
    Copy_heap_proofs.target_allocated h epoch depth d p q (u);
    Clean_copy.result_at h epoch depth d q (u);
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in
    Copy_cleanup_spec.swept_at_def raw after trail q;
    copy_heap_def h epoch depth d; u)

let rec (run_extends @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | not (H.mem h x) || H.mem after x} @ ghost =
  fun h depth pool env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    let u = () in match e with
    | RShared _ -> u
    | RVar (i, _, epoch, d) -> (match lookup env i with
      None -> u | Some _ -> copy_extends h epoch depth d x (u); u)
    | RBool p -> let desc : desc = Bool in let v = cell desc depth in
      Copy_heap_proofs.put_frame h p v x; u
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let start = H.put h arg v in
      Copy_heap_proofs.put_frame h arg v x;
      let next_pool = Entry (arg, pool) in let next_env = Bind (arg, env) in
      run_extends start depth next_pool next_env body middle body_pool x (u);
      (match result body with None -> u | Some b -> match out with None -> u
      | Some p -> let desc = Arrow (arg, b) in let v = cell desc depth in
        Copy_heap_proofs.put_frame middle p v x; u)
    | RApp_left (left, _) -> run_extends h depth pool env left after final_pool x (u); u
    | RApp_right (left, right, middle, left_pool) ->
      run_extends h depth pool env left middle left_pool x (u);
      run_extends middle depth left_pool env right after final_pool x (u); u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run_extends h depth pool env left h1 pool1 x (u);
      run_extends h1 depth pool1 env right h2 pool2 x (u);
      (match result left with None -> u | Some f ->
        match result right with None -> u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        Copy_heap_proofs.put_frame h2 p v x;
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        Copy_heap_proofs.put_frame h3 arrow w x;
        Optimized_metadata.unified_frame h4 f arrow ok after d x (u); u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let h2 = H.put h1 res v in let desc = Arrow (arg, res) in let w = cell desc depth in
      let h3 = H.put h2 self w in
      Copy_heap_proofs.put_frame h arg v x; Copy_heap_proofs.put_frame h1 res v x;
      Copy_heap_proofs.put_frame h2 self w x;
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Bind (arg, Bind (self, env)) in
      run_extends h3 depth next_pool next_env body middle body_pool x (u);
      (match result body with None -> u | Some b -> match finish with Aborted -> u
        | Unified (ok, d) -> Optimized_metadata.unified_frame middle b res ok after d x (u); u)
    | RLet_left (rhs, _) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_extends h next empty env rhs after final_pool x (u); u
    | RLet (rhs, body, middle, child_pool) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_extends h next empty env rhs middle child_pool x (u);
      (match result rhs with None -> u | Some p ->
        Generalize_proofs.closed_observe middle depth child_pool x (u);
        let closed = closed_heap middle depth child_pool in closed_at_def middle closed depth child_pool x;
        let parent = Nested_pool_spec.transfer closed child_pool pool in let next_env = Bind (p, env) in
        run_extends closed depth parent next_env body after final_pool x (u); u))

let rec (run_result @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {u : unit | H.mem after p} @ ghost = fun h depth pool env e after final_pool p premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    result_def e; let u = () in match e with
    | RShared _ -> active_def h p; u
    | RVar (i, q, epoch, d) -> (match lookup env i with None -> u
      | Some original -> copy_result h epoch depth d original q (u); u)
    | RBool q -> let desc : desc = Bool in let v = cell desc depth in
      Copy_heap_proofs.put_frame h q v q; u
    | RLam (arg, body, middle, body_pool, out) -> (match result body with None -> u
      | Some b -> match out with None -> u | Some q ->
        let desc = Arrow (arg, b) in let v = cell desc depth in
        Copy_heap_proofs.put_frame middle q v q; u)
    | RApp_left _ | RApp_right _ | RLet_left _ -> u
    | RApp (left, right, _, _, h2, _, q, arrow, ok, d) ->
      (match result left with None -> u | Some f ->
        match result right with None -> u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 q v in
        Copy_heap_proofs.put_frame h2 q v q;
        let desc = Arrow (a, q) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        Copy_heap_proofs.put_frame h3 arrow w q;
        Optimized_metadata.unified_frame h4 f arrow ok after d q (u); u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let h2 = H.put h1 res v in let desc = Arrow (arg, res) in let w = cell desc depth in
      let h3 = H.put h2 self w in Copy_heap_proofs.put_frame h2 self w self;
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Bind (arg, Bind (self, env)) in
      run_extends h3 depth next_pool next_env body middle body_pool self (u);
      (match result body with None -> u | Some b -> match finish with Aborted -> u
        | Unified (ok, d) -> Optimized_metadata.unified_frame middle b res ok after d self (u); u)
    | RLet (rhs, body, middle, child_pool) -> (match result rhs with None -> u | Some q ->
      let closed = closed_heap middle depth child_pool in
      let parent = Nested_pool_spec.transfer closed child_pool pool in let next_env = Bind (q, env) in
      run_result closed depth parent next_env body after final_pool p (u); u))

let rec (run_pool_member @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && listed final_pool x} ->
    {u : unit | H.mem after x} @ ghost = fun h depth pool env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    let u = () in match e with
    | RShared _ -> Pooled_proofs.pool_member h pool x (u); u
    | RVar (i, _, epoch, d) -> (match lookup env i with None -> u | Some _ ->
      Pooled_proofs.registered_member h pool epoch depth d x (u);
      if listed pool x then (
        Pooled_proofs.pool_member h pool x (u); copy_extends h epoch depth d x (u); u)
      else (
        Clean_copy.result_at h epoch depth d x (u);
        let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in let after = Copy_cleanup_spec.swept raw trail in
        Copy_cleanup_spec.swept_at_def raw after trail x; copy_heap_def h epoch depth d; u))
    | RBool p -> let next = Entry (p, pool) in listed_def next x;
      let desc : desc = Bool in let v = cell desc depth in Copy_heap_proofs.put_frame h p v x;
      if x === p then u else (Pooled_proofs.pool_member h pool x (u); u)
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let start = H.put h arg (cell var depth) in
      let next_pool = Entry (arg, pool) in let next_env = Bind (arg, env) in
      (match result body with None -> run_pool_member start depth next_pool next_env body middle body_pool x (u); u
      | Some b -> match out with None -> u | Some p ->
        let next = Entry (p, body_pool) in listed_def next x;
        let desc = Arrow (arg, b) in let v = cell desc depth in Copy_heap_proofs.put_frame middle p v x;
        if x === p then u else (
          run_pool_member start depth next_pool next_env body middle body_pool x (u); u))
    | RApp_left (left, _) -> run_pool_member h depth pool env left after final_pool x (u); u
    | RApp_right (_, right, middle, left_pool) ->
      run_pool_member middle depth left_pool env right after final_pool x (u); u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      (match result left with None -> u | Some f -> match result right with None -> u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        let pool3 = Entry (p, pool2) in let pool4 = Entry (arrow, pool3) in listed_def pool4 x; listed_def pool3 x;
        Copy_heap_proofs.put_frame h2 p v x; Copy_heap_proofs.put_frame h3 arrow w x;
        if x === p || x === arrow then () else
          (run_pool_member h1 depth pool1 env right h2 pool2 x (u); ());
        Optimized_metadata.unified_frame h4 f arrow ok after d x (u); u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in let next_env = Bind (arg, Bind (self, env)) in
      run_pool_member h3 depth next_pool next_env body middle body_pool x (u);
      (match result body with None -> u | Some b -> match finish with Aborted -> u
      | Unified (ok, d) -> Optimized_metadata.unified_frame middle b res ok after d x (u); u)
    | RLet_left (rhs, _) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_pool_member h next empty env rhs after final_pool x (u); u
    | RLet (rhs, body, middle, child_pool) -> (match result rhs with None -> u | Some p ->
      let closed = closed_heap middle depth child_pool in let parent = Nested_pool_spec.transfer closed child_pool pool in
      let next_env = Bind (p, env) in run_pool_member closed depth parent next_env body after final_pool x (u); u))
