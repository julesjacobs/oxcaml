open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec

let (copy_extends @ total) : (h : Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d} ->
    {u : unit | not (H.mem h x) || H.mem (copy_heap h epoch depth d) x} @ ghost =
  fun h certificate epoch depth d x premise -> ghost_ (
    Hm_effective_registration.history_at h certificate epoch depth d x ();
    Hm_effective_registration.result_at h certificate epoch depth d x ();
    let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in
    Copy_cleanup_spec.swept_at_def raw after trail x;
    copy_heap_def h epoch depth d; ())

let rec (run_extends @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | not (H.mem h x) || H.mem after x} @ ghost =
  fun h depth pool env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    match e with
    | RShared _ -> ()
    | RVar (i, target, epoch, d, certificate) -> (match lookup env i with
      None -> () | Some original -> Copy_certificate_spec.certifies_def h certificate epoch depth d original target; copy_extends h certificate epoch depth d x (); ())
    | RBool p -> let desc : desc = Bool in let _ = cell desc depth in
      ()
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let start = H.put h arg v in
      let next_pool = Entry (arg, pool) in let next_env = Bind (arg, env) in
      run_extends start depth next_pool next_env body middle body_pool x ();
      (match result body with None -> () | Some b -> match out with None -> ()
      | Some p -> let desc = Arrow (arg, b) in let _ = cell desc depth in
        ())
    | RApp_left (left, _) -> run_extends h depth pool env left after final_pool x (); ()
    | RApp_right (left, right, middle, left_pool) ->
      run_extends h depth pool env left middle left_pool x ();
      run_extends middle depth left_pool env right after final_pool x (); ()
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run_extends h depth pool env left h1 pool1 x ();
      run_extends h1 depth pool1 env right h2 pool2 x ();
      (match result left with None -> () | Some f ->
        match result right with None -> () | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        Effective_unifier_frame.unified_frame h4 f arrow ok after d x (); ())
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let h2 = H.put h1 res v in let desc = Arrow (arg, res) in let w = cell desc depth in
      let h3 = H.put h2 self w in
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Bind (arg, Bind (self, env)) in
      run_extends h3 depth next_pool next_env body middle body_pool x ();
      (match result body with None -> () | Some b -> match finish with Aborted -> ()
        | Unified (ok, d) -> Effective_unifier_frame.unified_frame middle b res ok after d x (); ())
    | RLet_left (rhs, _) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_extends h next empty env rhs after final_pool x (); ()
    | RLet (rhs, body, middle, child_pool) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_extends h next empty env rhs middle child_pool x ();
      (match result rhs with None -> () | Some p ->
        Representative_pool_spec.close_heap_def middle depth child_pool;
        Representative_level.representatives_scoped middle child_pool ();
        let filtered = Representative_level.representatives middle child_pool in
        Generalize_proofs.closed_observe middle depth filtered x ();
        let closed = Representative_pool_spec.close_heap middle depth child_pool in closed_at_def middle closed depth filtered x;
        let parent = Representative_pool_spec.transfer_rep closed child_pool pool in let next_env = Bind (p, env) in
        run_extends closed depth parent next_env body after final_pool x (); ()))

let rec (run_pool_member @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && listed final_pool x} ->
    {u : unit | H.mem after x} @ ghost = fun h depth pool env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    match e with
    | RShared _ -> Pooled_proofs.pool_member h pool x (); ()
    | RVar (i, target, epoch, d, certificate) -> (match lookup env i with None -> () | Some original ->
      Copy_certificate_spec.certifies_def h certificate epoch depth d original target;
      Hm_effective_registration.registered_member h certificate pool epoch depth d x ();
      if listed pool x then (
        Pooled_proofs.pool_member h pool x (); copy_extends h certificate epoch depth d x (); ())
      else (
        Hm_effective_registration.result_at h certificate epoch depth d x ();
        let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in let after = Copy_cleanup_spec.swept raw trail in
        Copy_cleanup_spec.swept_at_def raw after trail x; copy_heap_def h epoch depth d; ()))
    | RBool p -> let next = Entry (p, pool) in listed_def next x;
      let desc : desc = Bool in let _ = cell desc depth in if x === p then () else (Pooled_proofs.pool_member h pool x (); ())
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let start = H.put h arg (cell var depth) in
      let next_pool = Entry (arg, pool) in let next_env = Bind (arg, env) in
      (match result body with None -> run_pool_member start depth next_pool next_env body middle body_pool x (); ()
      | Some b -> match out with None -> () | Some p ->
        let next = Entry (p, body_pool) in listed_def next x;
        let desc = Arrow (arg, b) in let _ = cell desc depth in if x === p then () else (
          run_pool_member start depth next_pool next_env body middle body_pool x (); ()))
    | RApp_left (left, _) -> run_pool_member h depth pool env left after final_pool x (); ()
    | RApp_right (_, right, middle, left_pool) ->
      run_pool_member middle depth left_pool env right after final_pool x (); ()
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      (match result left with None -> () | Some f -> match result right with None -> () | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        let pool3 = Entry (p, pool2) in let pool4 = Entry (arrow, pool3) in listed_def pool4 x; listed_def pool3 x;
        if x === p || x === arrow then () else
          (run_pool_member h1 depth pool1 env right h2 pool2 x (); ());
        Effective_unifier_frame.unified_frame h4 f arrow ok after d x (); ())
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in let next_env = Bind (arg, Bind (self, env)) in
      run_pool_member h3 depth next_pool next_env body middle body_pool x ();
      (match result body with None -> () | Some b -> match finish with Aborted -> ()
      | Unified (ok, d) -> Effective_unifier_frame.unified_frame middle b res ok after d x (); ())
    | RLet_left (rhs, _) -> let next = depth + 1 in let empty : pool = Generalize_spec.Empty in
      run_pool_member h next empty env rhs after final_pool x (); ()
    | RLet (rhs, body, middle, child_pool) -> (match result rhs with None -> () | Some p ->
      let closed = Representative_pool_spec.close_heap middle depth child_pool in let parent = Representative_pool_spec.transfer_rep closed child_pool pool in
      let next_env = Bind (p, env) in run_pool_member closed depth parent next_env body after final_pool x (); ()))
