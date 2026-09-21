open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
module R = Representative_level

let[@def] (bounded_path @ total) (h : node Pref.heap @ immutable)
    (x : node Pref.t @ immutable) (r : R.representative @ immutable)
    (bound : int) = ghost_ (
  resolves h x r.root r.path && below h r.root bound)

let (allocate @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    (bound : int) -> {u : unit | not (H.mem h p) && bounded_path h x r bound} ->
    {u : unit | bounded_path (H.put h p v) x r bound} @ ghost =
  fun h p v x r bound premise -> ghost_ (
    let refine_ premise = premise in bounded_path_def h x r bound;
    let after = H.put h p v in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h y) || (H.mem after y
        && observe h y === observe after y)}) @ total = fun y ->
      Copy_heap_proofs.put_frame h p v y; observe_def h y; observe_def after y;
      let u = () in refine_ u in
    let u = () in Effective_copy_metadata.resolution_grows h after frame x r.root r.path (refine_ u);
    Pooled_allocation_proofs.allocation_below h p v r.root bound (refine_ u);
    bounded_path_def after x r bound; refine_ u)

let (copy @ total) : (h : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    (bound : int) ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d
      && bounded_path h x r bound} ->
    {u : unit | bounded_path (copy_heap h epoch depth d) x r bound} @ ghost =
  fun h certificate epoch depth d x r bound premise -> ghost_ (
    let refine_ premise = premise in bounded_path_def h x r bound;
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h y) || (H.mem after y
        && observe h y === observe after y)}) @ total = fun y ->
      let u = () in Hm_effective_registration.history_at h certificate epoch depth d y (refine_ u);
      Hm_effective_registration.result_at h certificate epoch depth d y (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail y;
      observe_def h y; observe_def after y; refine_ u in
    let u = () in Effective_copy_metadata.resolution_grows h after frame x r.root r.path (refine_ u);
    below_def h r.root bound;
    Hm_effective_bound.copy_member h certificate epoch depth d bound r.root (refine_ u);
    Hm_effective_bound.preserved_bound_def h after bound r.root;
    bounded_path_def after x r bound; refine_ u)

let (close @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    (bound : int) ->
    {u : unit | pool_scoped h pool && bound <= cut && bounded_path h x r bound} ->
    {u : unit | bounded_path (Representative_pool_spec.close_heap h cut pool) x r bound} @ ghost =
  fun h cut pool x r bound premise -> ghost_ (
    let refine_ premise = premise in bounded_path_def h x r bound;
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = R.representatives h pool in let u = () in
    R.representatives_scoped h pool (refine_ u);
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | H.mem h y === H.mem after y
        && observe h y === observe after y}) @ total = fun y ->
      let u = () in Generalize_proofs.closed_observe h cut filtered y (refine_ u);
      closed_at_def h after cut filtered y; observe_def h y; observe_def after y; refine_ u in
    R.resolution_frame h after frame x r.root r.path;
    below_def h r.root bound;
    Hm_effective_bound.close_bound h cut pool bound r.root (refine_ u);
    Hm_effective_bound.preserved_bound_def h after bound r.root;
    bounded_path_def after x r bound; refine_ u)

let (unify @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    (bound : int) ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d && bounded_path h x r bound} ->
    {s : R.representative | bounded_path after x s bound} @ immutable ghost =
  fun h p q ok after d x r bound premise -> ghost_ (
    let refine_ premise = premise in bounded_path_def h x r bound; let u = () in
    let refine_ s = Effective_unifier_heads.unified_head h p q ok after d x r.root r.path (refine_ u) in
    Effective_unifier_heads.progress_def h r.root after s.root;
    let before_level = at_level h r.root in let after_level = at_level after s.root in
    decreases_def before_level after_level;
    Compression_path_proofs.resolution_terminal after x s.root s.path (refine_ u);
    below_def h r.root bound; below_def after s.root bound;
    bounded_path_def after x s bound; refine_ s)

let rec (run @ total) : (h : node Pref.heap) @ immutable ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    (r : R.representative) @ immutable -> (bound : int) ->
    {u : unit | ran h depth pool env e after final_pool && bound <= depth
      && bounded_path h x r bound} ->
    {s : R.representative | bounded_path after x s bound} @ immutable ghost =
  fun h depth pool env e after final_pool x r bound premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
    let u = () in match e with
    | RShared _ -> refine_ r
    | RVar (i, target, epoch, d, certificate) ->
      (match lookup env i with None -> refine_ r | Some original ->
        Copy_certificate_spec.certifies_def h certificate epoch depth d original target;
        copy h certificate epoch depth d x r bound (refine_ u); refine_ r)
    | RBool p -> let desc = Bool in let v = cell desc depth in
      allocated_def h depth p desc; allocate h p v x r bound (refine_ u); refine_ r
    | RApp_left (left, _) ->
      let refine_ s = run h depth pool env left after final_pool x r bound (refine_ u) in refine_ s
    | RLet_left (rhs, _) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let refine_ s = run h child_depth empty env rhs after final_pool x r bound (refine_ u) in refine_ s
    | RLam (arg, body, middle, body_pool, out) ->
      let desc = Var in let v = cell desc depth in let start = H.put h arg v in
      let next_pool = Entry (arg, pool) in let next_env = Bind (arg, env) in
      allocated_def h depth arg desc; allocate h arg v x r bound (refine_ u);
      let refine_ s = run start depth next_pool next_env body middle body_pool x r bound (refine_ u) in
      (match result body with None -> refine_ s | Some b -> match out with None -> refine_ s | Some p ->
        let desc = Arrow (arg, b) in let v = cell desc depth in
        allocated_def middle depth p desc; allocate middle p v x s bound (refine_ u); refine_ s)
    | RApp_right (left, right, middle, left_pool) ->
      let refine_ s = run h depth pool env left middle left_pool x r bound (refine_ u) in
      let refine_ t = run middle depth left_pool env right after final_pool x s bound (refine_ u) in refine_ t
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let refine_ s = run h depth pool env left h1 pool1 x r bound (refine_ u) in
      let refine_ t = run h1 depth pool1 env right h2 pool2 x s bound (refine_ u) in
      (match result left with None -> refine_ t | Some f -> match result right with None -> refine_ t | Some a ->
        let var = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        allocate h2 p v x t bound (refine_ u); allocate h3 arrow w x t bound (refine_ u);
        let refine_ out = unify h4 f arrow ok after d x t bound (refine_ u) in refine_ out)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in let env3 = Bind (arg, Bind (self, env)) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      allocate h arg v x r bound (refine_ u); allocate h1 res v x r bound (refine_ u);
      allocate h2 self w x r bound (refine_ u);
      let refine_ s = run h3 depth pool3 env3 body middle body_pool x r bound (refine_ u) in
      (match result body with None -> refine_ s | Some b -> match finish with Aborted -> refine_ s | Unified (ok, d) ->
        let refine_ t = unify middle b res ok after d x s bound (refine_ u) in refine_ t)
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let refine_ s = run h child_depth empty env rhs middle child_pool x r bound (refine_ u) in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      close middle depth child_pool x s bound (refine_ u);
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      (match result rhs with None -> refine_ s | Some p -> let next_env = Bind (p, env) in
        let refine_ t = run closed depth transferred next_env body after final_pool x s bound (refine_ u) in refine_ t))

let (run_below @ total) : (h : node Pref.heap) @ immutable ->
    (a : Effective_level.heads) @ total -> (b : Effective_level.heads) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | ran h depth pool env e after final_pool && bound <= depth
      && Effective_level.valid_head h a x && Effective_level.valid_head after b x
      && Effective_level.effective_below h a x bound} ->
    {u : unit | Effective_level.effective_below after b x bound} @ ghost =
  fun h a b depth pool env e after final_pool x bound premise -> ghost_ (
    let refine_ premise = premise in
    Effective_level.valid_head_def h a x; Effective_level.valid_head_def after b x;
    Effective_level.effective_below_def h a x bound; Effective_level.level_def h a x;
    let u = () in Effective_level.head_terminal h a x (refine_ u);
    let r = a x in below_def h r.root bound; bounded_path_def h x r bound;
    let u = () in let refine_ s = run h depth pool env e after final_pool x r bound (refine_ u) in
    bounded_path_def after x s bound; resolves_def after x s.root s.path;
    let t = b x in R.unique after x s.root s.path t.root t.path (refine_ u);
    below_def after s.root bound;
    Effective_level.effective_below_def after b x bound; Effective_level.level_def after b x;
    refine_ u)
