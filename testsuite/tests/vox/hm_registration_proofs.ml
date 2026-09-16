open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec

let rec (copy_new_member @ total) : (h : Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d} ->
    {u : unit | not (H.mem (heap h epoch depth d) x) || H.mem h x || listed (Pooled_spec.registered pool epoch d) x} @ ghost =
  fun h pool epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in valid_def h epoch depth d; heap_def h epoch depth d;
    Pooled_spec.registered_def pool epoch d; let out = Pooled_spec.registered pool epoch d in listed_def out x;
    let u = () in match d with
    | Start -> let desc : desc = Bool in let v = cell desc depth in Copy_heap_proofs.put_frame h epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      copy_new_member h pool epoch depth rest x (refine_ u);
      let mid = heap h epoch depth rest in let v = cell desc depth in let h1 = H.put mid q v in let w = mark old epoch q in
      Copy_heap_proofs.put_frame mid q v x; Copy_heap_proofs.put_frame h1 p w x; refine_ u
    | Alias (rest, p, q, old) ->
      copy_new_member h pool epoch depth rest x (refine_ u);
      let mid = heap h epoch depth rest in let w = mark old epoch q in Copy_heap_proofs.put_frame mid p w x; refine_ u)

let (copy_unlisted @ total) : (h : Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && H.mem (copy_heap h epoch depth d) x
      && finite_node (copy_heap h epoch depth d) x && not (listed (Pooled_spec.registered pool epoch d) x)} ->
    {u : unit | H.mem h x && finite_node h x && not (listed pool x)} @ ghost = fun h pool epoch depth d x premise -> ghost_ (
      let refine_ premise = premise in let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
      let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
      let u = () in Clean_copy.result_at h epoch depth d x (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail x; copy_new_member h pool epoch depth d x (refine_ u);
      Pooled_proofs.registered_keeps pool epoch d x; Copy_heap_proofs.history_at h epoch depth d x (refine_ u);
      finite_node_def h x; finite_node_def after x; refine_ u)
let (allocation_unlisted @ total) : (h : Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p v) x && finite_node (H.put h p v) x && not (listed (Entry (p, pool)) x)} ->
    {u : unit | H.mem h x && finite_node h x && not (listed pool x)} @ ghost = fun h pool p v x premise -> ghost_ (
      let refine_ premise = premise in let after = H.put h p v in let out = Entry (p, pool) in listed_def out x;
      Copy_heap_proofs.put_frame h p v x; finite_node_def h x; finite_node_def after x; let u = () in refine_ u)
let (unify_finite_before @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : Optimized_unifier_spec.derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Optimized_unifier_spec.unified h p q ok after d && H.mem after x && finite_node after x} ->
    {u : unit | H.mem h x && finite_node h x} @ ghost = fun h p q ok after d x premise -> ghost_ (
      let refine_ premise = premise in let u = () in Optimized_metadata.unified_frame h p q ok after d x (refine_ u);
      Optimized_metadata.unified_scratch h p q ok after d x (refine_ u);
      Level_unifier_metadata.scratch_frame_def h after x;
      finite_node_def h x; finite_node_def after x;
      (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); refine_ u)

let rec (run_unlisted @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && not (result e === None)
      && H.mem after x && finite_node after x && not (listed final_pool x)} ->
    {u : unit | H.mem h x && finite_node h x && not (listed pool x)} @ ghost =
  fun h depth pool env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; result_def e;
    let u = () in match e with
    | RVar (i, _, epoch, d) -> (match lookup env i with None -> refine_ u | Some _ ->
      copy_unlisted h pool epoch depth d x (refine_ u); refine_ u)
    | RBool p -> let desc : desc = Bool in let v = cell desc depth in
      allocation_unlisted h pool p v x (refine_ u); refine_ u
    | RApp_left _ | RApp_right _ | RLet_left _ -> refine_ u
    | RLam (arg, body, middle, body_pool, out) ->
      (match result body with None -> refine_ u | Some b -> match out with None -> refine_ u | Some p ->
      let desc = Arrow (arg, b) in let w = cell desc depth in allocation_unlisted middle body_pool p w x (refine_ u);
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      run_unlisted h1 depth pool1 env1 body middle body_pool x (refine_ u);
      allocation_unlisted h pool arg v x (refine_ u); refine_ u)
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in let pool3 = Entry (p, pool2) in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        unify_finite_before h4 f arrow ok after d x (refine_ u);
        allocation_unlisted h3 pool3 arrow w x (refine_ u); allocation_unlisted h2 pool2 p v x (refine_ u);
        run_unlisted h1 depth pool1 env right h2 pool2 x (refine_ u);
        run_unlisted h depth pool env left h1 pool1 x (refine_ u); refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      (match result body with None -> refine_ u | Some b -> match finish with Aborted -> refine_ u | Unified (ok, d) ->
        unify_finite_before middle b res ok after d x (refine_ u);
        let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
        let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
        let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
        let pool3 = Entry (self, pool2) in let env3 = Bind (arg, Bind (self, env)) in
        run_unlisted h3 depth pool3 env3 body middle body_pool x (refine_ u);
        allocation_unlisted h2 pool2 self w x (refine_ u); allocation_unlisted h1 pool1 res v x (refine_ u);
        allocation_unlisted h pool arg v x (refine_ u); refine_ u)
    | RLet (rhs, body, middle, child_pool) -> (match result rhs with None -> refine_ u | Some p ->
      let closed = closed_heap middle depth child_pool in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in let env1 = Bind (p, env) in
      run_unlisted closed depth transferred env1 body after final_pool x (refine_ u);
      Nested_pool_proofs.transfer_listed closed child_pool pool x; Nested_pool_spec.retained_def closed x;
      finite_node_def closed x;
      Generalize_proofs.closed_observe middle depth child_pool x (refine_ u); closed_at_def middle closed depth child_pool x;
      finite_node_def middle x;
      let child_depth = depth + 1 in let empty : pool = Empty in
      run_unlisted h child_depth empty env rhs middle child_pool x (refine_ u); refine_ u))
