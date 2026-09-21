open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec

let rec (history_at @ total) : (saved : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | Copy_certificate_spec.certified_valid saved certificate epoch depth d} ->
    {u : unit | not (H.mem saved x) || (H.mem (heap saved epoch depth d) x &&
      match H.at saved x, H.at (heap saved epoch depth d) x with
      | Some old, Some now -> old.desc === now.desc && old.level === now.level && old.visited === now.visited &&
        (match mapping d x with None -> now.memo === old.memo
        | Some q -> now.memo === (if clean_session d then Forward q else Memo (epoch, q)))
      | None, None -> true | _ -> false)} @ ghost = fun saved certificate epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in Copy_certificate_spec.certified_valid_def saved certificate epoch depth d;
  heap_def saved epoch depth d; mapping_def d x; clean_session_def d;
  let u = () in match d with
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
  | Clean -> refine_ u
  | Fresh (rest, p, q, old, desc) ->
    history_at saved certificate epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let v = cell desc depth in
    put_frame h q v x; let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame h1 p w x; mark_def old epoch q; cell_def desc depth; refine_ u
  | Alias (rest, p, q, old) ->
    history_at saved certificate epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w x;
    mark_def old epoch q; refine_ u)

let rec (touched_saved @ total) : (saved : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid saved certificate epoch depth d} ->
    {u : unit | not (listed (Pooled_spec.touched d) x) || (H.mem saved x
      && match H.at saved x with None -> false | Some _ -> true)} @ ghost =
  fun saved certificate epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in Copy_certificate_spec.certified_valid_def saved certificate epoch depth d;
    Pooled_spec.touched_def d; let trail = Pooled_spec.touched d in listed_def trail x;
    let u = () in match d with Start | Clean -> refine_ u
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
      touched_saved saved certificate epoch depth rest x (refine_ u);
      history_at saved certificate epoch depth rest x (refine_ u); refine_ u)

let (result_at @ total) : (h : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d} ->
    {u : unit | Copy_cleanup_spec.swept_at (heap h epoch depth d)
      (copy_heap h epoch depth d) (Pooled_spec.touched d) x} @ ghost =
  fun h certificate epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let raw = heap h epoch depth d in
    let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (listed trail y) || H.mem raw y}) @ total = fun y ->
      let u = () in touched_saved h certificate epoch depth d y (refine_ u);
      history_at h certificate epoch depth d y (refine_ u); refine_ u in
    let refine_ out = Copy_cleanup_proofs.sweep_at raw trail members x in refine_ out)

let rec (copy_new_member @ total) : (h : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d} ->
    {u : unit | not (H.mem (heap h epoch depth d) x) || H.mem h x || listed (Pooled_spec.registered pool epoch d) x} @ ghost =
  fun h pool certificate epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in Copy_certificate_spec.certified_valid_def h certificate epoch depth d; heap_def h epoch depth d;
    Pooled_spec.registered_def pool epoch d; let out = Pooled_spec.registered pool epoch d in listed_def out x;
    let u = () in match d with
    | Clean -> refine_ u
    | Start -> let desc : desc = Bool in let v = cell desc depth in Copy_heap_proofs.put_frame h epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      copy_new_member h pool certificate epoch depth rest x (refine_ u);
      let mid = heap h epoch depth rest in let v = cell desc depth in let h1 = H.put mid q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      Copy_heap_proofs.put_frame mid q v x; Copy_heap_proofs.put_frame h1 p w x; refine_ u
    | Alias (rest, p, q, old) ->
      copy_new_member h pool certificate epoch depth rest x (refine_ u);
      let mid = heap h epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; Copy_heap_proofs.put_frame mid p w x; refine_ u)

let (copy_unlisted @ total) : (h : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d && H.mem (copy_heap h epoch depth d) x
      && Level_unifier_spec.terminal (copy_heap h epoch depth d) x && finite_node (copy_heap h epoch depth d) x && not (listed (Pooled_spec.registered pool epoch d) x)} ->
    {u : unit | H.mem h x && Level_unifier_spec.terminal h x && finite_node h x && not (listed pool x)} @ ghost = fun h pool certificate epoch depth d x premise -> ghost_ (
      let refine_ premise = premise in let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
      let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
      let u = () in result_at h certificate epoch depth d x (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail x; copy_new_member h pool certificate epoch depth d x (refine_ u);
      Pooled_proofs.registered_keeps pool epoch d x; history_at h certificate epoch depth d x (refine_ u);
      Level_unifier_spec.terminal_def h x; Level_unifier_spec.terminal_def after x;
      Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
      finite_node_def h x; finite_node_def after x; refine_ u)
let (allocation_unlisted @ total) : (h : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p v) x && Level_unifier_spec.terminal (H.put h p v) x && finite_node (H.put h p v) x && not (listed (Entry (p, pool)) x)} ->
    {u : unit | H.mem h x && Level_unifier_spec.terminal h x && finite_node h x && not (listed pool x)} @ ghost = fun h pool p v x premise -> ghost_ (
      let refine_ premise = premise in let after = H.put h p v in let out = Entry (p, pool) in listed_def out x;
      Copy_heap_proofs.put_frame h p v x; Level_unifier_spec.terminal_def h x; Level_unifier_spec.terminal_def after x;
      Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
      finite_node_def h x; finite_node_def after x; let u = () in refine_ u)
let (unify_finite_before @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d && H.mem after x && Level_unifier_spec.terminal after x && finite_node after x} ->
    {u : unit | H.mem h x && Level_unifier_spec.terminal h x && finite_node h x} @ ghost = fun h p q ok after d x premise -> ghost_ (
      let refine_ premise = premise in let u = () in Effective_unifier_frame.unified_frame h p q ok after d x (refine_ u);
      Effective_unifier_metadata.cells h p q ok after d x (refine_ u);
      Effective_unifier_metadata.cell_frame_def h after x;
      Effective_unifier_pool.unified_terminal h p q ok after d x (refine_ u);
      Level_unifier_spec.terminal_def h x; Level_unifier_spec.terminal_def after x;
      Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
      finite_node_def h x; finite_node_def after x;
      (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); refine_ u)

let rec (run_unlisted @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && not (result e === None)
      && H.mem after x && Level_unifier_spec.terminal after x && finite_node after x && not (listed final_pool x)} ->
    {u : unit | H.mem h x && Level_unifier_spec.terminal h x && finite_node h x && not (listed pool x)} @ ghost =
  fun h depth pool env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; result_def e;
    let u = () in match e with
    | RShared _ -> refine_ u
    | RVar (i, target, epoch, d, certificate) -> (match lookup env i with None -> refine_ u | Some original ->
      Copy_certificate_spec.certifies_def h certificate epoch depth d original target;
      copy_unlisted h pool certificate epoch depth d x (refine_ u); refine_ u)
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
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in let env1 = Bind (p, env) in
      run_unlisted closed depth transferred env1 body after final_pool x (refine_ u);
      Representative_pool_proofs.transfer_member closed child_pool pool x; Representative_pool_spec.retained_rep_def closed x; Nested_pool_spec.retained_def closed x;
      finite_node_def closed x; at_level_def closed x;
      Representative_pool_spec.close_heap_def middle depth child_pool;
      Representative_level.representatives_scoped middle child_pool (refine_ u);
      let filtered = Representative_level.representatives middle child_pool in
      Generalize_proofs.closed_observe middle depth filtered x (refine_ u);
      closed_at_def middle closed depth filtered x;
      Level_unifier_spec.terminal_def middle x; Level_unifier_spec.terminal_def closed x;
      Level_unifier_spec.observe_def middle x; Level_unifier_spec.observe_def closed x;
      finite_node_def middle x;
      Representative_level.representatives_member middle child_pool x;
      let child_depth = depth + 1 in let empty : pool = Empty in
      run_unlisted h child_depth empty env rhs middle child_pool x (refine_ u); refine_ u))

let rec (registered_member @ total) : (saved : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | Copy_certificate_spec.certified_valid saved certificate epoch depth d && listed (Pooled_spec.registered base epoch d) x} ->
    {u : unit | listed base x || H.mem (heap saved epoch depth d) x} @ ghost = fun saved certificate base epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in Pooled_spec.registered_def base epoch d; Copy_certificate_spec.certified_valid_def saved certificate epoch depth d; heap_def saved epoch depth d;
  let pool = Pooled_spec.registered base epoch d in listed_def pool x; let u = () in match d with
  | Clean -> refine_ u
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid q v x; let h1 = H.put mid q v in put_frame h1 p w x;
    if x === q then refine_ u else (registered_member saved certificate base epoch depth rest x (refine_ u); refine_ u)
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in let v = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid p v x; registered_member saved certificate base epoch depth rest x (refine_ u); refine_ u)
