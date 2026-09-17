open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
module R = Representative_level
module E = Effective_level
module P = Hm_effective_paths

let[@def] (finite_path @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (r : R.representative @ immutable) = ghost_ (
  resolves h p r.root r.path && finite_node h r.root)

let (fresh @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc && (match desc with Link _ -> false | _ -> true)} ->
    {r : R.representative | P.bounded_path (H.put h p (cell desc depth)) p r depth} @ immutable ghost =
  fun h depth p desc premise -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc;
    let v = cell desc depth in cell_def desc depth; let after = H.put h p v in
    Copy_heap_proofs.put_frame h p v p;
    let path = Here in let r = {R.root = p; path} in
    P.bounded_path_def after p r depth; resolves_def after p p path;
    terminal_def after p; observe_def after p; below_def after p depth; at_level_def after p;
    refine_ r)

let (bounded_finite @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : R.representative) @ immutable -> (bound : int) ->
    {u : unit | P.bounded_path h p r bound} -> {u : unit | finite_path h p r} @ ghost =
  fun h p r bound premise -> ghost_ (
    let refine_ premise = premise in P.bounded_path_def h p r bound;
    below_def h r.root bound; at_level_def h r.root; finite_node_def h r.root;
    finite_path_def h p r; let u = () in refine_ u)

let (copied @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certifies h certificate epoch depth d p q} ->
    {r : R.representative | finite_path (copy_heap h epoch depth d) q r} @ immutable ghost =
  fun h heads valid certificate epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    Copy_certificate_proofs.replay h certificate heads valid epoch depth d p q (refine_ u);
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
    Effective_copy_spec.effective_target_for_def h heads d p q;
    Effective_copy_heap_proofs.target_allocated h heads epoch depth d p q (refine_ u);
    Effective_copy_metadata.result_at h heads epoch depth d q (refine_ u);
    Copy_cleanup_spec.swept_at_def raw after trail q;
    match E.level h heads p with
    | Generic ->
      Effective_copy_heap_proofs.mapped_fresh h heads valid epoch depth d p q (refine_ u);
      Effective_copy_metadata.fresh_terminal h heads epoch depth d q (refine_ u);
      let path = Here in let r = {R.root = q; path} in
      resolves_def after q q path; terminal_def raw q; terminal_def after q;
      observe_def raw q; observe_def after q;
      at_level_def raw q; finite_node_def after q; finite_path_def after q r;
      refine_ r
    | Finite _ ->
      valid p; E.valid_head_def h heads p; E.level_def h heads p;
      E.head_terminal h heads p (refine_ u);
      let r = heads p in
      let frame : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem h x) || (H.mem after x
          && observe h x === observe after x)}) @ total = fun x ->
        let u = () in
        if H.mem h x then (Effective_copy_metadata.saved_observe h heads epoch depth d x (refine_ u); ()) else ();
        refine_ u in
      Effective_copy_metadata.resolution_grows h after frame p r.root r.path (refine_ u);
      Effective_copy_heap_proofs.history_at h heads epoch depth d r.root (refine_ u);
      Effective_copy_metadata.result_at h heads epoch depth d r.root (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail r.root;
      at_level_def h r.root; finite_node_def after r.root; finite_path_def after q r;
      refine_ r)

let (copied_forest @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certifies h certificate epoch depth d p q} ->
    {r : R.representative | finite_path (copy_heap h epoch depth d) q r} @ immutable ghost =
  fun h trees certificate epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in
    let raw : (x : node Pref.t) @ immutable total ->
      {r : R.representative | not (H.mem h x) || resolves h x r.root r.path} @ immutable total = fun x ->
        let u = () in if H.mem h x then (
          let refine_ t = trees x in let refine_ r = R.from_forest h t (refine_ u) in refine_ r)
        else let r = {R.root = x; path = Here} in refine_ r in
    let[@def] heads : E.heads = fun x -> let refine_ r = raw x in r in
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      heads_def x; let refine_ r = raw x in E.valid_head_def h heads x;
      let u = () in refine_ u in
    let u = () in let refine_ r = copied h heads valid certificate epoch depth d p q (refine_ u) in refine_ r)

let rec (run @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {r : R.representative | finite_path after p r} @ immutable ghost =
  fun h trees depth pool env e after final_pool p premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; result_def e;
    let u = () in match e with
    | RShared (_, _, r) -> active_def h r.root; at_level_def h r.root;
      finite_node_def h r.root; finite_path_def after p r; refine_ r
    | RVar (i, q, epoch, d, certificate) ->
      (match lookup env i with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some original -> let refine_ r = copied_forest h trees certificate epoch depth d original q (refine_ u) in refine_ r)
    | RBool q -> let desc = Bool in let refine_ r = fresh h depth q desc (refine_ u) in
      bounded_finite after q r depth (refine_ u); refine_ r
    | RLam (arg, body, middle, _, out) ->
      (match result body with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some b -> match out with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some q -> let desc = Arrow (arg, b) in let refine_ r = fresh middle depth q desc (refine_ u) in
        bounded_finite after q r depth (refine_ u); refine_ r)
    | RApp_left _ | RApp_right _ | RLet_left _ -> let r = {R.root = p; path = Here} in refine_ r
    | RApp (left, right, _, _, h2, _, q, arrow, ok, d) ->
      (match result left with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some f -> match result right with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some a ->
        let var = Var in let v = cell var depth in let h3 = H.put h2 q v in
        let desc = Arrow (a, q) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        let refine_ r = fresh h2 depth q var (refine_ u) in
        allocated_def h3 depth arrow desc; P.allocate h3 arrow w q r depth (refine_ u);
        let refine_ s = P.unify h4 f arrow ok after d q r depth (refine_ u) in
        bounded_finite after q s depth (refine_ u); refine_ s)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let refine_ r = fresh h2 depth self desc (refine_ u) in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in let env3 = Bind (arg, Bind (self, env)) in
      let refine_ s = P.run h3 depth pool3 env3 body middle body_pool self r depth (refine_ u) in
      (match result body with None -> refine_ s | Some b -> match finish with Aborted -> refine_ s
      | Unified (ok, d) -> let refine_ t = P.unify middle b res ok after d self s depth (refine_ u) in
        bounded_finite after self t depth (refine_ u); refine_ t)
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      let middle_trees : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.run_forest h trees child_depth empty env rhs middle child_pool x (refine_ u) in refine_ t in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      let closed_trees : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.representative_closed_forest middle middle_trees depth child_pool x (refine_ u) in refine_ t in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      (match result rhs with None -> let r = {R.root = p; path = Here} in refine_ r
      | Some q -> let next_env = Bind (q, env) in
        let refine_ r = run closed closed_trees depth transferred next_env body after final_pool p (refine_ u) in refine_ r))

let (result_below @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (heads : E.heads) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p
      && Hm_effective_runtime.runtime_at after heads depth final_pool p} ->
    {u : unit | E.effective_below after heads p depth} @ ghost =
  fun h trees depth pool env e after final_pool heads p premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    let refine_ r = run h trees depth pool env e after final_pool p (refine_ u) in
    finite_path_def after p r; resolves_def after p r.root r.path;
    Hm_effective_runtime.runtime_at_def after heads depth final_pool p;
    Hm_effective_runtime.safe_def after heads p;
    Hm_effective_runtime.depth_bound_def after heads depth p;
    E.valid_head_def after heads p; E.level_def after heads p;
    let s = heads p in R.unique after p r.root r.path s.root s.path (refine_ u);
    finite_node_def after r.root; at_level_def after r.root; refine_ u)
