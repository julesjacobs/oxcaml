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

let[@def] (finite_path @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (r : R.representative @ immutable) = ghost_ (
  resolves h p r.root r.path && finite_node h r.root)

let (fresh @ total) : (h : node Pref.heap) @ immutable -> (depth : int) ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc && (match desc with Link _ -> false | _ -> true)} ->
    {r : R.representative | P.bounded_path (H.put h p (cell desc depth)) p r depth} @ immutable ghost =
  fun h depth p desc premise -> ghost_ (
    allocated_def h depth p desc;
    let v = cell desc depth in cell_def desc depth; let after = H.put h p v in
    let path = Here in let r = {R.root = p; path} in
    P.bounded_path_def after p r depth; resolves_def after p p path;
    terminal_def after p; observe_def after p; below_def after p depth; at_level_def after p;
    r)

let (bounded_finite @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : R.representative) @ immutable -> (bound : int) ->
    {u : unit | P.bounded_path h p r bound} -> {u : unit | finite_path h p r} @ ghost =
  fun h p r bound premise -> ghost_ (
    P.bounded_path_def h p r bound;
    below_def h r.root bound; at_level_def h r.root; finite_node_def h r.root;
    finite_path_def h p r; ())

let (copied @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certifies h certificate epoch depth d p q} ->
    {r : R.representative | finite_path (copy_heap h epoch depth d) q r} @ immutable ghost =
  fun h heads valid certificate epoch depth d p q premise -> ghost_ (
    Copy_certificate_proofs.replay h certificate heads valid epoch depth d p q ();
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
    Effective_copy_spec.effective_target_for_def h heads d p q;
    Effective_copy_heap_proofs.target_allocated h heads epoch depth d p q ();
    Effective_copy_metadata.result_at h heads epoch depth d q ();
    Copy_cleanup_spec.swept_at_def raw after trail q;
    match E.level h heads p with
    | Generic ->
      Effective_copy_heap_proofs.mapped_fresh h heads valid epoch depth d p q ();
      Effective_copy_metadata.fresh_terminal h heads epoch depth d q ();
      let path = Here in let r = {R.root = q; path} in
      resolves_def after q q path; terminal_def raw q; terminal_def after q;
      observe_def raw q; observe_def after q;
      at_level_def raw q; finite_node_def after q; finite_path_def after q r;
      r
    | Finite _ ->
      valid p; E.valid_head_def h heads p; E.level_def h heads p;
      E.head_terminal h heads p ();
      let r = heads p in
      let frame : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem h x) || (H.mem after x
          && observe h x === observe after x)}) @ total = fun x ->
        if H.mem h x then (Effective_copy_metadata.saved_observe h heads epoch depth d x (); ()) else ();
        () in
      Effective_copy_metadata.resolution_grows h after frame p r.root r.path ();
      Effective_copy_heap_proofs.history_at h heads epoch depth d r.root ();
      Effective_copy_metadata.result_at h heads epoch depth d r.root ();
      Copy_cleanup_spec.swept_at_def raw after trail r.root;
      at_level_def h r.root; finite_node_def after r.root; finite_path_def after q r;
      r)

let (copied_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certifies h certificate epoch depth d p q} ->
    {r : R.representative | finite_path (copy_heap h epoch depth d) q r} @ immutable ghost =
  fun h trees certificate epoch depth d p q premise -> ghost_ (
    let raw : (x : node Pref.t) @ immutable total ->
      {r : R.representative | not (H.mem h x) || resolves h x r.root r.path} @ immutable total = fun x ->
        if H.mem h x then (
          let t = trees x in let r = R.from_forest h t () in r)
        else let r = {R.root = x; path = Here} in r in
    let[@def] heads : E.heads = fun x -> let r = raw x in r in
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      heads_def x; let _ = raw x in E.valid_head_def h heads x;
      () in
    let r = copied h heads valid certificate epoch depth d p q () in r)

let rec (run @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {r : R.representative | finite_path after p r} @ immutable ghost =
  fun h trees depth pool env e after final_pool p premise -> ghost_ (
    ran_def h depth pool env e after final_pool; result_def e;
    match e with
    | RShared (_, _, r) -> active_def h r.root; at_level_def h r.root;
      finite_node_def h r.root; finite_path_def after p r; r
    | RVar (i, q, epoch, d, certificate) ->
      (match lookup env i with None -> let r = {R.root = p; path = Here} in r
      | Some original -> let r = copied_forest h trees certificate epoch depth d original q () in r)
    | RBool q | RFalse q -> let desc = Bool in let r = fresh h depth q desc () in
      bounded_finite after q r depth (); r
    | RWord (_, q) -> let desc = Word in let r = fresh h depth q desc () in
      bounded_finite after q r depth (); r
    | RNil (arg, q) ->
      let var = Var in let middle = H.put h arg (cell var depth) in
      let desc = List arg in let r = fresh middle depth q desc () in
      bounded_finite after q r depth (); r
    | RCaseList (_, _, _, body) ->
      run h trees depth pool env body after final_pool p ()
    | RIf (_, _, _, body) ->
      run h trees depth pool env body after final_pool p ()
    | RPrimitive (op, _, _, body, middle, body_pool, out) ->
      (match result body with None -> unreachable_ () | Some _ ->
        match out with None -> unreachable_ () | Some q ->
        let desc = primitive_desc op in
        primitive_desc_def op;
        let r = fresh middle depth q desc () in bounded_finite after q r depth (); r)
    | RLam (arg, body, middle, _, out) ->
      (match result body with None -> let r = {R.root = p; path = Here} in r
      | Some b -> match out with None -> let r = {R.root = p; path = Here} in r
      | Some q -> let desc = Arrow (arg, b) in let r = fresh middle depth q desc () in
        bounded_finite after q r depth (); r)
    | RApp_left _ | RCons_left _ | RApp_right _ | RCons_right _ | RLet_left _ -> let r = {R.root = p; path = Here} in r
    | RCons (left, right, _, _, h2, _, q, ok, d) ->
      (match result left with None -> let r = {R.root = p; path = Here} in r
      | Some f -> match result right with None -> let r = {R.root = p; path = Here} in r
      | Some a ->
        let desc = List f in let h3 = H.put h2 q (cell desc depth) in
        let r = fresh h2 depth q desc () in
        let s = P.unify h3 a q ok after d q r depth () in
        bounded_finite after q s depth (); s)
    | RApp (left, right, _, _, h2, _, q, arrow, ok, d) ->
      (match result left with None -> let r = {R.root = p; path = Here} in r
      | Some f -> match result right with None -> let r = {R.root = p; path = Here} in r
      | Some a ->
        let var = Var in let v = cell var depth in let h3 = H.put h2 q v in
        let desc = Arrow (a, q) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        let r = fresh h2 depth q var () in
        allocated_def h3 depth arrow desc; P.allocate h3 arrow w q r depth ();
        let s = P.unify h4 f arrow ok after d q r depth () in
        bounded_finite after q s depth (); s)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let r = fresh h2 depth self desc () in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in let env3 = Bind (arg, Bind (self, env)) in
      let s = P.run h3 depth pool3 env3 body middle body_pool self r depth () in
      (match result body with None -> s | Some b -> match finish with Aborted -> s
      | Unified (ok, d) -> let t = P.unify middle b res ok after d self s depth () in
        bounded_finite after self t depth (); t)
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      let middle_trees : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.run_forest h trees child_depth empty env rhs middle child_pool x () in t in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      let closed_trees : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.representative_closed_forest middle middle_trees depth child_pool x () in t in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      (match result rhs with None -> let r = {R.root = p; path = Here} in r
      | Some q -> let next_env = Bind (q, env) in
        let r = run closed closed_trees depth transferred next_env body after final_pool p () in r))

let (result_below @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (heads : E.heads) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p
      && Hm_effective_runtime.runtime_at after heads depth final_pool p} ->
    {u : unit | E.effective_below after heads p depth} @ ghost =
  fun h trees depth pool env e after final_pool heads p premise -> ghost_ (
    let r = run h trees depth pool env e after final_pool p () in
    finite_path_def after p r; resolves_def after p r.root r.path;
    Hm_effective_runtime.runtime_at_def after heads depth final_pool p;
    Hm_effective_runtime.safe_def after heads p;
    Hm_effective_runtime.depth_bound_def after heads depth p;
    E.valid_head_def after heads p; E.level_def after heads p;
    let s = heads p in R.unique after p r.root r.path s.root s.path ();
    finite_node_def after r.root; at_level_def after r.root; ())
