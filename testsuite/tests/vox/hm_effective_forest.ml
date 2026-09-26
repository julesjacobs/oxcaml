open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
open Level_unifier_spec
open Level_finite_spec

let (allocated_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc} ->
    ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem (H.put h p (cell desc depth)) x then finite (H.put h p (cell desc depth)) t else observe (H.put h p (cell desc depth)) x === None)} @ immutable) @ total ghost = fun h trees depth p desc premise -> ghost_ (
  allocated_def h depth p desc;

  let v = cell desc depth in cell_def desc depth; payload_scoped_def h v; allocatable_def h v;
  let out : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem (H.put h p (cell desc depth)) x then finite (H.put h p (cell desc depth)) t
      else observe (H.put h p (cell desc depth)) x === None)} @ immutable) @ total = fun x ->
    let refine_ t = Level_finite_proofs.allocation_finite_at h trees p v x () in refine_ t in out)

let (clean_copy_forest @ total) : (h : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | Effective_copy_spec.effective_valid h heads epoch depth d} ->
    {t : tree | tree_root t === x && (if H.mem (copy_heap h epoch depth d) x
      then finite (copy_heap h epoch depth d) t else observe (copy_heap h epoch depth d) x === None)} @ immutable ghost =
  fun h heads trees epoch depth d x premise -> ghost_ (
    copy_heap_def h epoch depth d;
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | H.mem raw y === H.mem after y && observe raw y === observe after y}) @ total = fun y ->
      Effective_copy_metadata.result_at h heads epoch depth d y ();
      Copy_cleanup_spec.swept_at_def raw after trail y;
      observe_def raw y; observe_def after y; () in
    let refine_ t = Effective_copy_finite.copy_forest_at h heads trees epoch depth d x () in
    frame x; if H.mem raw x then (Forest_transport.finite_frame raw after frame t (); refine_ t)
    else refine_ t)

let (representative_closed_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool} ->
    {t : tree | tree_root t === x &&
      (if H.mem (Representative_pool_spec.close_heap h depth pool) x
       then finite (Representative_pool_spec.close_heap h depth pool) t
       else observe (Representative_pool_spec.close_heap h depth pool) x === None)} @ immutable ghost =
  fun h trees depth pool x premise -> ghost_ (
    Representative_pool_spec.close_heap_def h depth pool;
    Representative_level.representatives_scoped h pool ();
    let filtered = Representative_level.representatives h pool in
    let refine_ t = Forest_transport.closed_forest_at h trees depth filtered x () in refine_ t)

let rec (run_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable ghost =
  fun h trees depth pool env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    match e with
    | RShared _ -> let refine_ t = trees x in refine_ t
    | RVar (i, p, epoch, d, certificate) -> (match lookup env i with
      | None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some original ->
        let raw : (y : node Pref.t) @ immutable total ->
            {r : Representative_level.representative | not (H.mem h y) || resolves h y r.root r.path} @ immutable total =
          fun y -> if H.mem h y then (
            let refine_ t = trees y in let refine_ r = Representative_level.from_forest h t () in refine_ r)
          else let r = {Representative_level.root = y; path = Here} in refine_ r in
        let[@def] heads : Effective_level.heads = fun y -> let refine_ r = raw y in r in
        let witness : ((y : node Pref.t) @ immutable ->
            {u : unit | Effective_level.valid_head h heads y}) @ total = fun y ->
          heads_def y; let refine_ r = raw y in Effective_level.valid_head_def h heads y;
          () in
        Copy_certificate_proofs.replay h certificate heads witness epoch depth d original p ();
        let refine_ t = clean_copy_forest h heads trees epoch depth d x () in refine_ t)
    | RBool p | RFalse p -> let desc : desc = Bool in
      let ts = allocated_forest h trees depth p desc () in let refine_ t = ts x in refine_ t
    | RWord (_, p) -> let desc : desc = Word in
      let ts = allocated_forest h trees depth p desc () in let refine_ t = ts x in refine_ t
    | RApp_left (left, _) | RCons_left (left, _) -> let refine_ t = run_forest h trees depth pool env left after final_pool x () in refine_ t
    | RLet_left (rhs, _) -> let empty : pool = Empty in let child_depth = depth + 1 in
      let refine_ t = run_forest h trees child_depth empty env rhs after final_pool x () in refine_ t
    | RNil (arg, p) ->
      let var = Var in let middle = H.put h arg (cell var depth) in
      let ts1 = allocated_forest h trees depth arg var () in
      let desc = List arg in let ts2 = allocated_forest middle (refine_ ts1) depth p desc () in
      let refine_ t = ts2 x in refine_ t
    | RCaseList (_, _, _, body) ->
      let refine_ t = run_forest h trees depth pool env body after final_pool x () in refine_ t
    | RIf (_, _, _, body) ->
      let refine_ t = run_forest h trees depth pool env body after final_pool x () in refine_ t
    | RPrimitive (op, _, _, body, middle, body_pool, out) ->
      let ts : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let refine_ t = run_forest h trees depth pool env body middle body_pool x () in refine_ t in
      (match result body with None -> let refine_ t = ts x in refine_ t
      | Some _ -> match out with None -> unreachable_ () | Some p ->
        let desc = primitive_desc op in let next = allocated_forest middle ts depth p desc () in
        let refine_ t = next x in refine_ t)
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let h1 = H.put h arg (cell var depth) in
      let ts1 = allocated_forest h trees depth arg var () in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h1 (refine_ ts1) depth pool1 env1 body middle body_pool x () in refine_ t in
      (match result body with None -> let refine_ t = ts2 x in refine_ t | Some b -> match out with
      | None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some p -> let desc = Arrow (arg, b) in
        let ts3 = allocated_forest middle (refine_ ts2) depth p desc () in let refine_ t = ts3 x in refine_ t)
    | RApp_right (left, right, middle, left_pool) | RCons_right (left, right, middle, left_pool) ->
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h trees depth pool env left middle left_pool x () in refine_ t in
      let refine_ t = run_forest middle (refine_ ts1) depth left_pool env right after final_pool x () in refine_ t
    | RCons (left, right, h1, pool1, h2, pool2, p, ok, d) ->
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h trees depth pool env left h1 pool1 x () in refine_ t in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h1 (refine_ ts1) depth pool1 env right h2 pool2 x () in refine_ t in
      (match result left with None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some f -> match result right with None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some a -> let desc = List f in let h3 = H.put h2 p (cell desc depth) in
        let ts3 = allocated_forest h2 (refine_ ts2) depth p desc () in
        let refine_ t = Effective_unifier_finite.unified_finite_at h3 (refine_ ts3) a p ok after d x () in refine_ t)
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h trees depth pool env left h1 pool1 x () in refine_ t in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h1 (refine_ ts1) depth pool1 env right h2 pool2 x () in refine_ t in
      (match result left with None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some f -> match result right with None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some a -> let var : desc = Var in let h3 = H.put h2 p (cell var depth) in
        let ts3 = allocated_forest h2 (refine_ ts2) depth p var () in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in
        let ts4 = allocated_forest h3 (refine_ ts3) depth arrow desc () in
        let refine_ t = Effective_unifier_finite.unified_finite_at h4 (refine_ ts4) f arrow ok after d x () in refine_ t)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let h1 = H.put h arg (cell var depth) in
      let ts1 = allocated_forest h trees depth arg var () in
      let h2 = H.put h1 res (cell var depth) in
      let ts2 = allocated_forest h1 (refine_ ts1) depth res var () in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let ts3 = allocated_forest h2 (refine_ ts2) depth self desc () in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in
      let env3 = Bind (arg, Bind (self, env)) in
    let ts4 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h3 (refine_ ts3) depth pool3 env3 body middle body_pool x () in refine_ t in
      (match result body with None -> let refine_ t = ts4 x in refine_ t | Some b -> match finish with
      | Aborted -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Unified (ok, d) -> let refine_ t = Effective_unifier_finite.unified_finite_at middle (refine_ ts4) b res ok after d x () in refine_ t)
    | RLet (rhs, body, middle, child_pool) ->
      let empty : pool = Empty in let child_depth = depth + 1 in
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let refine_ t = run_forest h trees child_depth empty env rhs middle child_pool x () in refine_ t in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
      let refine_ t = representative_closed_forest middle (refine_ ts1) depth child_pool x () in refine_ t in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      (match result rhs with None -> let _impossible : {u : unit | false} = () in let refine_ t = trees x in refine_ t
      | Some p -> let env1 = Bind (p, env) in
        let refine_ t = run_forest closed (refine_ ts2) depth transferred env1 body after final_pool x () in refine_ t))

let (closed_forest @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable ghost =
  fun e after pool x premise -> ghost_ (
    let h = H.empty () in
    let trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total = fun x ->
      let t = Free x in tree_root_def t; observe_def h x; refine_ t in
    let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
    let refine_ t = run_forest h trees 0 empty env e after pool x () in refine_ t)
