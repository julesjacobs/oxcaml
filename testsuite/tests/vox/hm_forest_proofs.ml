open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Level_unifier_spec
open Level_finite_spec

let (allocated_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc} ->
    ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem (H.put h p (cell desc depth)) x then finite (H.put h p (cell desc depth)) t else observe (H.put h p (cell desc depth)) x === None)} @ immutable) @ total ghost = fun h trees depth p desc premise -> ghost_ (
  let refine_ premise = premise in allocated_def h depth p desc;
  children_below_def h desc depth;
  (match desc with Var | Bool -> () | Link q -> below_def h q depth; ()
  | Arrow (a, b) -> below_def h a depth; below_def h b depth; ());
  let v = cell desc depth in cell_def desc depth; allocatable_def h v;
  let out : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem (H.put h p (cell desc depth)) x then finite (H.put h p (cell desc depth)) t
      else observe (H.put h p (cell desc depth)) x === None)} @ immutable) @ total = fun x ->
    let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h trees p v x (refine_ u) in refine_ t in out)

let (clean_copy_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid h epoch depth d} ->
    {t : tree | tree_root t === x && (if H.mem (copy_heap h epoch depth d) x
      then finite (copy_heap h epoch depth d) t else observe (copy_heap h epoch depth d) x === None)} @ immutable ghost =
  fun h trees epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in copy_heap_def h epoch depth d;
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | H.mem raw y === H.mem after y && observe raw y === observe after y}) @ total = fun y ->
      let u = () in Clean_copy.result_at h epoch depth d y (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail y;
      observe_def raw y; observe_def after y; refine_ u in
    let u = () in let refine_ t = Forest_transport.copy_forest_at h trees epoch depth d x (refine_ u) in
    frame x; if H.mem raw x then (Forest_transport.finite_frame raw after frame t (refine_ u); refine_ t)
    else refine_ t)

let rec (run_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable ghost =
  fun h trees depth pool env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
    let u = () in match e with
    | RShared _ -> let refine_ t = trees x in refine_ t
    | RVar (i, _, epoch, d) -> (match lookup env i with
      | None -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Some _ -> let refine_ t = clean_copy_forest h trees epoch depth d x (refine_ u) in refine_ t)
    | RBool p -> let desc : desc = Bool in
      let ts = allocated_forest h trees depth p desc (refine_ u) in let refine_ t = ts x in refine_ t
    | RApp_left (left, _) -> let refine_ t = run_forest h trees depth pool env left after final_pool x (refine_ u) in refine_ t
    | RLet_left (rhs, _) -> let empty : pool = Empty in let child_depth = depth + 1 in
      let refine_ t = run_forest h trees child_depth empty env rhs after final_pool x (refine_ u) in refine_ t
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let h1 = H.put h arg (cell var depth) in
      let ts1 = allocated_forest h trees depth arg var (refine_ u) in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h1 (refine_ ts1) depth pool1 env1 body middle body_pool x (refine_ u) in refine_ t in
      (match result body with None -> let refine_ t = ts2 x in refine_ t | Some b -> match out with
      | None -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Some p -> let desc = Arrow (arg, b) in
        let ts3 = allocated_forest middle (refine_ ts2) depth p desc (refine_ u) in let refine_ t = ts3 x in refine_ t)
    | RApp_right (left, right, middle, left_pool) ->
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h trees depth pool env left middle left_pool x (refine_ u) in refine_ t in
      let refine_ t = run_forest middle (refine_ ts1) depth left_pool env right after final_pool x (refine_ u) in refine_ t
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h trees depth pool env left h1 pool1 x (refine_ u) in refine_ t in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h1 (refine_ ts1) depth pool1 env right h2 pool2 x (refine_ u) in refine_ t in
      (match result left with None -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Some f -> match result right with None -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Some a -> let var : desc = Var in let h3 = H.put h2 p (cell var depth) in
        let ts3 = allocated_forest h2 (refine_ ts2) depth p var (refine_ u) in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in
        let ts4 = allocated_forest h3 (refine_ ts3) depth arrow desc (refine_ u) in
        let refine_ t = Optimized_finite_proofs.unified_finite_at h4 (refine_ ts4) f arrow ok after d x (refine_ u) in refine_ t)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let h1 = H.put h arg (cell var depth) in
      let ts1 = allocated_forest h trees depth arg var (refine_ u) in
      let h2 = H.put h1 res (cell var depth) in
      let ts2 = allocated_forest h1 (refine_ ts1) depth res var (refine_ u) in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in
      let ts3 = allocated_forest h2 (refine_ ts2) depth self desc (refine_ u) in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in
      let env3 = Bind (arg, Bind (self, env)) in
    let ts4 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h3 (refine_ ts3) depth pool3 env3 body middle body_pool x (refine_ u) in refine_ t in
      (match result body with None -> let refine_ t = ts4 x in refine_ t | Some b -> match finish with
      | Aborted -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Unified (ok, d) -> let refine_ t = Optimized_finite_proofs.unified_finite_at middle (refine_ ts4) b res ok after d x (refine_ u) in refine_ t)
    | RLet (rhs, body, middle, child_pool) ->
      let empty : pool = Empty in let child_depth = depth + 1 in
    let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = run_forest h trees child_depth empty env rhs middle child_pool x (refine_ u) in refine_ t in
      let closed = closed_heap middle depth child_pool in
    let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = Forest_transport.closed_forest_at middle (refine_ ts1) depth child_pool x (refine_ u) in refine_ t in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in
      (match result rhs with None -> let _impossible : {u : unit | false} = refine_ u in let refine_ t = trees x in refine_ t
      | Some p -> let env1 = Bind (p, env) in
        let refine_ t = run_forest closed (refine_ ts2) depth transferred env1 body after final_pool x (refine_ u) in refine_ t))

let (closed_forest @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable ghost =
  fun e after pool x premise -> ghost_ (
    let refine_ premise = premise in let h = H.empty () in
    let trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total = fun x ->
      let t = Free x in tree_root_def t; observe_def h x; refine_ t in
    let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
    let u = () in let refine_ t = run_forest h trees 0 empty env e after pool x (refine_ u) in refine_ t)
