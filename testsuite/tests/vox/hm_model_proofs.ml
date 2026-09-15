open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Level_finite_spec

let rec (unify_restrict @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | node_equation h rho x} @ ghost = fun h rho p q ok after d model x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d;
    let u = () in match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> model x; refine_ u
    | Bind_left _ -> let v = redirect h p q in observe_write h p v x;
      model x; node_equation_def after rho x; node_equation_def h rho x; refine_ u
    | Bind_right _ -> let v = redirect h q p in observe_write h q v x;
      model x; node_equation_def after rho x; node_equation_def h rho x; refine_ u
    | Scanned (needle, marks, rest) -> let mid = scan_heap h marks in
      unify_restrict mid rho p q ok after rest model x (refine_ u);
      Marked_occurs_proofs.scan_equation h needle marks rho x (refine_ u); refine_ u
    | Lowering (bound, edits, _, rest) -> let mid = lower_heap h bound edits in
      unify_restrict mid rho p q ok after rest model x (refine_ u);
      lower_equation h bound edits rho x (refine_ u); refine_ u
    | Swap rest -> unify_restrict h rho q p ok after rest model x (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) -> unify_restrict h rho r s ok after rest model x (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      if left_ok then (
        let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          let u = () in let refine_ u = unify_restrict middle rho b e ok after right model x (refine_ u) in refine_ u in
        unify_restrict h rho a c left_ok middle left mid_model x (refine_ u); refine_ u)
      else (unify_restrict h rho a c left_ok middle left (refine_ model) x (refine_ u); refine_ u))

let (allocation_restrict @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p)
      && (H.mem h x || observe h x === None) && node_equation (H.put h p v) rho x} ->
    {u : unit | node_equation h rho x} @ ghost = fun h p v rho x premise -> ghost_ (
      let refine_ premise = premise in observe_write h p v x;
      let after = H.put h p v in node_equation_def after rho x; node_equation_def h rho x;
      let u = () in refine_ u)

let (copy_restrict @ total) : (h : Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && (H.mem h x || observe h x === None)
      && node_equation (copy_heap h epoch depth d) rho x} ->
    {u : unit | node_equation h rho x} @ ghost = fun h epoch depth d rho x premise -> ghost_ (
      let refine_ premise = premise in let raw = heap h epoch depth d in
      let after = copy_heap h epoch depth d in let trail = Pooled_spec.touched d in
      copy_heap_def h epoch depth d; let u = () in Clean_copy.result_at h epoch depth d x (refine_ u);
      Copy_cleanup_spec.swept_at_def raw after trail x;
      Copy_heap_proofs.history_at h epoch depth d x (refine_ u);
      node_equation_def after rho x; node_equation_def h rho x;
      observe_def after x; observe_def h x; refine_ u)

let rec (run_restrict @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | node_equation h rho x} @ ghost =
  fun h trees depth pool env e after final_pool rho model x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
    let refine_ old = trees x in let u = () in match e with
    | RVar (i, _, epoch, d) -> (match lookup env i with None -> refine_ u | Some _ ->
      model x; copy_restrict h epoch depth d rho x (refine_ u); refine_ u)
    | RBool p -> let desc : desc = Bool in let v = cell desc depth in
      allocated_def h depth p desc; model x; allocation_restrict h p v rho x (refine_ u); refine_ u
    | RApp_left (left, _) -> run_restrict h trees depth pool env left after final_pool rho model x (refine_ u); refine_ u
    | RLet_left (rhs, _) -> let empty : pool = Empty in let child_depth = depth + 1 in
      run_restrict h trees child_depth empty env rhs after final_pool rho model x (refine_ u); refine_ u
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      allocated_def h depth arg var; cell_def var depth; allocatable_def h v;
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h1 ts1 depth pool1 env1 body middle body_pool x (refine_ u) in refine_ t in
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in (match result body with None -> model x; () | Some b -> match out with None -> ()
          | Some p -> let desc = Arrow (arg, b) in let w = cell desc depth in
            allocated_def middle depth p desc; let refine_ t = ts2 x in model x;
            allocation_restrict middle p w rho x (refine_ u); ()); refine_ u in
      run_restrict h1 ts1 depth pool1 env1 body middle body_pool rho mid_model x (refine_ u);
      allocation_restrict h arg v rho x (refine_ u); refine_ u
    | RApp_right (left, right, h1, pool1) ->
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h trees depth pool env left h1 pool1 x (refine_ u) in refine_ t in
      let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
        let u = () in run_restrict h1 ts1 depth pool1 env right after final_pool rho model x (refine_ u); refine_ u in
      run_restrict h trees depth pool env left h1 pool1 rho model1 x (refine_ u); refine_ u
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h trees depth pool env left h1 pool1 x (refine_ u) in refine_ t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h1 ts1 depth pool1 env right h2 pool2 x (refine_ u) in refine_ t in
      (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u
      | Some a -> let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        cell_def var depth; allocatable_def h2 v;
      let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h2 ts2 p v x (refine_ u) in refine_ t in
      let model4 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h4 rho x}) @ total = fun x ->
        let u = () in unify_restrict h4 rho f arrow ok after d model x (refine_ u); refine_ u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in let refine_ t = ts3 x in model4 x; allocation_restrict h3 arrow w rho x (refine_ u); refine_ u in
      let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
        let u = () in let refine_ t = ts2 x in model3 x; allocation_restrict h2 p v rho x (refine_ u); refine_ u in
      let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
        let u = () in run_restrict h1 ts1 depth pool1 env right h2 pool2 rho model2 x (refine_ u); refine_ u in
        run_restrict h trees depth pool env left h1 pool1 rho model1 x (refine_ u); refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let h2 = H.put h1 res v in let desc = Arrow (arg, res) in let w = cell desc depth in
      let h3 = H.put h2 self w in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      cell_def var depth; allocatable_def h v; allocatable_def h1 v;
      cell_def desc depth; allocatable_def h2 w; children_below_def h2 desc depth;
      below_def h2 arg depth; below_def h2 res depth;
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h1 ts1 res v x (refine_ u) in refine_ t in
      let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h2 ts2 self w x (refine_ u) in refine_ t in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in
      let env3 = Bind (arg, Bind (self, env)) in
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in (match result body with None -> model x; () | Some b -> match finish with Aborted -> ()
        | Unified (ok, d) -> unify_restrict middle rho b res ok after d model x (refine_ u); ()); refine_ u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in run_restrict h3 ts3 depth pool3 env3 body middle body_pool rho mid_model x (refine_ u); refine_ u in
      let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
        let u = () in let refine_ t = ts2 x in model3 x; allocation_restrict h2 self w rho x (refine_ u); refine_ u in
      let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
        let u = () in let refine_ t = ts1 x in model2 x; allocation_restrict h1 res v rho x (refine_ u); refine_ u in
      model1 x; allocation_restrict h arg v rho x (refine_ u); refine_ u
    | RLet (rhs, body, middle, child_pool) ->
      let empty : pool = Empty in let child_depth = depth + 1 in
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h trees child_depth empty env rhs middle child_pool x (refine_ u) in refine_ t in
      (match result rhs with None -> refine_ u | Some p ->
      let closed = closed_heap middle depth child_pool in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in let env1 = Bind (p, env) in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Forest_transport.closed_forest_at middle ts1 depth child_pool x (refine_ u) in refine_ t in
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in run_restrict closed ts2 depth transferred env1 body after final_pool rho model x (refine_ u);
        Generalize_proofs.closed_model middle depth child_pool rho x (refine_ u);
        equation_def middle rho x; equation_def closed rho x; observe_def middle x; observe_def closed x;
        node_equation_def middle rho x; node_equation_def closed rho x; refine_ u in
      run_restrict h trees child_depth empty env rhs middle child_pool rho mid_model x (refine_ u); refine_ u))
