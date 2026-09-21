open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
open Hm_effective_runtime
module E = Effective_level
module R = Representative_level
module A = Hm_effective_allocation
module F = Hm_effective_forest

let rec (run_invariant @ total) : (h : Pref.heap) @ immutable ->
    (a : E.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h a depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (b : E.heads) @ total ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head after b x})) @ total ->
    (final_pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | safe after b x && (result e === None || runtime_at after b depth final_pool x)} @ ghost =
  fun h a trees depth pool facts env e after b vb final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool; result_def e;
    let va : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h a y}) @ total = fun y ->
      facts y; runtime_at_def h a depth pool y; safe_def h a y; () in
    match e with
    | RShared _ -> facts x; rebase h a b va (refine_ vb) depth pool x ();
      runtime_at_def after b depth final_pool x; ()
    | RVar (i, target, epoch, d, certificate) ->
      (match lookup env i with None -> () | Some original ->
        Copy_certificate_proofs.replay h certificate a va epoch depth d original target ();
        copy_heap_def h epoch depth d;
        let raw = heap h epoch depth d in let trail = Pooled_spec.touched d in
        let frame : ((y : node Pref.t) @ immutable ->
          {u : unit | H.mem raw y === H.mem after y && observe raw y === observe after y}) @ total = fun y ->
          Effective_copy_metadata.result_at h a epoch depth d y ();
          Copy_cleanup_spec.swept_at_def raw after trail y; observe_def raw y; observe_def after y; () in
        let raw_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head raw b y}) @ total = fun y ->
          vb y; E.valid_head_def after b y; E.valid_head_def raw b y;
          frame y; let r = b y in R.resolution_frame raw after frame y r.root r.path;
          () in
        Hm_effective_copy_runtime.copy_runtime h a b depth pool facts epoch d (refine_ raw_valid) x ();
        runtime_at_def after b depth final_pool x; ())
    | RBool p -> let desc = Bool in allocated_def h depth p desc;
      facts x; A.allocate_runtime h a b depth pool p desc va (refine_ vb) x ();
      runtime_at_def after b depth final_pool x; ()
    | RApp_left (left, _) ->
      run_invariant h a trees depth pool facts env left after b vb final_pool x (); ()
    | RLam (arg, body, middle, body_pool, out) ->
      let var = Var in
      allocated_def h depth arg var;
      let h1 = H.put h arg (cell var depth) in let h1_pool = Entry (arg, pool) in
      let h1_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else observe h1 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h trees depth arg var () in
        let t = out y in t in
      let[@def] h1_heads : E.heads = fun y -> let r = Forest_heads.select h1 h1_trees y in r in
      let h1_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 h1_heads y}) @ total = fun y ->
        h1_heads_def y; let _r = Forest_heads.select h1 h1_trees y in
        E.valid_head_def h1 h1_heads y; () in
      let h1_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 h1_heads depth h1_pool y}) @ total = fun y ->
        facts y; A.allocate_runtime h a h1_heads depth pool arg var va (refine_ h1_valid) y (); () in
      let next_env = Bind (arg, env) in
      (match result body with None ->
        run_invariant h1 h1_heads h1_trees depth h1_pool h1_facts next_env body after b vb final_pool x (); ()
      | Some target -> match out with None -> () | Some p ->
      let middle_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else observe middle y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h1 h1_trees depth h1_pool next_env body middle body_pool y () in t in
      let[@def] middle_heads : E.heads = fun y -> let r = Forest_heads.select middle middle_trees y in r in
      let middle_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads y}) @ total = fun y ->
        middle_heads_def y; let _r = Forest_heads.select middle middle_trees y in
        E.valid_head_def middle middle_heads y; () in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads depth body_pool y}) @ total = fun y ->
        run_invariant h1 h1_heads h1_trees depth h1_pool h1_facts next_env body middle middle_heads middle_valid body_pool y (); () in
      middle_facts target; Hm_effective_result.result_below h1 h1_trees depth h1_pool next_env body middle body_pool middle_heads target ();
      h1_valid arg; A.allocated_below h depth arg var h1_heads ();
      middle_valid arg;
      Hm_effective_paths.run_below h1 h1_heads middle_heads depth h1_pool next_env body middle body_pool arg depth ();
      let desc = Arrow (arg, target) in allocated_def middle depth p desc;
      middle_facts x; A.allocate_runtime middle middle_heads b depth body_pool p desc middle_valid (refine_ vb) x ();
      runtime_at_def after b depth final_pool x; ())
    | RApp_right (left, right, h1, pool1) ->
      let h1_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else observe h1 y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h trees depth pool env left h1 pool1 y () in t in
      let[@def] h1_heads : E.heads = fun y -> let r = Forest_heads.select h1 h1_trees y in r in
      let h1_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 h1_heads y}) @ total = fun y ->
        h1_heads_def y; let _r = Forest_heads.select h1 h1_trees y in
        E.valid_head_def h1 h1_heads y; () in
      let h1_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 h1_heads depth pool1 y}) @ total = fun y ->
        run_invariant h a trees depth pool facts env left h1 h1_heads h1_valid pool1 y (); () in
      run_invariant h1 h1_heads h1_trees depth pool1 h1_facts env right after b vb final_pool x (); ()
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let h1_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else observe h1 y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h trees depth pool env left h1 pool1 y () in t in
      let[@def] h1_heads : E.heads = fun y -> let r = Forest_heads.select h1 h1_trees y in r in
      let h1_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 h1_heads y}) @ total = fun y ->
        h1_heads_def y; let _r = Forest_heads.select h1 h1_trees y in
        E.valid_head_def h1 h1_heads y; () in
      let h1_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 h1_heads depth pool1 y}) @ total = fun y ->
        run_invariant h a trees depth pool facts env left h1 h1_heads h1_valid pool1 y (); () in
      let h2_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h2 y then finite h2 t else observe h2 y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h1 h1_trees depth pool1 env right h2 pool2 y () in t in
      let[@def] h2_heads : E.heads = fun y -> let r = Forest_heads.select h2 h2_trees y in r in
      let h2_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h2 h2_heads y}) @ total = fun y ->
        h2_heads_def y; let _r = Forest_heads.select h2 h2_trees y in
        E.valid_head_def h2 h2_heads y; () in
      let h2_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 h2_heads depth pool2 y}) @ total = fun y ->
        run_invariant h1 h1_heads h1_trees depth pool1 h1_facts env right h2 h2_heads h2_valid pool2 y (); () in
      (match result left with None -> () | Some f -> match result right with None -> () | Some target ->
      h2_facts target; Hm_effective_result.result_below h1 h1_trees depth pool1 env right h2 pool2 h2_heads target ();
      let var = Var in
      allocated_def h2 depth p var;
      let h3 = H.put h2 p (cell var depth) in let h3_pool = Entry (p, pool2) in
      let h3_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h3 y then finite h3 t else observe h3 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h2 h2_trees depth p var () in
        let t = out y in t in
      let[@def] h3_heads : E.heads = fun y -> let r = Forest_heads.select h3 h3_trees y in r in
      let h3_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h3 h3_heads y}) @ total = fun y ->
        h3_heads_def y; let _r = Forest_heads.select h3 h3_trees y in
        E.valid_head_def h3 h3_heads y; () in
      let h3_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 h3_heads depth h3_pool y}) @ total = fun y ->
        h2_facts y; A.allocate_runtime h2 h2_heads h3_heads depth pool2 p var h2_valid (refine_ h3_valid) y (); () in
      h2_valid target; h3_valid target;
      let v = cell var depth in A.saved_below h2 h2_heads h3_heads p v target depth ();
      h3_valid p; A.allocated_below h2 depth p var h3_heads ();
      let desc = Arrow (target, p) in
      allocated_def h3 depth arrow desc;
      let h4 = H.put h3 arrow (cell desc depth) in let h4_pool = Entry (arrow, h3_pool) in
      let h4_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h4 y then finite h4 t else observe h4 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h3 h3_trees depth arrow desc () in
        let t = out y in t in
      let[@def] h4_heads : E.heads = fun y -> let r = Forest_heads.select h4 h4_trees y in r in
      let h4_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h4 h4_heads y}) @ total = fun y ->
        h4_heads_def y; let _r = Forest_heads.select h4 h4_trees y in
        E.valid_head_def h4 h4_heads y; () in
      let h4_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h4 h4_heads depth h4_pool y}) @ total = fun y ->
        h3_facts y; A.allocate_runtime h3 h3_heads h4_heads depth h3_pool arrow desc h3_valid (refine_ h4_valid) y (); () in
      unify_runtime h4 h4_heads b depth h4_pool h4_facts f arrow ok after d vb h4_trees x ();
      runtime_at_def after b depth final_pool x; ())
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var = Var in
      allocated_def h depth arg var;
      let h1 = H.put h arg (cell var depth) in let h1_pool = Entry (arg, pool) in
      let h1_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else observe h1 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h trees depth arg var () in
        let t = out y in t in
      let[@def] h1_heads : E.heads = fun y -> let r = Forest_heads.select h1 h1_trees y in r in
      let h1_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 h1_heads y}) @ total = fun y ->
        h1_heads_def y; let _r = Forest_heads.select h1 h1_trees y in
        E.valid_head_def h1 h1_heads y; () in
      let h1_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 h1_heads depth h1_pool y}) @ total = fun y ->
        facts y; A.allocate_runtime h a h1_heads depth pool arg var va (refine_ h1_valid) y (); () in
      allocated_def h1 depth res var;
      let h2 = H.put h1 res (cell var depth) in let h2_pool = Entry (res, h1_pool) in
      let h2_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h2 y then finite h2 t else observe h2 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h1 h1_trees depth res var () in
        let t = out y in t in
      let[@def] h2_heads : E.heads = fun y -> let r = Forest_heads.select h2 h2_trees y in r in
      let h2_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h2 h2_heads y}) @ total = fun y ->
        h2_heads_def y; let _r = Forest_heads.select h2 h2_trees y in
        E.valid_head_def h2 h2_heads y; () in
      let h2_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 h2_heads depth h2_pool y}) @ total = fun y ->
        h1_facts y; A.allocate_runtime h1 h1_heads h2_heads depth h1_pool res var h1_valid (refine_ h2_valid) y (); () in
      h1_valid arg; A.allocated_below h depth arg var h1_heads ();
      h2_valid arg; let v = cell var depth in A.saved_below h1 h1_heads h2_heads res v arg depth ();
      h2_valid res; A.allocated_below h1 depth res var h2_heads ();
      let desc = Arrow (arg, res) in
      allocated_def h2 depth self desc;
      let h3 = H.put h2 self (cell desc depth) in let h3_pool = Entry (self, h2_pool) in
      let h3_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h3 y then finite h3 t else observe h3 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h2 h2_trees depth self desc () in
        let t = out y in t in
      let[@def] h3_heads : E.heads = fun y -> let r = Forest_heads.select h3 h3_trees y in r in
      let h3_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head h3 h3_heads y}) @ total = fun y ->
        h3_heads_def y; let _r = Forest_heads.select h3 h3_trees y in
        E.valid_head_def h3 h3_heads y; () in
      let h3_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 h3_heads depth h3_pool y}) @ total = fun y ->
        h2_facts y; A.allocate_runtime h2 h2_heads h3_heads depth h2_pool self desc h2_valid (refine_ h3_valid) y (); () in
      let next_env = Bind (arg, Bind (self, env)) in
      (match result body with None ->
        run_invariant h3 h3_heads h3_trees depth h3_pool h3_facts next_env body after b vb final_pool x (); ()
      | Some target -> match finish with Aborted -> () | Unified (ok, d) ->
      let middle_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else observe middle y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h3 h3_trees depth h3_pool next_env body middle body_pool y () in t in
      let[@def] middle_heads : E.heads = fun y -> let r = Forest_heads.select middle middle_trees y in r in
      let middle_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads y}) @ total = fun y ->
        middle_heads_def y; let _r = Forest_heads.select middle middle_trees y in
        E.valid_head_def middle middle_heads y; () in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads depth body_pool y}) @ total = fun y ->
        run_invariant h3 h3_heads h3_trees depth h3_pool h3_facts next_env body middle middle_heads middle_valid body_pool y (); () in
      unify_runtime middle middle_heads b depth body_pool middle_facts target res ok after d vb middle_trees x ();
      runtime_at_def after b depth final_pool x; ())
    | RLet_left (rhs, _) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h a child_depth empty y}) @ total = fun y ->
        facts y; enter_runtime h a depth pool y (); () in
      run_invariant h a trees child_depth empty child_facts env rhs after b vb final_pool x (); ()
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h a child_depth empty y}) @ total = fun y ->
        facts y; enter_runtime h a depth pool y (); () in
      let middle_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else observe middle y === None)} @ immutable) @ total = fun y ->
        let t = F.run_forest h trees child_depth empty env rhs middle child_pool y () in t in
      let[@def] middle_heads : E.heads = fun y -> let r = Forest_heads.select middle middle_trees y in r in
      let middle_valid : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads y}) @ total = fun y ->
        middle_heads_def y; let _r = Forest_heads.select middle middle_trees y in
        E.valid_head_def middle middle_heads y; () in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads child_depth child_pool y}) @ total = fun y ->
        run_invariant h a trees child_depth empty child_facts env rhs middle middle_heads middle_valid child_pool y (); () in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      let closed_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem closed y then finite closed t else observe closed y === None)} @ immutable) @ total = fun y ->
        let t = F.representative_closed_forest middle middle_trees depth child_pool y () in t in
      let closed_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at closed middle_heads depth transferred y}) @ total = fun y ->
        Hm_effective_closing.close_after_run h a middle_heads depth pool facts env rhs middle child_pool (refine_ middle_facts) y (); () in
      (match result rhs with None -> () | Some p -> let next_env = Bind (p, env) in
        run_invariant closed middle_heads closed_trees depth transferred closed_facts next_env body after b vb final_pool x (); ())
      )
