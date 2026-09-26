open Copy_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module F = Level_finite_spec
module U = Level_unifier_spec
module G = Hm_effective_forest
module M = Hm_effective_membership
module P = Hm_effective_driver_proofs
module R = Representative_pool_spec

let (closing_extends @ total) :
    (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | H.mem h x === H.mem (R.close_heap h depth pool) x} @ ghost =
  fun h depth pool x premise -> ghost_ (
    R.close_heap_def h depth pool;
    Representative_level.representatives_scoped h pool ();
    let filtered = Representative_level.representatives h pool in
    Generalize_proofs.closed_observe h depth filtered x ();
    let closed = R.close_heap h depth pool in
    closed_at_def h closed depth filtered x;
    ())

let rec (run @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === x &&
        (if H.mem h x then F.finite h t else U.observe h x === None)}
      @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (execution : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (trace : A.trace) @ immutable ->
    {u : unit | ran h depth pool env execution after final_pool
      && S.records trace execution} ->
    {u : unit | S.owned after trace} @ ghost =
  fun h trees depth pool env execution after final_pool trace premise -> ghost_ (
    let var : desc = Var in
    S.records_def trace execution; S.owned_def after trace;
    S.root_agrees trace execution (); A.root_def trace;
    (match A.root trace with
     | None -> ()
     | Some p -> P.run_result h trees depth pool env execution
         after final_pool p ());
    S.option_owned_def after (A.root trace);
    ran_def h depth pool env execution after final_pool;
    match trace, execution with
    | A.Failed, _ | A.Variable_use _, _ | A.Boolean_literal _, _ | A.False_literal _, _ | A.Empty_list_literal _, _ | A.Word_literal _, _ -> ()
    | A.List_case child, RCaseList (_, _, _, body)
    | A.Conditional child, RIf (_, _, _, body) ->
      run h trees depth pool env body after final_pool child ()
    | A.Primitive (_, _, child), RPrimitive (op, _, _, body, middle, body_pool, out) ->
      run h trees depth pool env body middle body_pool child ();
      let frame : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem middle x) || H.mem after x}) @ total = fun x ->
        (match result body with None -> () | Some _ -> match out with None -> () | Some _ ->
          let desc = primitive_desc op in let _ = cell desc depth in ()); () in
      S.owned_after middle after child frame ()
    | A.Abstraction (_, _, child), RLam (arg, body, middle, body_pool, _) ->
      let start = H.put h arg (cell var depth) in
      let ts = G.allocated_forest h trees depth arg var () in
      let next_pool = Entry (arg, pool) in
      let next_env = Bind (arg, env) in
      run start (refine_ ts) depth next_pool next_env body middle body_pool child ();
      M.run_extends start depth next_pool next_env body middle body_pool arg ();
      (match result body with
       | None -> unreachable_ ()
       | Some _ ->
         let frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem middle x) || H.mem after x}) @ total =
           fun x -> let _ = x in () in
         S.owned_after middle after child frame ())
    | A.List_constructor (_, left, right),
        RCons (l, r, h1, pool1, h2, pool2, p, ok, d) ->
      run h trees depth pool env l h1 pool1 left ();
      let ts1 : ((x : node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === x &&
          (if H.mem h1 x then F.finite h1 t else U.observe h1 x === None)}
        @ immutable) @ total = fun x ->
        G.run_forest h trees depth pool env l h1 pool1 x () in
      run h1 ts1 depth pool1 env r h2 pool2 right ();
      (match result l, result r with
       | Some f, Some a ->
         let h3 = H.put h2 p (cell (List f) depth) in
         let right_frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem h2 x) || H.mem after x}) @ total =
           fun x ->
             Effective_unifier_frame.unified_frame h3 a p ok after d x () in
         let left_frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem h1 x) || H.mem after x}) @ total =
           fun x ->
             M.run_extends h1 depth pool1 env r h2 pool2 x ();
             right_frame x in
         S.owned_after h1 after left left_frame ();
         S.owned_after h2 after right right_frame ()
       | _ -> unreachable_ ())
    | A.Application (_, left, right),
        RApp (l, r, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run h trees depth pool env l h1 pool1 left ();
      let ts1 : ((x : node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === x &&
          (if H.mem h1 x then F.finite h1 t else U.observe h1 x === None)}
        @ immutable) @ total = fun x ->
        G.run_forest h trees depth pool env l h1 pool1 x () in
      run h1 ts1 depth pool1 env r h2 pool2 right ();
      (match result l, result r with
       | Some f, Some a ->
         let h3 = H.put h2 p (cell var depth) in
         let h4 = H.put h3 arrow (cell (Arrow (a, p)) depth) in
         let right_frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem h2 x) || H.mem after x}) @ total =
           fun x ->
             Effective_unifier_frame.unified_frame h4 f arrow ok after d x () in
         let left_frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem h1 x) || H.mem after x}) @ total =
           fun x ->
             M.run_extends h1 depth pool1 env r h2 pool2 x ();
             right_frame x in
         S.owned_after h1 after left left_frame ();
         S.owned_after h2 after right right_frame ()
       | _ -> unreachable_ ())
    | A.Recursion (_, _, _, child),
        RRec (arg, res, self, body, middle, body_pool, finish) ->
      let h1 = H.put h arg (cell var depth) in
      let ts1 = G.allocated_forest h trees depth arg var () in
      let h2 = H.put h1 res (cell var depth) in
      let ts2 = G.allocated_forest h1 (refine_ ts1) depth res var () in
      let desc = Arrow (arg, res) in
      let h3 = H.put h2 self (cell desc depth) in
      let ts3 = G.allocated_forest h2 (refine_ ts2) depth self desc () in
      let next_pool = Entry (self, Entry (res, Entry (arg, pool))) in
      let next_env = Bind (arg, Bind (self, env)) in
      run h3 (refine_ ts3) depth next_pool next_env body middle body_pool child ();
      M.run_extends h3 depth next_pool next_env body middle body_pool arg ();
      M.run_extends h3 depth next_pool next_env body middle body_pool res ();
      let frame : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem middle x) || H.mem after x}) @ total =
        fun x ->
          match result body, finish with
          | None, _ -> ()
          | Some b, Unified (ok, d) ->
            Effective_unifier_frame.unified_frame middle b res ok after d x ()
          | _ -> unreachable_ () in
      frame arg; frame res;
      S.owned_after middle after child frame ()
    | A.Let_binding (rhs, body), RLet (r, b, middle, child_pool) ->
      let child_depth = depth + 1 in
      let empty : pool = Generalize_spec.Empty in
      run h trees child_depth empty env r middle child_pool rhs ();
      let ts1 : ((x : node Pref.t) @ immutable ->
        {t : F.tree | F.tree_root t === x &&
          (if H.mem middle x then F.finite middle t
           else U.observe middle x === None)} @ immutable) @ total = fun x ->
        G.run_forest h trees child_depth empty env r middle child_pool x () in
      (match result r with
       | None -> unreachable_ ()
       | Some p ->
         let closed = R.close_heap middle depth child_pool in
         let ts2 : ((x : node Pref.t) @ immutable ->
           {t : F.tree | F.tree_root t === x &&
             (if H.mem closed x then F.finite closed t
              else U.observe closed x === None)} @ immutable) @ total = fun x ->
           G.representative_closed_forest middle ts1 depth child_pool x () in
         let parent = R.transfer_rep closed child_pool pool in
         let next_env = Bind (p, env) in
         run closed ts2 depth parent next_env b after final_pool body ();
         let frame : ((x : node Pref.t) @ immutable ->
           {u : unit | not (H.mem middle x) || H.mem after x}) @ total =
           fun x ->
             closing_extends middle depth child_pool x ();
             M.run_extends closed depth parent next_env b after final_pool x () in
         S.owned_after middle after rhs frame ())
    | _ -> ())
