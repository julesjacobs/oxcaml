open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_runtime_spec
open Level_finite_spec
module D = Hm_declarative
module T = Hm_type_proofs

let[@def] rec (mono_env @ total) (h : Pref.heap @ immutable) (env : env @ immutable) = ghost_ (
  match env with Empty -> true | Bind (p, rest) -> active h p && mono_env h rest)
let[@def] rec (context @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (env : env @ immutable) = match env with Empty -> D.Empty_context
  | Bind (p, rest) -> D.Binding (D.Forall (D.Z, D.embed (rho p)), context rho rest)

let rec (context_wf @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (env : env) @ immutable -> {u : unit | D.context_wf D.Z (context rho env)} @ ghost = fun rho env -> ghost_ (
  context_def rho env; let g = context rho env in let z = D.Z in D.context_wf_def z g;
  (match env with Empty -> () | Bind (p, rest) -> let t = rho p in
    let s = D.Forall (z, D.embed t) in D.scheme_wf_def z s; D.add_def z z;
    T.embed_wf z t; context_wf rho rest; ()); let u = () in u)

let rec (lookup_context @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (env : env) @ immutable -> (i : D.index) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | lookup env i === Some p} ->
    {u : unit | D.lookup (context rho env) i === Some (D.Forall (D.Z, D.embed (rho p)))} @ ghost =
  fun rho env i p premise -> ghost_ (
    lookup_def env i; context_def rho env;
    let g = context rho env in D.lookup_def g i;
    let u = () in match env with Empty -> u | Bind (_, rest) -> match i with
    | D.Z -> u | D.S i -> lookup_context rho rest i p (u); u)

let rec (mono_lookup @ total) : (h : Pref.heap) @ immutable -> (env : env) @ immutable ->
    (i : D.index) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | mono_env h env && lookup env i === Some p} -> {u : unit | active h p} @ ghost =
  fun h env i p premise -> ghost_ (
    mono_env_def h env; lookup_def env i;
    let u = () in match env with Empty -> u | Bind (_, rest) -> match i with
    | D.Z -> u | D.S i -> mono_lookup h rest i p (u); u)

let rec (allocation_env @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (env : env) @ immutable ->
    {u : unit | not (H.mem h p) && mono_env h env} -> {u : unit | mono_env (H.put h p v) env} @ ghost =
  fun h p v env premise -> ghost_ (
    let after = H.put h p v in mono_env_def h env; mono_env_def after env;
    let u = () in match env with Empty -> u | Bind (q, rest) ->
      Hm_runtime_proofs.allocation_active h p v q (u);
      allocation_env h p v rest (u); u)

let rec (run_env @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable -> (bindings : env) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && mono_env h bindings} ->
    {u : unit | mono_env after bindings} @ ghost = fun h depth pool env e after final_pool bindings premise -> ghost_ (
    mono_env_def h bindings; mono_env_def after bindings;
    let u = () in match bindings with Empty -> u | Bind (p, rest) ->
      Hm_runtime_proofs.run_active h depth pool env e after final_pool p (u);
      run_env h depth pool env e after final_pool rest (u); u)

let rec (run_sound @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && mono_env h env && result e === Some p} ->
    {d : D.typing | D.typed D.Z (context rho env) (source e) (D.embed (rho p)) d} @ immutable ghost =
  fun h trees depth pool env e after final_pool rho model p premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    let_free_def e; result_def e; source_def e; context_wf rho env;
    let g = context rho env in let term = source e in let ty = rho p in let t = D.embed ty in
    let z = D.Z in T.embed_wf z ty; let u = () in match e with
    | RApp_left _ | RApp_right _ | RLet_left _ | RLet _ ->
      let d = D.Constant in d
    | RShared (i, _) ->
      lookup_context rho env i p (u);
      let args = D.No_arguments in let d = D.Variable args in let scheme = D.Forall (z, t) in
      D.typed_def z g term t d; D.length_def args; D.arity_def scheme;
      D.arguments_wf_def z args; D.open_scheme_def scheme args; T.open_empty t; d
    | RVar (i, _, _, history) -> (match lookup env i with None -> let d = D.Constant in d
      | Some original -> mono_lookup h env i original (u);
        active_def h original; at_level_def h original; target_for_def h history original p;
        lookup_context rho env i p (u);
        let args = D.No_arguments in let d = D.Variable args in let scheme = D.Forall (z, t) in
        D.typed_def z g term t d; D.length_def args; D.arity_def scheme;
        D.arguments_wf_def z args; D.open_scheme_def scheme args; T.open_empty t; d)
    | RBool _ -> model p; node_equation_def after rho p; observe_def after p;
      let desc : desc = Bool in cell_def desc depth; D.embed_def ty;
      let d = D.Constant in D.typed_def z g term t d; d
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      allocated_def h depth arg var; cell_def var depth; allocatable_def h v;
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      allocation_env h arg v env (u); Hm_runtime_proofs.fresh_active h depth arg var (u);
      mono_env_def h1 env1; context_def rho env1;
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Level_finite_proofs.allocation_finite_at h trees arg v x (u) in t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Hm_forest_proofs.run_forest h1 ts1 depth pool1 env1 body middle body_pool x (u) in t in
      (match result body with None -> let d = D.Constant in d | Some b -> match out with
      | None -> let d = D.Constant in d | Some _ ->
      let desc = Arrow (arg, b) in let w = cell desc depth in allocated_def middle depth p desc;
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in let _t = ts2 x in model x; Hm_model_proofs.allocation_restrict middle p w rho x (u); u in
      let body_typing = run_sound h1 ts1 depth pool1 env1 body middle body_pool rho mid_model b (u) in
      model p; node_equation_def after rho p; observe_def after p; cell_def desc depth;
      D.embed_def ty; let a = D.embed (rho arg) in let d = D.Abstraction (a, body_typing) in
      D.typed_def z g term t d; d)
    | RApp (left, right, h1, pool1, h2, pool2, _, arrow, ok, derivation) ->
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Hm_forest_proofs.run_forest h trees depth pool env left h1 pool1 x (u) in t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Hm_forest_proofs.run_forest h1 ts1 depth pool1 env right h2 pool2 x (u) in t in
      (match result left with None -> let d = D.Constant in d | Some f ->
      match result right with None -> let d = D.Constant in d | Some a ->
      let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
      let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
      allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
      cell_def var depth; allocatable_def h2 v;
      let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Level_finite_proofs.allocation_finite_at h2 ts2 p v x (u) in t in
      let model4 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h4 rho x}) @ total = fun x ->
        let u = () in Optimized_model_proofs.success_forward_at h4 rho f arrow after derivation model x (u); u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in let _t = ts3 x in model4 x; Hm_model_proofs.allocation_restrict h3 arrow w rho x (u); u in
      let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
        let u = () in let _t = ts2 x in model3 x; Hm_model_proofs.allocation_restrict h2 p v rho x (u); u in
      let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
        let u = () in Hm_model_proofs.run_restrict h1 ts1 depth pool1 env right h2 pool2 rho model2 x (u); u in
      let left_typing = run_sound h trees depth pool env left h1 pool1 rho model1 f (u) in
      run_env h depth pool env left h1 pool1 env (u);
      let right_typing = run_sound h1 ts1 depth pool1 env right h2 pool2 rho model2 a (u) in
      Optimized_model_proofs.success_forward_at h4 rho f arrow after derivation model arrow (u);
      node_equation_def h4 rho arrow; observe_def h4 arrow; cell_def desc depth;
      let ft = rho f in D.embed_def ft;
      let at = D.embed (rho a) in let d = D.Application (at, left_typing, right_typing) in
      D.typed_def z g term t d; d)
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
        let u = () in let t = Level_finite_proofs.allocation_finite_at h trees arg v x (u) in t in
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Level_finite_proofs.allocation_finite_at h1 ts1 res v x (u) in t in
      let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let t = Level_finite_proofs.allocation_finite_at h2 ts2 self w x (u) in t in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in
      let self_env = Bind (self, env) in let env3 = Bind (arg, self_env) in
      allocation_env h arg v env (u); allocation_env h1 res v env (u);
      allocation_env h2 self w env (u);
      Hm_runtime_proofs.fresh_active h depth arg var (u);
      Hm_runtime_proofs.allocation_active h1 res v arg (u);
      Hm_runtime_proofs.allocation_active h2 self w arg (u);
      Hm_runtime_proofs.fresh_active h2 depth self desc (u);
      mono_env_def h3 self_env; mono_env_def h3 env3; context_def rho self_env; context_def rho env3;
      (match result body with None -> let d = D.Constant in d | Some b -> match finish with
      | Aborted -> let d = D.Constant in d | Unified (_, derivation) ->
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in Optimized_model_proofs.success_forward_at middle rho b res after derivation model x (u); u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in Hm_model_proofs.run_restrict h3 ts3 depth pool3 env3 body middle body_pool rho mid_model x (u); u in
      let body_typing = run_sound h3 ts3 depth pool3 env3 body middle body_pool rho mid_model b (u) in
      Optimized_model_proofs.success_forward_at middle rho b res after derivation model p (u);
      model3 self; node_equation_def h3 rho self; observe_def h3 self; D.embed_def ty;
      let at = D.embed (rho arg) in let bt = D.embed (rho res) in
      let d = D.Recursion (at, bt, body_typing) in D.typed_def z g term t d; d))

let (closed_sound @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (t : tree) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && result e === Some p && finite after t && tree_root t === p} ->
    {d : D.typing | D.typed D.Z D.Empty_context (source e) (D.embed (readback t)) d} @ immutable ghost =
  fun e after pool p t premise -> ghost_ (
    let h = H.empty () in
    let trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total = fun x ->
      let t = Free x in tree_root_def t; observe_def h x; t in
    let final_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total = fun x ->
      let u = () in let t = Hm_forest_proofs.closed_forest e after pool x (u) in t in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let t = final_trees x in readback t in
    let agrees : ((x : node Pref.t) @ immutable ->
        {u : unit | let t = final_trees x in not (H.mem after x) || rho x === readback t}) @ total = fun x ->
      rho_def x; let u = () in u in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x}) @ total = fun x ->
      let u = Level_finite_proofs.readback_model_at after final_trees rho agrees x in u in
    let env : env = Hm_environment_spec.Empty in let empty : pool = Generalize_spec.Empty in
    mono_env_def h env; context_def rho env; let u = () in
    let d = run_sound h trees 0 empty env e after pool rho model p (u) in
    let actual = final_trees p in rho_def p; finite_def after t;
    Level_finite_proofs.finite_unique after t actual (u); d)
