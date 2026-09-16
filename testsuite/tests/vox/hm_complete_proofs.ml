open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec
open Hm_runtime_spec
open Hm_sound_proofs
module D = Hm_declarative
module T = Hm_type_proofs

let[@def] (default_value @ total) (_i : D.index @ immutable) = Boolean
open Optimized_unifier_spec

let rec (embed_eval @ total) : (t : D.mono) @ immutable ->
    {u : unit | D.mono_wf D.Z t} -> {u : unit | T.embed (T.eval default_value t) === t} @ ghost =
  fun t premise -> ghost_ (
    let refine_ premise = premise in let z = D.Z in D.mono_wf_def z t;
    T.eval_def default_value t; let value = T.eval default_value t in T.embed_def value;
    let u = () in match t with D.Free _ | D.Boolean -> refine_ u
    | D.Parameter i -> D.present_def z i; refine_ u
    | D.Function (a, b) -> embed_eval a (refine_ u); embed_eval b (refine_ u); refine_ u)
let (embed_injective @ total) : (a : ty) @ immutable -> (b : ty) @ immutable ->
    {u : unit | T.embed a === T.embed b} -> {u : unit | a === b} @ ghost = fun a b premise -> ghost_ (
    let refine_ premise = premise in T.eval_embed default_value a; T.eval_embed default_value b;
    let u = () in refine_ u)
let rec (context_equal @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
    (env : env) @ immutable -> {u : unit | mono_env h env} ->
    {u : unit | context tau env === context rho env} @ ghost = fun h rho tau equal env premise -> ghost_ (
    let refine_ premise = premise in mono_env_def h env; context_def rho env; context_def tau env;
    let u = () in match env with Empty -> refine_ u | Bind (p, rest) ->
      active_def h p; equal p; context_equal h rho tau equal rest (refine_ u); refine_ u)

let (with_alloc @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable -> (value : ty) @ immutable -> (after : Pref.heap) @ immutable ->
    {u : unit | allocated h depth p desc && after === H.put h p (cell desc depth) && Copy_model_proofs.describes rho desc value} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau p === value} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h depth pool facts rho model p desc value after premise claim use -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc; children_below_def h desc depth;
    let v = cell desc depth in cell_def desc depth; payload_scoped_def h v;
    (match desc with Var | Bool -> () | Link q -> below_def h q depth; ()
      | Arrow (a, b) -> below_def h a depth; below_def h b depth; ());
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u in
    let old : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      model x; node_equation_def h rho x; observe_def h x; equation_def h rho x; let u = () in refine_ u in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put h p v) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau p === value} -> {u : unit | claim}) @ total = fun tau next equal fit ->
      
      let converted : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x}) @ total = fun x ->
        next x; equation_def after tau x; observe_def after x; node_equation_def after tau x; let u = () in refine_ u in
      let refine_ u = use tau converted equal fit in refine_ u in
    let u = () in let refine_ u = Copy_model_proofs.with_allocation_model h scope rho old p v value (refine_ u) claim consume in refine_ u)

let (with_copy @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    {u : unit | valid h epoch depth d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation (copy_heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
        {u : unit | not (target_for h d p q) || tau q === rho p})) @ total -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h depth pool facts rho model epoch d premise claim use -> ghost_ (
    let refine_ premise = premise in
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u in
    let old : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      model x; node_equation_def h rho x; observe_def h x; equation_def h rho x; let u = () in refine_ u in
    let wanted : ((x : node Pref.t) @ immutable -> {u : unit | instance_at h rho rho x}) @ total = fun x ->
      old x; equation_def h rho x; instance_at_def h rho rho x; let u = () in refine_ u in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
        {u : unit | not (target_for h d p q) || tau q === rho p})) @ total -> {u : unit | claim}) @ total = fun tau next equal assigned ->
      let after = copy_heap h epoch depth d in copy_heap_def h epoch depth d;
      let converted : ((x : node Pref.t) @ immutable -> {u : unit | node_equation (copy_heap h epoch depth d) tau x}) @ total = fun x ->
        next x; let u = () in Clean_copy.model_equivalence h epoch depth d tau x (refine_ u);
        equation_def after tau x; observe_def after x; node_equation_def after tau x; refine_ u in
      let refine_ u = use tau converted equal assigned in refine_ u in
    let u = () in let refine_ u = Copy_complete_proofs.with_copy_model h scope rho old rho wanted epoch depth d (refine_ u) claim consume in refine_ u)

let (unify_complete @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && rho p === rho q} ->
    {u : unit | ok && node_equation after rho x} @ ghost = fun h rho model p q ok after d x premise -> ghost_ (
      let refine_ premise = premise in let u = () in
      if ok then (Optimized_model_proofs.success_backward_at h rho model p q after d x (refine_ u); refine_ u)
      else (Optimized_model_proofs.failure_refutes h rho model p q after d (refine_ u); refine_ u))

let (allocation_facts @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable -> {u : unit | allocated h depth p desc} ->
    ((x : node Pref.t) @ immutable -> {u : unit | runtime_at (H.put h p (cell desc depth)) depth (Entry (p, pool)) x}) @ total ghost =
  fun h depth pool facts p desc premise -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc;
    let next : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at (H.put h p (cell desc depth)) depth (Entry (p, pool)) x}) @ total = fun x ->
      facts x; let u = () in let refine_ u = Hm_runtime_proofs.allocate_runtime h depth pool p desc x (refine_ u) in refine_ u in next)
let (execution_facts @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e} ->
    ((x : node Pref.t) @ immutable -> {u : unit | runtime_at after depth final_pool x}) @ total ghost =
  fun h depth pool facts env e after final_pool premise -> ghost_ (
    let refine_ premise = premise in
    let next : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at after depth final_pool x}) @ total = fun x ->
      let u = () in let refine_ u = Hm_runtime_proofs.run_runtime h depth pool facts env e after final_pool x (refine_ u) in refine_ u in next)
let[@def] (matches @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (e : execution @ immutable) (target : ty @ immutable) = ghost_ (
      match result e with None -> false | Some p -> rho p === target)

let rec (with_run_model @ total) : (h : Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && let_free e && mono_env h env
      && D.typed D.Z (context rho env) (source e) (T.embed target) d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total -> {u : unit | matches tau e target} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h depth pool facts env e after final_pool rho model target d premise claim use -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
    let_free_def e; source_def e; result_def e;
    let z = D.Z in let g = context rho env in let source_term = source e in let typ = T.embed target in
    D.typed_def z g source_term typ d; T.embed_def target; let u = () in match e with
    | RLet_left _ | RLet _ -> refine_ u
    | RShared (i, p) -> (match d with
      | D.Variable args ->
        lookup_context rho env i p (refine_ u);
        let scheme = D.Forall (z, T.embed (rho p)) in D.arity_def scheme; D.length_def args;
        (match args with D.Argument _ -> refine_ u | D.No_arguments ->
          D.open_scheme_def scheme args; let ot = T.embed (rho p) in T.open_empty ot;
          let original_type = rho p in embed_injective target original_type (refine_ u);
          let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho x === rho x}) @ total = fun x -> let u = () in refine_ u in
          matches_def rho e target; let refine_ u = use rho (refine_ model) equal (refine_ u) in refine_ u)
      | _ -> refine_ u)
    | RVar (i, p, epoch, history) -> (match d with
      | D.Variable args -> (match lookup env i with None -> refine_ u | Some original ->
        lookup_context rho env i original (refine_ u);
        let scheme = D.Forall (z, T.embed (rho original)) in D.arity_def scheme;
        D.length_def args; (match args with D.Argument _ -> refine_ u | D.No_arguments ->
        D.open_scheme_def scheme args; let ot = T.embed (rho original) in T.open_empty ot;
        let original_type = rho original in embed_injective target original_type (refine_ u);
        let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation (copy_heap h epoch depth history) tau x})) @ total ->
          (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
          (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
            {u : unit | not (target_for h history p q) || tau q === rho p})) @ total -> {u : unit | claim}) @ total = fun tau next equal assigned ->
          assigned original p; matches_def tau e target;
          let u = () in let refine_ u = use tau (refine_ next) equal (refine_ u) in refine_ u in
        let refine_ u = with_copy h depth pool facts rho model epoch history (refine_ u) claim consume in refine_ u))
      | _ -> refine_ u)
    | RBool p -> (match d with D.Constant ->
      let desc : desc = Bool in Copy_model_proofs.describes_def rho desc target;
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total -> {u : unit | tau p === target} -> {u : unit | claim}) @ total = fun tau next equal fit ->
        let refine_ fit = fit in let u = () in
        matches_def tau e target; let refine_ u = use tau next equal (refine_ u) in refine_ u in
      let refine_ u = with_alloc h depth pool facts rho model p desc target after (refine_ u) claim (refine_ consume) in refine_ u
      | _ -> refine_ u)
    | RLam (arg, body, middle, body_pool, out) -> (match d with D.Abstraction (_, db) ->
      (match target with Variable _ | Boolean -> refine_ u | Function (a, b) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let env1 = Bind (arg, env) in
      let facts1 = allocation_facts h depth pool facts arg var (refine_ u) in
      allocated_def h depth arg var; allocation_env h arg v env (refine_ u);
      Hm_runtime_proofs.fresh_active h depth arg var (refine_ u); mono_env_def h1 env1;
      Copy_model_proofs.describes_def rho var a;
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total -> {u : unit | rho1 arg === a} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit ->
        let refine_ fit = fit in let u = () in
        context_equal h rho rho1 equal1 env (refine_ u); context_def rho1 env1;
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total -> {u : unit | matches rho2 body b} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit ->
        let refine_ fit = fit in let u = () in
        matches_def rho2 body b;
        (match result body with None -> refine_ u | Some root -> match out with None -> refine_ u | Some p ->
        equal2 arg; active_def h1 arg;
        let desc = Arrow (arg, root) in Copy_model_proofs.describes_def rho2 desc target;
        let facts2 = execution_facts h1 depth pool1 (refine_ facts1) env1 body middle body_pool (refine_ u) in
      let consume3 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
        (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || tau x === rho2 x})) @ total -> {u : unit | tau p === target} -> {u : unit | claim}) @ total = fun tau next equal3 fit ->
        let refine_ fit = fit in let u = () in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
          let u = () in Copy_heap_proofs.put_frame h arg v x;
          Hm_execution_proofs.run_extends h1 depth pool1 env1 body middle body_pool x (refine_ u);
          equal1 x; equal2 x; equal3 x; refine_ u in
        matches_def tau e target; let refine_ u = use tau next equal (refine_ u) in refine_ u in
        let refine_ u = with_alloc middle depth body_pool facts2 rho2 model2 p desc target after (refine_ u) claim (refine_ consume3) in refine_ u) in
        let refine_ u = with_run_model h1 depth pool1 (refine_ facts1) env1 body middle body_pool rho1 model1 b db (refine_ u) claim consume2 in refine_ u in
      let refine_ u = with_alloc h depth pool facts rho model arg var a h1 (refine_ u) claim (refine_ consume1) in refine_ u)
      | _ -> refine_ u)
    | RApp_left (left, _) -> (match d with D.Application (at, df, da) ->
      let a = T.eval default_value at in
      (match source_term with D.Apply (_, argument) -> D.typed_def z g argument at da; () | _ -> ());
      embed_eval at (refine_ u); let function_type = Function (a, target) in T.embed_def function_type;
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total -> {u : unit | matches tau left function_type} -> {u : unit | claim}) @ total = fun tau next equal fit ->
        let refine_ fit = fit in let u = () in
        matches_def tau left function_type; refine_ u in
      let refine_ u = with_run_model h depth pool facts env left after final_pool rho model function_type df (refine_ u) claim consume in refine_ u
      | _ -> refine_ u)
    | RApp_right (left, right, h1, pool1) -> (match d with D.Application (at, df, da) ->
      let a = T.eval default_value at in
      (match source_term with D.Apply (_, argument) -> D.typed_def z g argument at da; () | _ -> ());
      embed_eval at (refine_ u); let function_type = Function (a, target) in T.embed_def function_type;
      let facts1 = execution_facts h depth pool facts env left h1 pool1 (refine_ u) in
      run_env h depth pool env left h1 pool1 env (refine_ u);
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total -> {u : unit | matches rho1 left function_type} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit ->
        let refine_ fit = fit in let u = () in
        context_equal h rho rho1 equal1 env (refine_ u);
      let consume2 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || tau x === rho1 x})) @ total -> {u : unit | matches tau right a} -> {u : unit | claim}) @ total = fun tau next equal2 fit ->
        let refine_ fit = fit in let u = () in
        matches_def tau right a; refine_ u in
        let refine_ u = with_run_model h1 depth pool1 facts1 env right after final_pool rho1 model1 a da (refine_ u) claim consume2 in refine_ u in
      let refine_ u = with_run_model h depth pool facts env left h1 pool1 rho model function_type df (refine_ u) claim consume1 in refine_ u
      | _ -> refine_ u)
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, derivation) -> (match d with D.Application (at, df, da) ->
      let a = T.eval default_value at in
      (match source_term with D.Apply (_, argument) -> D.typed_def z g argument at da; () | _ -> ());
      embed_eval at (refine_ u); let function_type = Function (a, target) in T.embed_def function_type;
      let facts1 = execution_facts h depth pool facts env left h1 pool1 (refine_ u) in
      run_env h depth pool env left h1 pool1 env (refine_ u);
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total -> {u : unit | matches rho1 left function_type} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit ->
        let refine_ fit = fit in let u = () in
        context_equal h rho rho1 equal1 env (refine_ u);
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total -> {u : unit | matches rho2 right a} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit ->
        let refine_ fit = fit in let u = () in
        matches_def rho1 left function_type; matches_def rho2 right a;
        (match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some arg ->
        Hm_execution_proofs.run_result h depth pool env left h1 pool1 f (refine_ u);
        Hm_execution_proofs.run_result h1 depth pool1 env right h2 pool2 arg (refine_ u);
        Hm_execution_proofs.run_extends h1 depth pool1 env right h2 pool2 f (refine_ u);
        equal2 f;
        let facts2 = execution_facts h1 depth pool1 facts1 env right h2 pool2 (refine_ u) in
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let pool3 = Entry (p, pool2) in let desc = Arrow (arg, p) in let w = cell desc depth in
        let h4 = H.put h3 arrow w in
        let facts3 = allocation_facts h2 depth pool2 facts2 p var (refine_ u) in
        Copy_model_proofs.describes_def rho2 var target;
      let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho3 x})) @ total ->
        (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total -> {u : unit | rho3 p === target} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit ->
        let refine_ fit = fit in let u = () in
        equal3 arg; equal3 f; Copy_heap_proofs.put_frame h2 p v f;
        Copy_model_proofs.describes_def rho3 desc function_type;
      let consume4 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model4 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h4 tau x})) @ total ->
        (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total -> {u : unit | tau arrow === function_type} -> {u : unit | claim}) @ total = fun tau model4 equal4 fit ->
        let refine_ fit = fit in let u = () in
        equal4 f; equal4 p; Copy_heap_proofs.put_frame h2 p v p;
        unify_complete h4 tau model4 f arrow ok after derivation arrow (refine_ u);
        let next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x}) @ total = fun x ->
          let u = () in let refine_ u = unify_complete h4 tau model4 f arrow ok after derivation x (refine_ u) in refine_ u in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
          let u = () in Hm_execution_proofs.run_extends h depth pool env left h1 pool1 x (refine_ u);
          Hm_execution_proofs.run_extends h1 depth pool1 env right h2 pool2 x (refine_ u);
          Copy_heap_proofs.put_frame h2 p v x; equal1 x; equal2 x; equal3 x; equal4 x; refine_ u in
        matches_def tau e target; let refine_ u = use tau next equal (refine_ u) in refine_ u in
        let refine_ u = with_alloc h3 depth pool3 (refine_ facts3) rho3 model3 arrow desc function_type h4 (refine_ u) claim (refine_ consume4) in refine_ u in
        let refine_ u = with_alloc h2 depth pool2 facts2 rho2 model2 p var target h3 (refine_ u) claim (refine_ consume3) in refine_ u) in
        let refine_ u = with_run_model h1 depth pool1 facts1 env right h2 pool2 rho1 model1 a da (refine_ u) claim consume2 in refine_ u in
      let refine_ u = with_run_model h depth pool facts env left h1 pool1 rho model function_type df (refine_ u) claim consume1 in refine_ u
      | _ -> refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) -> (match d with D.Recursion (_, _, db) ->
      (match target with Variable _ | Boolean -> refine_ u | Function (a, b) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
      let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
      let pool3 = Entry (self, pool2) in let self_env = Bind (self, env) in let env3 = Bind (arg, self_env) in
      let facts1 = allocation_facts h depth pool facts arg var (refine_ u) in
      let facts2 = allocation_facts h1 depth pool1 (refine_ facts1) res var (refine_ u) in
      let facts3 = allocation_facts h2 depth pool2 (refine_ facts2) self desc (refine_ u) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      allocation_env h arg v env (refine_ u); allocation_env h1 res v env (refine_ u); allocation_env h2 self w env (refine_ u);
      Hm_runtime_proofs.fresh_active h depth arg var (refine_ u);
      Hm_runtime_proofs.allocation_active h1 res v arg (refine_ u); Hm_runtime_proofs.allocation_active h2 self w arg (refine_ u);
      Hm_runtime_proofs.fresh_active h2 depth self desc (refine_ u);
      mono_env_def h3 self_env; mono_env_def h3 env3;
      Copy_model_proofs.describes_def rho var a;
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total -> {u : unit | rho1 arg === a} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit ->
        let refine_ fit = fit in let u = () in
        Copy_model_proofs.describes_def rho1 var b;
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total -> {u : unit | rho2 res === b} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit ->
        let refine_ fit = fit in let u = () in
        active_def h1 arg; equal2 arg; Copy_model_proofs.describes_def rho2 desc target;
      let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho3 x})) @ total ->
        (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total -> {u : unit | rho3 self === target} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit ->
        let refine_ fit = fit in let u = () in
        Copy_heap_proofs.put_frame h1 res v arg; Copy_heap_proofs.put_frame h1 res v res;
        equal3 arg; equal3 res;
        let old_equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
          Copy_heap_proofs.put_frame h arg v x; Copy_heap_proofs.put_frame h1 res v x;
          equal1 x; equal2 x; equal3 x; let u = () in refine_ u in
        context_equal h rho rho3 old_equal env (refine_ u);
        context_def rho3 self_env; context_def rho3 env3;
      let consume4 : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (body_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle tau x})) @ total ->
        (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total -> {u : unit | matches tau body b} -> {u : unit | claim}) @ total = fun tau body_model equal4 fit ->
        let refine_ fit = fit in let u = () in
        matches_def tau body b;
        (match result body with None -> refine_ u | Some root -> match finish with Aborted -> refine_ u | Unified (ok, derivation) ->
        Copy_heap_proofs.put_frame h2 self w res; Copy_heap_proofs.put_frame h2 self w self;
        equal4 res; equal4 self;
        unify_complete middle tau body_model root res ok after derivation root (refine_ u);
        let next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x}) @ total = fun x ->
          let u = () in let refine_ u = unify_complete middle tau body_model root res ok after derivation x (refine_ u) in refine_ u in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
          Copy_heap_proofs.put_frame h arg v x; Copy_heap_proofs.put_frame h1 res v x; Copy_heap_proofs.put_frame h2 self w x;
          old_equal x; equal4 x; let u = () in refine_ u in
        matches_def tau e target; let refine_ u = use tau next equal (refine_ u) in refine_ u) in
        let refine_ u = with_run_model h3 depth pool3 (refine_ facts3) env3 body middle body_pool rho3 model3 b db (refine_ u) claim consume4 in refine_ u in
        let refine_ u = with_alloc h2 depth pool2 (refine_ facts2) rho2 model2 self desc target h3 (refine_ u) claim (refine_ consume3) in refine_ u in
        let refine_ u = with_alloc h1 depth pool1 (refine_ facts1) rho1 model1 res var b h2 (refine_ u) claim (refine_ consume2) in refine_ u in
      let refine_ u = with_alloc h depth pool facts rho model arg var a h1 (refine_ u) claim (refine_ consume1) in refine_ u)
      | _ -> refine_ u))

let (with_closed_model @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && D.typed D.Z D.Empty_context (source e) (T.embed target) d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      {u : unit | matches tau e target} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool target d premise claim use -> ghost_ (
    let refine_ premise = premise in let h = H.empty () in
    let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
    let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 empty x}) @ total = fun x ->
      runtime_at_def h 0 empty x; safe_def h x; depth_bound_def h 0 x; covered_def h (-1) empty x;
      ordered_def h x; let u = () in refine_ u in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x}) @ total = fun x ->
      node_equation_def h rho x; observe_def h x; let u = () in refine_ u in
    mono_env_def h env; context_def rho env;
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | matches tau e target} -> {u : unit | claim}) @ total = fun tau next _equal fit ->
      let refine_ u = use tau next fit in refine_ u in
    let u = () in let refine_ u = with_run_model h 0 empty facts env e after pool rho model target d (refine_ u) claim consume in refine_ u)

let (closed_reject @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && result e === None
      && D.typed D.Z D.Empty_context (source e) (T.embed target) d} -> {u : unit | false} @ ghost =
  fun e after pool target d premise -> ghost_ (
    let refine_ premise = premise in
    let use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      {u : unit | matches tau e target} -> {u : unit | false}) @ total = fun tau _model fit ->
      let refine_ fit = fit in matches_def tau e target; let u = () in refine_ u in
    let u = () in let refine_ u = with_closed_model e after pool target d (refine_ u) false use in refine_ u)

let (closed_factor @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (t : Level_finite_spec.tree) @ immutable ->
    (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && result e === Some p && Level_finite_spec.finite after t && Level_finite_spec.tree_root t === p
      && D.typed D.Z D.Empty_context (source e) (T.embed target) d} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback t)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun e after pool p t target d premise claim use -> ghost_ (
    let refine_ premise = premise in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      {u : unit | matches tau e target} -> {u : unit | claim}) @ total = fun tau model fit ->
      let refine_ fit = fit in matches_def tau e target; let u = () in
      Level_mgu_proofs.readback_factor after tau model t (refine_ u);
      let refine_ u = use tau (refine_ u) in refine_ u in
    let u = () in let refine_ u = with_closed_model e after pool target d (refine_ u) claim consume in refine_ u)

let (closed_completes @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && D.typed D.Z D.Empty_context (source e) (T.embed target) d} ->
    {u : unit | not (result e === None)} @ ghost = fun e after pool target d premise -> ghost_ (
      let refine_ premise = premise in let u = () in match result e with Some _ -> refine_ u
      | None -> closed_reject e after pool target d (refine_ u); refine_ u)
