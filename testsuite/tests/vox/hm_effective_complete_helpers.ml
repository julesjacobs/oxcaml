open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_effective_execution_spec
open Hm_effective_runtime
open Hm_environment_spec
module D = Hm_declarative
module T = Hm_type_proofs
module E = Effective_level
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
let (with_alloc @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads depth pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable -> (value : ty) @ immutable -> (after : Pref.heap) @ immutable ->
    {u : unit | allocated h depth p desc && after === H.put h p (cell desc depth) && Copy_model_proofs.describes rho desc value} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau p === value} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h heads depth pool facts rho model p desc value after premise claim use -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc;
    let v = cell desc depth in cell_def desc depth; payload_scoped_def h v;
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; let u = () in refine_ u in
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

let (unify_complete @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : Effective_unifier_spec.derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d && rho p === rho q} ->
    {u : unit | ok && node_equation after rho x} @ ghost = fun h rho model p q ok after d x premise -> ghost_ (
      let refine_ premise = premise in let u = () in
      if ok then (Effective_unifier_model.success_backward_at h rho model p q after d x (refine_ u); refine_ u)
      else (Effective_unifier_model.failure_refutes h rho model p q after d (refine_ u); refine_ u))

let[@def] (matches @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (e : execution @ immutable) (target : ty @ immutable) = ghost_ (
      match result e with None -> false | Some p -> rho p === target)

let (node_model @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x}) @ total ghost =
  fun h rho model -> ghost_ (
    let out : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x}) @ total = fun x ->
      model x; equation_def h rho x; Level_unifier_spec.observe_def h x;
      Level_unifier_spec.node_equation_def h rho x; let u = () in refine_ u in out)

let (copy_model @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x})) @ total ->
    ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total ghost =
  fun h rho model -> ghost_ (
    let out : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      model x; Level_unifier_spec.node_equation_def h rho x; Level_unifier_spec.observe_def h x;
      equation_def h rho x; let u = () in refine_ u in out)


let rec (aligned_weaken @ total) : (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (k : D.index) @ immutable -> {u : unit | aligned g ts} ->
    {u : unit | aligned (D.weaken_context k g) ts} @ ghost = fun g ts k premise -> ghost_ (
    let refine_ premise = premise in aligned_def g ts; D.weaken_context_def k g;
    let shifted = D.weaken_context k g in aligned_def shifted ts;
    let u = () in match g with D.Empty_context -> refine_ u | D.Binding (_, rest) ->
      match ts with No_templates -> refine_ u | Template_binding (_, tail) -> aligned_weaken rest tail k (refine_ u); refine_ u)

let[@def] rec (default_values @ total) (k : D.index @ immutable) = match k with
  | D.Z -> T.No_values | D.S k -> T.Value (Boolean, default_values k)
let rec (default_values_length @ total) : (k : D.index) @ immutable ->
    {u : unit | T.values_length (default_values k) === k} @ ghost = fun k -> ghost_ (
    default_values_def k; let values = default_values k in T.values_length_def values;
    (match k with D.Z -> () | D.S k -> default_values_length k; ()); let u = () in refine_ u)

let rec (snapshot @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | Level_finite_spec.finite h tree} ->
    {s : template | root s === Level_finite_spec.tree_root tree
      && Effective_template.valid_template h heads s} @ immutable ghost =
  fun h heads tree premise -> ghost_ (
    let refine_ premise = premise in Level_finite_spec.finite_def h tree;
    Level_finite_spec.tree_root_def tree; let p = Level_finite_spec.tree_root tree in
    let u = () in if E.level h heads p === Generic then (
      Effective_template.generic_def h heads p;
      let s = match tree with
        | Level_finite_spec.Free _ -> Parameter p
        | Level_finite_spec.Constant_tree _ -> Constant p
        | Level_finite_spec.Alias_tree (_, child) ->
          let refine_ t = snapshot h heads child (refine_ u) in Indirect (p, t)
        | Level_finite_spec.Branch (_, a, b) ->
          let refine_ s = snapshot h heads a (refine_ u) in
          let refine_ t = snapshot h heads b (refine_ u) in Product (p, s, t) in
      root_def s; Effective_template.valid_template_def h heads s; refine_ s)
    else (let s = Boundary p in root_def s;
      Effective_template.finite_def h heads p;
      Effective_template.valid_template_def h heads s; refine_ s))

let (with_variable_model @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (certificate : Representative_certificate.certificate) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads depth pool x})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (not (H.mem h x) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (env : env) @ immutable -> (ts : templates) @ immutable -> (g : D.context) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable -> (args : D.arguments) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | Hm_effective_environment.effective_env h heads depth env ts && aligned g ts && D.lookup g i === Some sigma
      && D.length args === D.arity sigma && Copy_certificate_spec.certified_valid h certificate epoch depth d
      && (match lookup env i with None -> false | Some p -> Copy_certificate_spec.certifies h certificate epoch depth d p q)} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === T.eval xi (D.open_scheme sigma args)} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h heads certificate depth pool facts forest env ts g rho model xi realize i sigma args epoch d q premise claim use -> ghost_ (
    let refine_ premise = premise in copy_heap_def h epoch depth d; let u = () in Hm_environment_models.aligned_lookup g ts i (refine_ u);
    match template_lookup ts i with None -> refine_ u | Some schema ->
    let original = root schema in
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; let u = () in refine_ u in
    let actual = match lookup env i with None -> root schema | Some p -> p in
    let refine_ selected = Hm_effective_environment.lookup_schema h heads depth env ts i actual (refine_ u) in
    Copy_certificate_proofs.replay h certificate heads valid epoch depth d original q (refine_ u);
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; let u = () in refine_ u in
    let trees : ((x : node Pref.t) @ immutable ->
      {s : template | not (H.mem h x) || (root s === x && Effective_template.valid_template h heads s)} @ immutable) @ total = fun x ->
      let refine_ tree = forest x in let u = () in
      if H.mem h x then (let refine_ s = snapshot h heads tree (refine_ u) in refine_ s)
      else (let s = Boundary x in refine_ s) in
    T.eval_arguments_length xi args; T.eval_open_scheme xi sigma args;
    let values = T.eval_arguments xi args in
    let consume_choices : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma values} ->
      {u : unit | claim}) @ total = fun choices fit ->
      let refine_ fit = fit in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (Copy_cleanup_spec.swept (heap h epoch depth d) (Pooled_spec.touched d)) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau q === interpret rho choices schema} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
          let refine_ assigned = assigned in let u = () in let refine_ u = use tau (refine_ next) equal (refine_ u) in refine_ u in
      let u = () in let refine_ u = Effective_copy_template.with_clean_scheme_instance h heads scope trees rho model choices epoch depth d schema q (refine_ u) claim (refine_ consume) in refine_ u in
    let refine_ u = realize i sigma schema values (refine_ u) claim consume_choices in refine_ u)


let (with_application_model @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h x then Level_finite_spec.finite h t else observe h x === None)} @ immutable)) @ total -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads depth pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x})) @ total ->
    (f : node Pref.t) @ immutable -> (a : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (arrow : node Pref.t) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable -> (d : Effective_unifier_spec.derivation) @ immutable ->
    (target : ty) @ immutable ->
    {u : unit | H.mem h f && H.mem h a && rho f === Function (rho a, target)
      && allocated h depth p Var && allocated (H.put h p (cell Var depth)) depth arrow (Arrow (a, p))
      && Effective_unifier_spec.unified (H.put (H.put h p (cell Var depth)) arrow (cell (Arrow (a, p)) depth)) f arrow ok after d} ->
    (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | ok && tau p === target} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h heads forest depth pool facts rho model f a p arrow ok after d target premise claim use -> ghost_ (
    let refine_ premise = premise in let var : desc = Var in let v = cell var depth in
    let h1 = H.put h p v in let pool1 = Entry (p, pool) in
    let desc = Arrow (a, p) in let h2 = H.put h1 arrow (cell desc depth) in
    let u = () in allocated_def h depth p var;
    let trees1 = Hm_effective_forest.allocated_forest h forest depth p var (refine_ u) in
    let trees1 : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h1 x then Level_finite_spec.finite h1 t else observe h1 x === None)} @ immutable) @ total = refine_ trees1 in
    let[@def] heads1 : E.heads = fun x -> let refine_ r = Forest_heads.select h1 trees1 x in r in
    let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
      heads1_def x; let refine_ r = Forest_heads.select h1 trees1 x in E.valid_head_def h1 heads1 x; let u = () in refine_ u in
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; let u = () in refine_ u in
    let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
      facts x; let u = () in Hm_effective_allocation.allocate_runtime h heads heads1 depth pool p var valid (refine_ valid1) x (refine_ u); refine_ u in
    let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model1 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h1 rho1 x})) @ total ->
      (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
      {u : unit | rho1 p === target} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
      let refine_ fit1 = fit1 in equal1 f; equal1 a;
      let value = Function (rho1 a, target) in Copy_model_proofs.describes_def rho1 desc value;
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model2 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h2 rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
        {u : unit | rho2 arrow === value} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
          let refine_ fit2 = fit2 in equal2 f; equal2 p;
          let next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho2 x}) @ total = fun x ->
            let u = () in let refine_ u = unify_complete h2 rho2 model2 f arrow ok after d x (refine_ u) in refine_ u in
          let u = () in unify_complete h2 rho2 model2 f arrow ok after d p (refine_ u);
          let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho2 x === rho x}) @ total = fun x ->
            equal1 x; equal2 x; let u = () in refine_ u in
          let refine_ u = use rho2 next equal (refine_ u) in refine_ u in
      let u = () in let refine_ u = with_alloc h1 heads1 depth pool1 (refine_ facts1) rho1 model1 arrow desc value h2 (refine_ u) claim consume2 in refine_ u in
    Copy_model_proofs.describes_def rho var target;
    let u = () in let refine_ u = with_alloc h heads depth pool facts rho model p var target h1 (refine_ u) claim consume1 in refine_ u)


let rec (env_lookup @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    (i : D.index) @ immutable -> (s : template) @ immutable ->
    {u : unit | Hm_effective_environment.effective_env h heads depth env ts && template_lookup ts i === Some s} ->
    {u : unit | lookup env i === Some (root s)
      && Effective_template.valid_template h heads s && Effective_template.boundary_bound h heads depth s} @ ghost =
  fun h heads depth env ts i s premise -> ghost_ (
    let refine_ premise = premise in Hm_effective_environment.effective_env_def h heads depth env ts;
    lookup_def env i; template_lookup_def ts i;
    let u = () in match env with
    | Empty -> refine_ u
    | Bind (_, rest) -> match ts with No_templates -> refine_ u
      | Template_binding (_, tail) -> match i with D.Z -> refine_ u
        | D.S i -> env_lookup h heads depth rest tail i s (refine_ u); refine_ u)
