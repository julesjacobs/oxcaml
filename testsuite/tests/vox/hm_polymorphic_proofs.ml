open Copy_spec
open Level_spec
open Generalize_spec
open Hm_execution_spec
open Hm_runtime_spec
open Hm_environment_spec
module D = Hm_declarative
module T = Hm_type_proofs

let (with_variable_model @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
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
    {u : unit | env_at h depth env ts && aligned g ts && D.lookup g i === Some sigma
      && D.length args === D.arity sigma && valid h epoch depth d
      && (match lookup env i with None -> false | Some p -> target_for h d p q)} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === T.eval xi (D.open_scheme sigma args)} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h depth pool facts forest env ts g rho model xi realize i sigma args epoch d q premise claim use -> ghost_ (
    let refine_ premise = premise in let u = () in Hm_environment_proofs.aligned_lookup g ts i (refine_ u);
    match template_lookup ts i with None -> refine_ u | Some schema ->
    Hm_environment_proofs.env_lookup h depth env ts i schema (refine_ u);
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u in
    let trees : ((x : node Pref.t) @ immutable ->
      {s : template | not (H.mem h x) || (root s === x && template h s)} @ immutable) @ total = fun x ->
      let refine_ s = Hm_one_let_proofs.runtime_template h depth pool facts forest x in refine_ s in
    T.eval_arguments_length xi args; T.eval_open_scheme xi sigma args;
    let values = T.eval_arguments xi args in
    let consume_choices : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma values} ->
      {u : unit | claim}) @ total = fun choices fit ->
      let refine_ fit = fit in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau q === interpret rho choices schema} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
          let refine_ assigned = assigned in let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u in
      let u = () in let refine_ u = Hm_one_let_proofs.with_clean_instance h scope trees rho model choices epoch depth d schema q (refine_ u) claim consume in refine_ u in
    let refine_ u = realize i sigma schema values (refine_ u) claim consume_choices in refine_ u)

let (run_environment @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (ts : templates) @ immutable -> (e : execution) @ immutable ->
    (after : node Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && env_at h depth env ts} ->
    {u : unit | env_at after depth env ts} @ ghost = fun h depth pool facts env ts e after final_pool premise -> ghost_ (
    let refine_ premise = premise in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | protected_at h after depth x}) @ total = fun x ->
      let u = () in if H.mem h x then (
        Hm_protected_proofs.run_member h depth pool env e after final_pool depth x (refine_ u); refine_ u)
      else (facts x; runtime_at_def h depth pool x; safe_def h x;
        protected_at_def h after depth x; below_def h x depth; refine_ u) in
    let u = () in Hm_environment_proofs.env_transport h after depth frame env ts (refine_ u); refine_ u)

let (allocation_environment @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc && env_at h depth env ts} ->
    {u : unit | env_at (H.put h p (cell desc depth)) depth env ts} @ ghost =
  fun h depth pool facts env ts p desc premise -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc; let v = cell desc depth in let after = H.put h p v in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | protected_at h after depth x}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x;
      let u = () in Hm_environment_proofs.allocation_protected h p v depth x (refine_ u); refine_ u in
    let u = () in Hm_environment_proofs.env_transport h after depth frame env ts (refine_ u); refine_ u)

let (preserved_boundary @ total) : (h : node Pref.heap) @ immutable -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | env_at h depth env ts} ->
    {u : unit | not (environment_boundary ts x) || rho x === tau x} @ ghost =
  fun h depth env ts rho tau equal x premise -> ghost_ (
    let refine_ premise = premise in let u = () in if environment_boundary ts x then (
      Hm_environment_proofs.environment_boundary_owned h depth env ts x (refine_ u); equal x; refine_ u)
    else refine_ u)


let (bind_environment @ total) : (h : node Pref.heap) @ immutable -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable -> (g : D.context) @ immutable ->
    (p : node Pref.t) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | env_at h depth env ts && aligned g ts && below h p depth} ->
    {u : unit | env_at h depth (Bind (p, env)) (Template_binding (Boundary p, ts))
      && aligned (D.Binding (D.Forall (D.Z, a), g)) (Template_binding (Boundary p, ts))} @ ghost =
  fun h depth env ts g p a premise -> ghost_ (
    let refine_ premise = premise in let z = D.Z in let sigma = D.Forall (z, a) in
    let g1 = D.Binding (sigma, g) in let schema = Boundary p in let ts1 = Template_binding (schema, ts) in
    let env1 = Bind (p, env) in env_at_def h depth env1 ts1; aligned_def g1 ts1;
    root_def schema; template_def h schema; boundary_bound_def h depth schema;
    below_def h p depth; at_level_def h p; finite_node_def h p; let u = () in refine_ u)

let (node_model @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x}) @ total ghost =
  fun h rho model -> ghost_ (
    let out : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x}) @ total = fun x ->
      model x; equation_def h rho x; Level_unifier_spec.observe_def h x;
      Level_unifier_spec.node_equation_def h rho x; let u = () in refine_ u in out)

let (copy_model @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x})) @ total ->
    ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total ghost =
  fun h rho model -> ghost_ (
    let out : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      model x; Level_unifier_spec.node_equation_def h rho x; Level_unifier_spec.observe_def h x;
      equation_def h rho x; let u = () in refine_ u in out)


let rec (boundary_raise @ total) : (h : node Pref.heap) @ immutable -> (lo : int) -> (hi : int) ->
    (s : template) @ immutable -> {u : unit | lo <= hi && boundary_bound h lo s} ->
    {u : unit | boundary_bound h hi s} @ ghost = fun h lo hi s premise -> ghost_ (
    let refine_ premise = premise in boundary_bound_def h lo s; boundary_bound_def h hi s;
    let u = () in match s with Boundary p -> below_def h p lo; below_def h p hi; refine_ u
    | Parameter _ | Constant _ | Word_constant _ -> refine_ u
    | Product (_, a, b) -> boundary_raise h lo hi a (refine_ u); boundary_raise h lo hi b (refine_ u); refine_ u
    | Indirect (_, child) | List_template (_, child) -> boundary_raise h lo hi child (refine_ u); refine_ u)

let rec (environment_raise @ total) : (h : node Pref.heap) @ immutable -> (lo : int) -> (hi : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable -> {u : unit | lo <= hi && env_at h lo env ts} ->
    {u : unit | env_at h hi env ts} @ ghost = fun h lo hi env ts premise -> ghost_ (
    let refine_ premise = premise in env_at_def h lo env ts; env_at_def h hi env ts;
    let u = () in match env with Empty -> refine_ u | Bind (_, rest) -> match ts with No_templates -> refine_ u
    | Template_binding (s, tail) -> boundary_raise h lo hi s (refine_ u);
      environment_raise h lo hi rest tail (refine_ u); refine_ u)

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

let rec (with_run_model @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable)) @ total ->
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
    (n : D.index) @ immutable -> (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (target : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && env_at h depth env ts && aligned g ts
      && D.typed n g (source e) target d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | Hm_complete_proofs.matches tau e (T.eval xi target)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost =
  fun h depth pool facts forest env ts g rho model xi realize n e after final_pool target d premise claim use -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
    source_def e; result_def e; let term = source e in D.typed_def n g term target d;
    let value = T.eval xi target in T.eval_def xi target;
    let u = () in match e with
    | RLet_left (rhs, _) -> (match d with D.Let_binding (sigma, dr, db) ->
      (match sigma with D.Forall (k, a) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty x}) @ total = fun x ->
        facts x; let u = () in Hm_let_runtime_proofs.enter_runtime h depth pool x (refine_ u); refine_ u in
      environment_raise h depth child_depth env ts (refine_ u); aligned_weaken g ts k (refine_ u);
      let shifted = D.weaken_context k g in let rhs_n = D.add k n in
      let values = default_values k in default_values_length k;
      let[@def] zeta : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix values xi j in
      let valuation : ((j : D.index) @ immutable -> {u : unit | zeta j === T.prefix values xi j}) @ total = fun j ->
        zeta_def j; let u = () in refine_ u in
      let rhs_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i original schema args fit claim use ->
        let refine_ fit = fit in let u = () in
        let refine_ u = Hm_environment_proofs.realize_weaken g ts rho xi realize values zeta valuation i original schema args (refine_ u) claim use in refine_ u in
      let consume_rhs : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_complete_proofs.matches rho1 rhs (T.eval zeta a)} -> {u : unit | claim}) @ total = fun rho1 _model1 _equal1 fit1 ->
        let refine_ fit1 = fit1 in let rhs_value = T.eval zeta a in Hm_complete_proofs.matches_def rho1 rhs rhs_value;
        let u = () in refine_ u in
      let refine_ u = with_run_model h child_depth empty child_facts forest env ts shifted rho model zeta rhs_realize rhs_n rhs after final_pool a dr (refine_ u) claim consume_rhs in refine_ u)
      | _ -> refine_ u)
    | RLet (rhs, body, middle, child_pool) -> (match d with D.Let_binding (sigma, dr, db) ->
      (match sigma with D.Forall (k, a) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty x}) @ total = fun x ->
        facts x; let u = () in Hm_let_runtime_proofs.enter_runtime h depth pool x (refine_ u); refine_ u in
      environment_raise h depth child_depth env ts (refine_ u); aligned_weaken g ts k (refine_ u);
      let shifted = D.weaken_context k g in let rhs_n = D.add k n in
      let values = default_values k in default_values_length k;
      let[@def] zeta : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix values xi j in
      let valuation : ((j : D.index) @ immutable -> {u : unit | zeta j === T.prefix values xi j}) @ total = fun j ->
        zeta_def j; let u = () in refine_ u in
      let rhs_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i original schema args fit claim use ->
        let refine_ fit = fit in let u = () in
        let refine_ u = Hm_environment_proofs.realize_weaken g ts rho xi realize values zeta valuation i original schema args (refine_ u) claim use in refine_ u in
      let consume_rhs : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation middle rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_complete_proofs.matches rho1 rhs (T.eval zeta a)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in let rhs_value = T.eval zeta a in Hm_complete_proofs.matches_def rho1 rhs rhs_value;
        match result rhs with None -> refine_ u | Some original ->
        let closed = closed_heap middle depth child_pool in let transferred = Nested_pool_spec.transfer closed child_pool pool in
        let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle child_depth child_pool x}) @ total = fun x ->
          let u = () in Hm_let_runtime_proofs.run_runtime h child_depth empty child_facts env rhs middle child_pool x (refine_ u); refine_ u in
        let closed_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at closed depth transferred x}) @ total = fun x ->
          let u = () in Hm_let_runtime_proofs.close_runtime h depth pool facts env rhs middle child_pool (refine_ middle_facts) x (refine_ u); refine_ u in
        let middle_forest : ((x : node Pref.t) @ immutable ->
          {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
            (if H.mem middle x then Level_finite_spec.finite middle t else Level_unifier_spec.observe middle x === None)} @ immutable) @ total = fun x ->
          let u = () in let refine_ t = Hm_forest_proofs.run_forest h forest child_depth empty env rhs middle child_pool x (refine_ u) in refine_ t in
        let closed_forest : ((x : node Pref.t) @ immutable ->
          {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
            (if H.mem closed x then Level_finite_spec.finite closed t else Level_unifier_spec.observe closed x === None)} @ immutable) @ total = fun x ->
          let u = () in let refine_ t = Forest_transport.closed_forest_at middle middle_forest depth child_pool x (refine_ u) in refine_ t in
        Hm_execution_proofs.run_result h child_depth empty env rhs middle child_pool original (refine_ u);
        let refine_ finite_tree = middle_forest original in let tree = Forest_transport.unfolding finite_tree in
        Forest_transport.unfolding_valid middle finite_tree (refine_ u); Forest_transport.unfolding_root finite_tree;
        let schema = scheme middle depth tree in
        let coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered middle depth child_pool x}) @ total = fun x ->
          middle_facts x; runtime_at_def middle child_depth child_pool x; let u = () in refine_ u in
        let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered middle x}) @ total = fun x ->
          middle_facts x; runtime_at_def middle child_depth child_pool x; safe_def middle x; let u = () in refine_ u in
        Generalize_scheme_proofs.scheme_valid middle depth child_pool coverage tree (refine_ u);
        Generalize_scheme_proofs.scheme_root middle depth tree;
        Hm_one_let_proofs.scheme_boundary_bound middle depth child_pool order tree (refine_ u);
        let frame : ((x : node Pref.t) @ immutable -> {u : unit | protected_at h closed depth x}) @ total = fun x ->
          let u = () in if H.mem h x then (
            Hm_protected_proofs.run_member h child_depth empty env rhs middle child_pool depth x (refine_ u);
            Hm_environment_proofs.close_protected middle depth child_pool depth x (refine_ u);
            Hm_environment_proofs.protected_trans h middle closed depth x (refine_ u); refine_ u)
          else (facts x; runtime_at_def h depth pool x; safe_def h x;
            protected_at_def h closed depth x; below_def h x depth; refine_ u) in
        Hm_environment_proofs.env_transport h closed depth frame env ts (refine_ u);
        let next_env = Bind (original, env) in let next_ts = Template_binding (schema, ts) in let next_g = D.Binding (sigma, g) in
        env_at_def closed depth next_env next_ts; aligned_def next_g next_ts;
        let closed_model : ((x : node Pref.t) @ immutable -> {u : unit | equation closed rho1 x}) @ total = fun x ->
          model1 x; let u = () in Generalize_proofs.closed_model middle depth child_pool rho1 x (refine_ u); refine_ u in
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let u = () in let refine_ u = preserved_boundary h depth env ts rho rho1 equal1 x (refine_ u) in refine_ u in
        let old_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i old_sigma old_schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_transport g ts rho rho1 xi realize boundaries i old_sigma old_schema args (refine_ u) claim use in refine_ u in
        let body_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup next_g i === Some sigma && template_lookup next_ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i requested requested_schema args fit claim use ->
          let refine_ fit = fit in D.lookup_def next_g i; template_lookup_def next_ts i;
          let u = () in match i with
          | D.S i -> let refine_ u = old_realize i requested requested_schema args (refine_ u) claim use in refine_ u
          | D.Z -> D.arity_def sigma;
            let[@def] eta_xi : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix args xi j in
            let eta_valuation : ((j : D.index) @ immutable -> {u : unit | eta_xi j === T.prefix args xi j}) @ total = fun j ->
              eta_xi_def j; let u = () in refine_ u in
            let alternative_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning eta_xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun j old_sigma old_schema old_args fit claim use ->
              let refine_ fit = fit in let u = () in
              let refine_ u = Hm_environment_proofs.realize_weaken g ts rho xi realize args eta_xi eta_valuation
                j old_sigma old_schema old_args (refine_ u) claim use in refine_ u in
            let alternative : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle eta x})) @ total ->
              (eta_equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || eta x === rho x})) @ total ->
              {u : unit | Hm_complete_proofs.matches eta rhs (T.eval eta_xi a)} -> {u : unit | claim}) @ total = fun eta eta_model eta_equal assigned ->
                let refine_ assigned = assigned in let alternative_value = T.eval eta_xi a in
                Hm_complete_proofs.matches_def eta rhs alternative_value;
                let equal_saved : ((x : node Pref.t) @ immutable -> {u : unit | not (below h x depth) || rho1 x === eta x}) @ total = fun x ->
                  equal1 x; eta_equal x; below_def h x depth; let u = () in refine_ u in
                let u = () in Hm_origin_proofs.rhs_interpret h depth pool facts env rhs middle child_pool rho1 model1 eta eta_model equal_saved tree (refine_ u);
                T.eval_valuation args xi eta_xi eta_valuation a; T.meaning_def xi sigma args;
                let refine_ u = use eta (refine_ u) in refine_ u in
            let refine_ u = with_run_model h child_depth empty child_facts forest env ts shifted rho model eta_xi alternative_realize rhs_n rhs middle child_pool a dr (refine_ u) claim alternative in refine_ u in
        let consume_body : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem closed x) || tau x === rho1 x})) @ total ->
          {u : unit | Hm_complete_proofs.matches tau body (T.eval xi target)} -> {u : unit | claim}) @ total = fun tau next equal2 assigned ->
            let refine_ assigned = assigned in Hm_complete_proofs.matches_def tau body value; Hm_complete_proofs.matches_def tau e value;
            let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
              let u = () in Hm_execution_proofs.run_extends h child_depth empty env rhs middle child_pool x (refine_ u);
              Generalize_proofs.closed_observe middle depth child_pool x (refine_ u); closed_at_def middle closed depth child_pool x;
              equal1 x; equal2 x; refine_ u in
            let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u in
        let refine_ u = with_run_model closed depth transferred closed_facts closed_forest next_env next_ts next_g rho1 closed_model xi body_realize n body after final_pool target db (refine_ u) claim consume_body in refine_ u in
      let refine_ u = with_run_model h child_depth empty child_facts forest env ts shifted rho model zeta rhs_realize rhs_n rhs middle child_pool a dr (refine_ u) claim consume_rhs in refine_ u)
      | _ -> refine_ u)
    | RShared (i, q) -> (match d with
      | D.Variable args -> (match D.lookup g i with None -> refine_ u | Some sigma ->
        Hm_environment_proofs.aligned_lookup g ts i (refine_ u);
        match template_lookup ts i with None -> refine_ u | Some schema ->
        Hm_environment_proofs.env_lookup h depth env ts i schema (refine_ u);
        Hm_environment_proofs.active_template h schema q (refine_ u);
        T.eval_arguments_length xi args; T.eval_open_scheme xi sigma args;
        let values = T.eval_arguments xi args in
        let consume : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          {u : unit | interpret rho choices schema === T.meaning xi sigma values} ->
          {u : unit | claim}) @ total = fun choices fit ->
          let refine_ fit = fit in interpret_def rho choices schema;
          let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho x === rho x}) @ total = fun x -> let u = () in refine_ u in
          Hm_complete_proofs.matches_def rho e value;
          let u = () in let refine_ u = use rho (refine_ model) equal (refine_ u) in refine_ u in
        let refine_ u = realize i sigma schema values (refine_ u) claim consume in refine_ u)
      | _ -> refine_ u)
    | RVar (i, q, epoch, history) -> (match d with
      | D.Variable args -> (match D.lookup g i with None -> refine_ u | Some sigma ->
        let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth history) tau x})) @ total ->
          (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
          {u : unit | tau q === T.eval xi (D.open_scheme sigma args)} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
            let refine_ assigned = assigned in Hm_complete_proofs.matches_def tau e value;
            let u = () in let refine_ u = use tau (refine_ next) equal (refine_ u) in refine_ u in
        let refine_ u = with_variable_model h depth pool facts (refine_ forest) env ts g rho model xi realize
          i sigma args epoch history q (refine_ u) claim consume in refine_ u)
      | _ -> refine_ u)
    | RBool p -> (match d with D.Constant ->
      let desc : desc = Bool in Copy_model_proofs.describes_def rho desc value;
      let nodes = node_model h rho model in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau p === value} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
          let refine_ assigned = assigned in let converted = copy_model after tau next in
          Hm_complete_proofs.matches_def tau e value;
          let u = () in let refine_ u = use tau converted equal (refine_ u) in refine_ u in
      let refine_ u = Hm_complete_proofs.with_alloc h depth pool facts rho nodes p desc value after (refine_ u) claim consume in refine_ u
      | _ -> refine_ u)
    | RLam (arg, body, middle, body_pool, out) -> (match d with D.Abstraction (at, db) ->
      (match target with D.Function (a, b) ->
      let av = T.eval xi a in let bv = T.eval xi b in let var : desc = Var in let v = cell var depth in
      let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      let z = D.Z in let sigma = D.Forall (z, a) in let g1 = D.Binding (sigma, g) in
      let boundary = Boundary arg in let ts1 = Template_binding (boundary, ts) in
      let facts1 = Hm_complete_proofs.allocation_facts h depth pool facts arg var (refine_ u) in
      let forest1 = Hm_forest_proofs.allocated_forest h forest depth arg var (refine_ u) in
      allocation_environment h depth pool facts env ts arg var (refine_ u);
      cell_def var depth; below_def h1 arg depth; at_level_def h1 arg;
      bind_environment h1 depth env ts g arg a (refine_ u);
      Copy_model_proofs.describes_def rho var av; let nodes = node_model h rho model in
      let consume_arg : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | rho1 arg === av} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 assigned ->
        let refine_ assigned = assigned in let model1 = copy_model h1 rho1 model1 in

        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let u = () in let refine_ u = preserved_boundary h depth env ts rho rho1 equal1 x (refine_ u) in refine_ u in
        let realized : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args (refine_ u) claim use in refine_ u in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g1 i === Some sigma && template_lookup ts1 i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_monomorphic g ts rho1 xi realized arg a (refine_ u)
            i sigma schema args (refine_ u) claim use in refine_ u in
        let consume_body : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation middle rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_complete_proofs.matches rho2 body (T.eval xi b)} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit ->
          let refine_ fit = fit in Hm_complete_proofs.matches_def rho2 body bv;
          match result body with None -> refine_ u | Some body_root -> match out with None -> refine_ u | Some p ->
          equal2 arg; let desc = Arrow (arg, body_root) in Copy_model_proofs.describes_def rho2 desc value;
          let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth body_pool x}) @ total = fun x ->
            let u = () in Hm_let_runtime_proofs.run_runtime h1 depth pool1 (refine_ facts1) env1 body middle body_pool x (refine_ u); refine_ u in
          let equal12 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho2 x === rho x}) @ total = fun x ->
            equal1 x; equal2 x; let u = () in refine_ u in
          let nodes2 = node_model middle rho2 model2 in
          let consume_arrow : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho3 x})) @ total ->
            (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || rho3 x === rho2 x})) @ total ->
            {u : unit | rho3 p === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 assigned3 ->
              let refine_ assigned3 = assigned3 in
              let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
                let u = () in Hm_execution_proofs.run_extends h1 depth pool1 env1 body middle body_pool x (refine_ u);
                equal12 x; equal3 x; refine_ u in
              let converted = copy_model after rho3 model3 in Hm_complete_proofs.matches_def rho3 e value;
              let u = () in let refine_ u = use rho3 converted equal (refine_ u) in refine_ u in
          let refine_ u = Hm_complete_proofs.with_alloc middle depth body_pool middle_facts rho2 nodes2 p desc value after (refine_ u) claim consume_arrow in refine_ u in
        let refine_ u = with_run_model h1 depth pool1 (refine_ facts1) (refine_ forest1) env1 ts1 g1 rho1 model1 xi realize1 n body middle body_pool b db (refine_ u) claim consume_body in refine_ u in
      let refine_ u = Hm_complete_proofs.with_alloc h depth pool facts rho nodes arg var av h1 (refine_ u) claim consume_arg in refine_ u
      | _ -> refine_ u)
      | _ -> refine_ u)
    | RApp_left (left, _) -> (match d with D.Application (at, dl, _) ->
      let ft = D.Function (at, target) in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | Hm_complete_proofs.matches tau left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun tau _next _equal fit ->
          let refine_ fit = fit in let fv = T.eval xi ft in Hm_complete_proofs.matches_def tau left fv; let u = () in refine_ u in
      let refine_ u = with_run_model h depth pool facts forest env ts g rho model xi realize n left after final_pool ft dl (refine_ u) claim consume in refine_ u
      | _ -> refine_ u)
    | RApp_right (left, right, h1, pool1) -> (match d with D.Application (at, dl, dr) ->
      let ft = D.Function (at, target) in let fv = T.eval xi ft in let av = T.eval xi at in T.eval_def xi ft;
      run_environment h depth pool facts env ts left h1 pool1 (refine_ u);
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total = fun x ->
        let u = () in Hm_let_runtime_proofs.run_runtime h depth pool facts env left h1 pool1 x (refine_ u); refine_ u in
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h1 x then Level_finite_spec.finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h forest depth pool env left h1 pool1 x (refine_ u) in refine_ t in
      let consume_left : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_complete_proofs.matches rho1 left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in Hm_complete_proofs.matches_def rho1 left fv;
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let u = () in let refine_ u = preserved_boundary h depth env ts rho rho1 equal1 x (refine_ u) in refine_ u in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args (refine_ u) claim use in refine_ u in
        let consume_right : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_complete_proofs.matches rho2 right (T.eval xi at)} -> {u : unit | claim}) @ total = fun rho2 _model2 _equal2 fit2 ->
          let refine_ fit2 = fit2 in Hm_complete_proofs.matches_def rho2 right av;
          let u = () in refine_ u in
        let refine_ u = with_run_model h1 depth pool1 facts1 forest1 env ts g rho1 model1 xi realize1 n right after final_pool at dr (refine_ u) claim consume_right in refine_ u in
      let refine_ u = with_run_model h depth pool facts forest env ts g rho model xi realize n left h1 pool1 ft dl (refine_ u) claim consume_left in refine_ u
      | _ -> refine_ u)
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, derivation) -> (match d with D.Application (at, dl, dr) ->
      let ft = D.Function (at, target) in let fv = T.eval xi ft in let av = T.eval xi at in T.eval_def xi ft;
      run_environment h depth pool facts env ts left h1 pool1 (refine_ u);
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 x}) @ total = fun x ->
        let u = () in Hm_let_runtime_proofs.run_runtime h depth pool facts env left h1 pool1 x (refine_ u); refine_ u in
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h1 x then Level_finite_spec.finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest h forest depth pool env left h1 pool1 x (refine_ u) in refine_ t in
      let consume_left : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_complete_proofs.matches rho1 left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in Hm_complete_proofs.matches_def rho1 left fv;
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let u = () in let refine_ u = preserved_boundary h depth env ts rho rho1 equal1 x (refine_ u) in refine_ u in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args (refine_ u) claim use in refine_ u in
        let consume_right : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation h2 rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_complete_proofs.matches rho2 right (T.eval xi at)} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
          let refine_ fit2 = fit2 in Hm_complete_proofs.matches_def rho2 right av;
          match result left with None -> refine_ u | Some f -> match result right with None -> refine_ u | Some a ->
          Hm_execution_proofs.run_result h depth pool env left h1 pool1 f (refine_ u);
          Hm_execution_proofs.run_extends h1 depth pool1 env right h2 pool2 f (refine_ u);
          Hm_execution_proofs.run_result h1 depth pool1 env right h2 pool2 a (refine_ u); equal2 f;
          let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 x}) @ total = fun x ->
            let u = () in Hm_let_runtime_proofs.run_runtime h1 depth pool1 facts1 env right h2 pool2 x (refine_ u); refine_ u in
          let nodes2 = node_model h2 rho2 model2 in
          let consume_application : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho3 x})) @ total ->
            (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
            {u : unit | ok && rho3 p === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 assigned ->
              let refine_ assigned = assigned in
              let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
                let u = () in Hm_execution_proofs.run_extends h depth pool env left h1 pool1 x (refine_ u);
                Hm_execution_proofs.run_extends h1 depth pool1 env right h2 pool2 x (refine_ u);
                equal1 x; equal2 x; equal3 x; refine_ u in
              let converted = copy_model after rho3 model3 in Hm_complete_proofs.matches_def rho3 e value;
              let u = () in let refine_ u = use rho3 converted equal (refine_ u) in refine_ u in
          let refine_ u = Hm_one_let_proofs.with_application_model h2 depth pool2 facts2 rho2 nodes2 f a p arrow ok after derivation value (refine_ u) claim consume_application in refine_ u in
        let refine_ u = with_run_model h1 depth pool1 facts1 forest1 env ts g rho1 model1 xi realize1 n right h2 pool2 at dr (refine_ u) claim consume_right in refine_ u in
      let refine_ u = with_run_model h depth pool facts forest env ts g rho model xi realize n left h1 pool1 ft dl (refine_ u) claim consume_left in refine_ u
      | _ -> refine_ u)
    | RRec (arg, res, self, body, middle, body_pool, finish) -> (match d with D.Recursion (a, b, db) ->
      let av = T.eval xi a in let bv = T.eval xi b in
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
      let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in let pool3 = Entry (self, pool2) in
      let self_env = Bind (self, env) in let env3 = Bind (arg, self_env) in
      let z = D.Z in let self_sigma = D.Forall (z, target) in let arg_sigma = D.Forall (z, a) in
      let self_g = D.Binding (self_sigma, g) in let g3 = D.Binding (arg_sigma, self_g) in
      let self_schema = Boundary self in let arg_schema = Boundary arg in
      let self_ts = Template_binding (self_schema, ts) in let ts3 = Template_binding (arg_schema, self_ts) in
      let facts1 = Hm_complete_proofs.allocation_facts h depth pool facts arg var (refine_ u) in
      let facts2 = Hm_complete_proofs.allocation_facts h1 depth pool1 (refine_ facts1) res var (refine_ u) in
      let facts3 = Hm_complete_proofs.allocation_facts h2 depth pool2 (refine_ facts2) self desc (refine_ u) in
      let forest1 = Hm_forest_proofs.allocated_forest h forest depth arg var (refine_ u) in
      let forest2 = Hm_forest_proofs.allocated_forest h1 (refine_ forest1) depth res var (refine_ u) in
      let forest3 = Hm_forest_proofs.allocated_forest h2 (refine_ forest2) depth self desc (refine_ u) in
      allocation_environment h depth pool facts env ts arg var (refine_ u);
      allocation_environment h1 depth pool1 (refine_ facts1) env ts res var (refine_ u);
      allocation_environment h2 depth pool2 (refine_ facts2) env ts self desc (refine_ u);
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      cell_def var depth; cell_def desc depth;
      below_def h3 arg depth; at_level_def h3 arg; below_def h3 self depth; at_level_def h3 self;
      bind_environment h3 depth env ts g self target (refine_ u);
      bind_environment h3 depth self_env self_ts self_g arg a (refine_ u);
      let nodes = node_model h rho model in Copy_model_proofs.describes_def rho var av;
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | rho1 arg === av} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let refine_ fit1 = fit1 in Copy_model_proofs.describes_def rho1 var bv;
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model2 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h2 rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
        {u : unit | rho2 res === bv} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
        let refine_ fit2 = fit2 in equal2 arg; Copy_model_proofs.describes_def rho2 desc value;
      let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h3 rho3 x})) @ total ->
        (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
        {u : unit | rho3 self === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit3 ->
        let refine_ fit3 = fit3 in equal3 arg; equal3 res;
        let old_equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
          equal1 x; equal2 x; equal3 x; let u = () in refine_ u in
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho3 x}) @ total = fun x ->
          let u = () in let refine_ u = preserved_boundary h depth env ts rho rho3 old_equal x (refine_ u) in refine_ u in
        let realized : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_transport g ts rho rho3 xi realize boundaries i sigma schema args (refine_ u) claim use in refine_ u in
        let self_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup self_g i === Some sigma && template_lookup self_ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_monomorphic g ts rho3 xi realized self target (refine_ u)
            i sigma schema args (refine_ u) claim use in refine_ u in
        let realize3 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g3 i === Some sigma && template_lookup ts3 i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let refine_ fit = fit in let u = () in
          let refine_ u = Hm_environment_proofs.realize_monomorphic self_g self_ts rho3 xi self_realize arg a (refine_ u)
            i sigma schema args (refine_ u) claim use in refine_ u in
      let consume_body : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (body_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle tau x})) @ total ->
        (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total ->
        {u : unit | Hm_complete_proofs.matches tau body (T.eval xi b)} -> {u : unit | claim}) @ total = fun tau body_model equal4 fit ->
        let refine_ fit = fit in Hm_complete_proofs.matches_def tau body bv;
        match result body with None -> refine_ u | Some root -> match finish with Aborted -> refine_ u | Unified (ok, derivation) ->
        equal4 res; equal4 self; let body_nodes = node_model middle tau body_model in
        let u = () in Hm_complete_proofs.unify_complete middle tau body_nodes root res ok after derivation root (refine_ u);
        let next_nodes : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x}) @ total = fun x ->
          let u = () in let refine_ u = Hm_complete_proofs.unify_complete middle tau body_nodes root res ok after derivation x (refine_ u) in refine_ u in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
          old_equal x; equal4 x; let u = () in refine_ u in
        let next = copy_model after tau next_nodes in Hm_complete_proofs.matches_def tau e value;
        let refine_ u = use tau next equal (refine_ u) in refine_ u in
        let model3 = copy_model h3 rho3 model3 in
        let refine_ u = with_run_model h3 depth pool3 (refine_ facts3) (refine_ forest3) env3 ts3 g3 rho3 model3 xi realize3 n body middle body_pool b db (refine_ u) claim consume_body in refine_ u in
        let refine_ u = Hm_complete_proofs.with_alloc h2 depth pool2 (refine_ facts2) rho2 model2 self desc value h3 (refine_ u) claim consume3 in refine_ u in
        let refine_ u = Hm_complete_proofs.with_alloc h1 depth pool1 (refine_ facts1) rho1 model1 res var bv h2 (refine_ u) claim consume2 in refine_ u in
      let refine_ u = Hm_complete_proofs.with_alloc h depth pool facts rho nodes arg var av h1 (refine_ u) claim consume1 in refine_ u
      | _ -> refine_ u))

let (with_closed_model @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} -> (claim : bool) ->
    (use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_complete_proofs.matches rho e target} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool target d premise claim use -> ghost_ (
    let refine_ premise = premise in let h = H.empty () in let z = D.Z in
    let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
    let ts = No_templates in let g = D.Empty_context in
    let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 empty x}) @ total = fun x ->
      runtime_at_def h 0 empty x; safe_def h x; depth_bound_def h 0 x; covered_def h (-1) empty x;
      ordered_def h x; let u = () in refine_ u in
    let forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable) @ total = fun x ->
      let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t; Level_unifier_spec.observe_def h x; refine_ t in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      equation_def h rho x; let u = () in refine_ u in
    let[@def] xi : D.index @ immutable total -> ty @ immutable total = fun _i -> Boolean in
    let realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
        let refine_ fit = fit in let u = () in
        let refine_ u = Hm_environment_proofs.realize_empty rho xi i sigma schema args (refine_ u) claim use in refine_ u in
    env_at_def h 0 env ts; aligned_def g ts;
    T.eval_embed xi target; let mono = D.embed target in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | Hm_complete_proofs.matches tau e (T.eval xi mono)} -> {u : unit | claim}) @ total = fun tau next _equal fit ->
        let refine_ fit = fit in let u = () in let refine_ u = use tau next (refine_ u) in refine_ u in
    let u = () in let refine_ u = with_run_model h 0 empty facts forest env ts g rho model xi realize z e after pool mono d (refine_ u) claim consume in refine_ u)

let (closed_completes @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} ->
    {u : unit | not (result e === None)} @ ghost = fun e after pool target d premise -> ghost_ (
    let refine_ premise = premise in let claim = not (result e === None) in
    let use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_complete_proofs.matches rho e target} -> {u : unit | claim}) @ total = fun rho _model fit ->
        let refine_ fit = fit in Hm_complete_proofs.matches_def rho e target; let u = () in refine_ u in
    let u = () in let refine_ u = with_closed_model e after pool target d (refine_ u) claim use in refine_ u)

let (closed_reject @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === None && D.typed D.Z D.Empty_context (source e) (D.embed target) d} ->
    {u : unit | false} @ ghost = fun e after pool target d premise -> ghost_ (
    let refine_ premise = premise in let u = () in closed_completes e after pool target d (refine_ u); refine_ u)

let (closed_factor @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree : Level_finite_spec.tree) @ immutable ->
    (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === Some p && Level_finite_spec.finite after tree && Level_finite_spec.tree_root tree === p
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool p tree target d premise claim use -> ghost_ (
    let refine_ premise = premise in
    let consume : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_complete_proofs.matches rho e target} -> {u : unit | claim}) @ total = fun rho model fit ->
        let refine_ fit = fit in Hm_complete_proofs.matches_def rho e target; let nodes = node_model after rho model in
        let u = () in Level_mgu_proofs.readback_factor after rho nodes tree (refine_ u);
        let refine_ u = use rho (refine_ u) in refine_ u in
    let u = () in let refine_ u = with_closed_model e after pool target d (refine_ u) claim consume in refine_ u)
