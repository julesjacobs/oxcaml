open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_effective_execution_spec
open Hm_effective_runtime
open Hm_environment_spec
open Hm_effective_environment
open Hm_effective_complete_helpers
module D = Hm_declarative
module T = Hm_type_proofs
module E = Effective_level
module A = Hm_effective_allocation
module C = Hm_effective_environment_proofs
let (preserved_boundary @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_env h heads depth env ts} ->
    {u : unit | not (environment_boundary ts x) || rho x === tau x} @ ghost =
  fun h heads depth env ts rho tau equal x premise -> ghost_ (
    if environment_boundary ts x then (
      C.environment_boundary_owned h heads depth env ts x (); equal x; ())
    else ())


let (bind_environment @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable -> (g : D.context) @ immutable ->
    (p : node Pref.t) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | effective_env h heads depth env ts && aligned g ts && E.effective_below h heads p depth} ->
    {u : unit | effective_env h heads depth (Bind (p, env)) (Template_binding (Boundary p, ts))
      && aligned (D.Binding (D.Forall (D.Z, a), g)) (Template_binding (Boundary p, ts))} @ ghost =
  fun h heads depth env ts g p a premise -> ghost_ (
    let z = D.Z in let sigma = D.Forall (z, a) in
    let g1 = D.Binding (sigma, g) in let schema = Boundary p in let ts1 = Template_binding (schema, ts) in
    let env1 = Bind (p, env) in effective_env_def h heads depth env1 ts1; aligned_def g1 ts1;
    root_def schema; Effective_template.valid_template_def h heads schema; Effective_template.boundary_bound_def h heads depth schema;
    E.effective_below_def h heads p depth; at_level_def h p; Effective_template.finite_def h heads p; ())

let rec (with_run_model @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads depth pool x})) @ total ->
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
    (n : D.index) @ immutable -> (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (target : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && effective_env h heads depth env ts && aligned g ts
      && D.typed n g (source e) target d} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | Hm_effective_complete_helpers.matches tau e (T.eval xi target)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost =
  fun h heads depth pool facts forest env ts g rho model xi realize n e after final_pool target d premise claim use -> ghost_ (
    ran_def h depth pool env e after final_pool;
    source_def e; result_def e; let term = source e in D.typed_def n g term target d;
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; () in
    let value = T.eval xi target in T.eval_def xi target;
    match e with
    | RLet_left (rhs, _) -> (match d with D.Let_binding (sigma, dr, db) ->
      (match sigma with D.Forall (k, a) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads child_depth empty x}) @ total = fun x ->
        facts x; enter_runtime h heads depth pool x (); () in
      C.environment_raise h heads depth child_depth env ts (); aligned_weaken g ts k ();
      let shifted = D.weaken_context k g in let rhs_n = D.add k n in
      let values = default_values k in default_values_length k;
      let[@def] zeta : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix values xi j in
      let valuation : ((j : D.index) @ immutable -> {u : unit | zeta j === T.prefix values xi j}) @ total = fun j ->
        zeta_def j; () in
      let rhs_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i original schema args fit claim use ->
        let () = Hm_environment_models.realize_weaken g ts rho xi realize values zeta valuation i original schema args () claim use in () in
      let consume_rhs : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches rho1 rhs (T.eval zeta a)} -> {u : unit | claim}) @ total = fun rho1 _model1 _equal1 fit1 ->
        let rhs_value = T.eval zeta a in Hm_effective_complete_helpers.matches_def rho1 rhs rhs_value;
        () in
      let () = with_run_model h heads child_depth empty child_facts forest env ts shifted rho model zeta rhs_realize rhs_n rhs after final_pool a dr () claim consume_rhs in ())
      | _ -> ())
    | RLet (rhs, body, middle, child_pool) -> (match d with D.Let_binding (sigma, dr, db) ->
      (match sigma with D.Forall (k, a) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads child_depth empty x}) @ total = fun x ->
        facts x; enter_runtime h heads depth pool x (); () in
      C.environment_raise h heads depth child_depth env ts (); aligned_weaken g ts k ();
      let shifted = D.weaken_context k g in let rhs_n = D.add k n in
      let values = default_values k in default_values_length k;
      let[@def] zeta : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix values xi j in
      let valuation : ((j : D.index) @ immutable -> {u : unit | zeta j === T.prefix values xi j}) @ total = fun j ->
        zeta_def j; () in
      let rhs_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i original schema args fit claim use ->
        let () = Hm_environment_models.realize_weaken g ts rho xi realize values zeta valuation i original schema args () claim use in () in
      let consume_rhs : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation middle rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches rho1 rhs (T.eval zeta a)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        let rhs_value = T.eval zeta a in Hm_effective_complete_helpers.matches_def rho1 rhs rhs_value;
        match result rhs with None -> () | Some original ->
        let closed = Representative_pool_spec.close_heap middle depth child_pool in let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
        let middle_forest : ((x : node Pref.t) @ immutable ->
          {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
            (if H.mem middle x then Level_finite_spec.finite middle t else Level_unifier_spec.observe middle x === None)} @ immutable) @ total = fun x ->
          let t = Hm_effective_forest.run_forest h forest child_depth empty env rhs middle child_pool x () in t in
      let[@def] middle_heads : E.heads = fun x -> let r = Forest_heads.select middle middle_forest x in r in
      let middle_valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads x}) @ total = fun x ->
        middle_heads_def x; let _r = Forest_heads.select middle middle_forest x in E.valid_head_def middle middle_heads x;
        () in
              let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads child_depth child_pool x}) @ total = fun x ->
          Hm_effective_invariant.run_invariant h heads forest child_depth empty child_facts env rhs middle middle_heads middle_valid child_pool x (); () in
        let closed_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at closed middle_heads depth transferred x}) @ total = fun x ->
          Hm_effective_closing.close_after_run h heads middle_heads depth pool facts env rhs middle child_pool (refine_ middle_facts) x (); () in
        Representative_pool_spec.close_heap_def middle depth child_pool;
        let filtered = Representative_level.representatives middle child_pool in
        Representative_level.representatives_scoped middle child_pool ();
        let closed_forest : ((x : node Pref.t) @ immutable ->
          {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
            (if H.mem closed x then Level_finite_spec.finite closed t else Level_unifier_spec.observe closed x === None)} @ immutable) @ total = fun x ->
          let t = Forest_transport.closed_forest_at middle middle_forest depth filtered x () in t in
        Hm_effective_driver_proofs.run_result h forest child_depth empty env rhs middle child_pool original ();
        let finite_tree = middle_forest original in let tree = finite_tree in
        Forest_transport.unfolding_valid middle finite_tree (); Forest_transport.unfolding_root finite_tree;
        let schema = Effective_template.scheme middle middle_heads depth tree in
        let coverage : ((x : node Pref.t) @ immutable -> {u : unit | Representative_level.representative_covered middle depth child_pool x}) @ total = fun x ->
          middle_facts x; runtime_at_def middle middle_heads child_depth child_pool x; () in
        let order : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads x && E.effective_ordered middle middle_heads x && (match E.level middle middle_heads x with Generic -> true | Finite n -> n >= 0)}) @ total = fun x ->
          middle_facts x; runtime_at_def middle middle_heads child_depth child_pool x; safe_def middle middle_heads x; depth_bound_def middle middle_heads child_depth x; E.effective_below_def middle middle_heads x child_depth; E.level_def middle middle_heads x; () in
        Effective_template.scheme_valid middle middle_heads middle_valid depth child_pool coverage tree ();
        Effective_template.scheme_root middle middle_heads depth tree;
        Effective_template.scheme_boundary middle middle_heads depth child_pool middle_valid (refine_ order) tree ();
        C.run_environment h heads middle_heads valid child_depth empty env ts rhs middle child_pool middle_valid depth ();
        let close_frame : ((x : node Pref.t) @ immutable ->
          {u : unit | Effective_template.protected middle middle_heads closed middle_heads depth x}) @ total = fun x ->
          middle_valid x; Effective_template.close_protected middle middle_heads depth child_pool depth x (); () in
        transport middle middle_heads closed middle_heads depth close_frame env ts ();
        let next_env = Bind (original, env) in let next_ts = Template_binding (schema, ts) in let next_g = D.Binding (sigma, g) in
        effective_env_def closed middle_heads depth next_env next_ts; aligned_def next_g next_ts;
        let closed_model : ((x : node Pref.t) @ immutable -> {u : unit | equation closed rho1 x}) @ total = fun x ->
          model1 x; Generalize_proofs.closed_model middle depth filtered rho1 x (); () in
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let () = preserved_boundary h heads depth env ts rho rho1 equal1 x () in () in
        let old_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i old_sigma old_schema args fit claim use ->
          let () = Hm_environment_models.realize_transport g ts rho rho1 xi realize boundaries i old_sigma old_schema args () claim use in () in
        let body_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup next_g i === Some sigma && template_lookup next_ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i requested requested_schema args fit claim use ->
          D.lookup_def next_g i; template_lookup_def next_ts i;
          match i with
          | D.S i -> let () = old_realize i requested requested_schema args () claim use in ()
          | D.Z -> D.arity_def sigma;
            let[@def] eta_xi : D.index @ immutable total -> ty @ immutable total = fun j -> T.prefix args xi j in
            let eta_valuation : ((j : D.index) @ immutable -> {u : unit | eta_xi j === T.prefix args xi j}) @ total = fun j ->
              eta_xi_def j; () in
            let alternative_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup shifted i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning eta_xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun j old_sigma old_schema old_args fit claim use ->
              let () = Hm_environment_models.realize_weaken g ts rho xi realize args eta_xi eta_valuation
                j old_sigma old_schema old_args () claim use in () in
            let alternative : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle eta x})) @ total ->
              (eta_equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || eta x === rho x})) @ total ->
              {u : unit | Hm_effective_complete_helpers.matches eta rhs (T.eval eta_xi a)} -> {u : unit | claim}) @ total = fun eta eta_model eta_equal assigned ->
                let alternative_value = T.eval eta_xi a in
                Hm_effective_complete_helpers.matches_def eta rhs alternative_value;
                let equal_saved : ((x : node Pref.t) @ immutable -> {u : unit | not (below h x depth) || rho1 x === eta x}) @ total = fun x ->
                  equal1 x; eta_equal x; below_def h x depth; () in
                Hm_effective_origin.rhs_interpret h forest depth env rhs middle child_pool middle_heads order rho1 model1 eta eta_model equal_saved tree ();
                T.eval_valuation args xi eta_xi eta_valuation a; T.meaning_def xi sigma args;
                let () = use eta () in () in
            let () = with_run_model h heads child_depth empty child_facts forest env ts shifted rho model eta_xi alternative_realize rhs_n rhs middle child_pool a dr () claim alternative in () in
        let consume_body : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem closed x) || tau x === rho1 x})) @ total ->
          {u : unit | Hm_effective_complete_helpers.matches tau body (T.eval xi target)} -> {u : unit | claim}) @ total = fun tau next equal2 assigned ->
            Hm_effective_complete_helpers.matches_def tau body value; Hm_effective_complete_helpers.matches_def tau e value;
            let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
              Hm_effective_membership.run_extends h child_depth empty env rhs middle child_pool x ();
              Generalize_proofs.closed_observe middle depth filtered x (); closed_at_def middle closed depth filtered x;
              equal1 x; equal2 x; () in
            let () = use tau next equal () in () in
        let () = with_run_model closed middle_heads depth transferred closed_facts closed_forest next_env next_ts next_g rho1 closed_model xi body_realize n body after final_pool target db () claim consume_body in () in
      let () = with_run_model h heads child_depth empty child_facts forest env ts shifted rho model zeta rhs_realize rhs_n rhs middle child_pool a dr () claim consume_rhs in ())
      | _ -> ())
    | RShared (i, q, rep) -> (match d with
      | D.Variable args -> (match D.lookup g i with None -> () | Some sigma ->
        Hm_environment_models.aligned_lookup g ts i ();
        match template_lookup ts i with None -> () | Some schema ->
        env_lookup h heads depth env ts i schema ();
        valid q; resolves_def h q rep.root rep.path;
        E.valid_head_def h heads q; let actual = heads q in
        Representative_level.unique h q rep.root rep.path actual.root actual.path ();
        E.level_def h heads q; active_def h rep.root; at_level_def h rep.root;
        Effective_template.valid_template_def h heads schema; root_def schema;
        (match schema with Boundary _ -> () | Parameter p | Constant p | Product (p, _, _) | Indirect (p, _) ->
          Effective_template.generic_def h heads p; ());
        T.eval_arguments_length xi args; T.eval_open_scheme xi sigma args;
        let values = T.eval_arguments xi args in
        let consume : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          {u : unit | interpret rho choices schema === T.meaning xi sigma values} ->
          {u : unit | claim}) @ total = fun choices fit ->
          interpret_def rho choices schema;
          let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho x === rho x}) @ total = fun x -> () in
          Hm_effective_complete_helpers.matches_def rho e value;
          let () = use rho (refine_ model) equal () in () in
        let () = realize i sigma schema values () claim consume in ())
      | _ -> ())
    | RVar (i, q, epoch, history, certificate) -> (match d with
      | D.Variable args -> (match D.lookup g i with None -> () | Some sigma ->
        let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth history) tau x})) @ total ->
          (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
          {u : unit | tau q === T.eval xi (D.open_scheme sigma args)} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
            Hm_effective_complete_helpers.matches_def tau e value;
            let () = use tau (refine_ next) equal () in () in
        (match lookup env i with None -> () | Some original ->
          Copy_certificate_spec.certifies_def h certificate epoch depth history original q; ());
        let () = with_variable_model h heads certificate depth pool facts (refine_ forest) env ts g rho model xi realize
          i sigma args epoch history q () claim consume in ())
      | _ -> ())
    | RBool p -> (match d with D.Constant ->
      let desc : desc = Bool in Copy_model_proofs.describes_def rho desc value;
      let nodes = node_model h rho model in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau p === value} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
          let converted = copy_model after tau next in
          Hm_effective_complete_helpers.matches_def tau e value;
          let () = use tau converted equal () in () in
      let () = Hm_effective_complete_helpers.with_alloc h heads depth pool facts rho nodes p desc value after () claim consume in ()
      | _ -> ())
    | RLam (arg, body, middle, body_pool, out) -> (match d with D.Abstraction (at, db) ->
      (match target with D.Function (a, b) ->
      let av = T.eval xi a in let bv = T.eval xi b in let var : desc = Var in let v = cell var depth in
      let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      let z = D.Z in let sigma = D.Forall (z, a) in let g1 = D.Binding (sigma, g) in
      let boundary = Boundary arg in let ts1 = Template_binding (boundary, ts) in
      allocated_def h depth arg var;
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let trees = Hm_effective_forest.allocated_forest h forest depth arg var () in let t = trees x in t in
      let[@def] heads1 : E.heads = fun x -> let r = Forest_heads.select h1 forest1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let _r = Forest_heads.select h1 forest1 x in E.valid_head_def h1 heads1 x;
        () in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
        facts x; A.allocate_runtime h heads heads1 depth pool arg var valid (refine_ valid1) x (); () in
            C.allocation_environment h heads heads1 valid arg v (refine_ valid1) depth env ts ();
      cell_def var depth; valid1 arg; A.allocated_below h depth arg var heads1 ();
      bind_environment h1 heads1 depth env ts g arg a ();
      Copy_model_proofs.describes_def rho var av; let nodes = node_model h rho model in
      let consume_arg : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | rho1 arg === av} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 assigned ->
        let model1 = copy_model h1 rho1 model1 in

        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let () = preserved_boundary h heads depth env ts rho rho1 equal1 x () in () in
        let realized : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args () claim use in () in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g1 i === Some sigma && template_lookup ts1 i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_monomorphic g ts rho1 xi realized arg a ()
            i sigma schema args () claim use in () in
        let consume_body : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation middle rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_effective_complete_helpers.matches rho2 body (T.eval xi b)} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit ->
          Hm_effective_complete_helpers.matches_def rho2 body bv;
          match result body with None -> () | Some body_root -> match out with None -> () | Some p ->
          equal2 arg; let desc = Arrow (arg, body_root) in Copy_model_proofs.describes_def rho2 desc value;
          let middle_forest : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.run_forest h1 forest1 depth pool1 env1 body middle body_pool x () in t in
      let[@def] middle_heads : E.heads = fun x -> let r = Forest_heads.select middle middle_forest x in r in
      let middle_valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads x}) @ total = fun x ->
        middle_heads_def x; let _r = Forest_heads.select middle middle_forest x in E.valid_head_def middle middle_heads x;
        () in
                let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads depth body_pool x}) @ total = fun x ->
            Hm_effective_invariant.run_invariant h1 heads1 forest1 depth pool1 facts1 env1 body middle middle_heads middle_valid body_pool x (); () in
          let equal12 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho2 x === rho x}) @ total = fun x ->
            equal1 x; equal2 x; () in
          let nodes2 = node_model middle rho2 model2 in
          let consume_arrow : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho3 x})) @ total ->
            (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || rho3 x === rho2 x})) @ total ->
            {u : unit | rho3 p === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 assigned3 ->
              let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
                Hm_effective_membership.run_extends h1 depth pool1 env1 body middle body_pool x ();
                equal12 x; equal3 x; () in
              let converted = copy_model after rho3 model3 in Hm_effective_complete_helpers.matches_def rho3 e value;
              let () = use rho3 converted equal () in () in
          let () = Hm_effective_complete_helpers.with_alloc middle middle_heads depth body_pool middle_facts rho2 nodes2 p desc value after () claim consume_arrow in () in
        let () = with_run_model h1 heads1 depth pool1 (facts1) (forest1) env1 ts1 g1 rho1 model1 xi realize1 n body middle body_pool b db () claim consume_body in () in
      let () = Hm_effective_complete_helpers.with_alloc h heads depth pool facts rho nodes arg var av h1 () claim consume_arg in ()
      | _ -> ())
      | _ -> ())
    | RApp_left (left, _) -> (match d with D.Application (at, dl, _) ->
      let ft = D.Function (at, target) in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches tau left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun tau _next _equal fit ->
          let fv = T.eval xi ft in Hm_effective_complete_helpers.matches_def tau left fv; () in
      let () = with_run_model h heads depth pool facts forest env ts g rho model xi realize n left after final_pool ft dl () claim consume in ()
      | _ -> ())
    | RApp_right (left, right, h1, pool1) -> (match d with D.Application (at, dl, dr) ->
      let ft = D.Function (at, target) in let fv = T.eval xi ft in let av = T.eval xi at in T.eval_def xi ft;
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h1 x then Level_finite_spec.finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.run_forest h forest depth pool env left h1 pool1 x () in t in
      let[@def] heads1 : E.heads = fun x -> let r = Forest_heads.select h1 forest1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let _r = Forest_heads.select h1 forest1 x in E.valid_head_def h1 heads1 x;
        () in
            C.run_environment h heads heads1 valid depth pool env ts left h1 pool1 valid1 depth ();
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
        Hm_effective_invariant.run_invariant h heads forest depth pool facts env left h1 heads1 valid1 pool1 x (); () in
      let consume_left : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches rho1 left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        Hm_effective_complete_helpers.matches_def rho1 left fv;
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let () = preserved_boundary h heads depth env ts rho rho1 equal1 x () in () in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args () claim use in () in
        let consume_right : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_effective_complete_helpers.matches rho2 right (T.eval xi at)} -> {u : unit | claim}) @ total = fun rho2 _model2 _equal2 fit2 ->
          Hm_effective_complete_helpers.matches_def rho2 right av;
          () in
        let () = with_run_model h1 heads1 depth pool1 facts1 forest1 env ts g rho1 model1 xi realize1 n right after final_pool at dr () claim consume_right in () in
      let () = with_run_model h heads depth pool facts forest env ts g rho model xi realize n left h1 pool1 ft dl () claim consume_left in ()
      | _ -> ())
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, derivation) -> (match d with D.Application (at, dl, dr) ->
      let ft = D.Function (at, target) in let fv = T.eval xi ft in let av = T.eval xi at in T.eval_def xi ft;
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h1 x then Level_finite_spec.finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.run_forest h forest depth pool env left h1 pool1 x () in t in
      let[@def] heads1 : E.heads = fun x -> let r = Forest_heads.select h1 forest1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let _r = Forest_heads.select h1 forest1 x in E.valid_head_def h1 heads1 x;
        () in
            C.run_environment h heads heads1 valid depth pool env ts left h1 pool1 valid1 depth ();
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
        Hm_effective_invariant.run_invariant h heads forest depth pool facts env left h1 heads1 valid1 pool1 x (); () in
      let consume_left : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches rho1 left (T.eval xi ft)} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        Hm_effective_complete_helpers.matches_def rho1 left fv;
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho1 x}) @ total = fun x ->
          let () = preserved_boundary h heads depth env ts rho rho1 equal1 x () in () in
        let realize1 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho1 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_transport g ts rho rho1 xi realize boundaries i sigma schema args () claim use in () in
        let consume_right : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (model2 : ((x : node Pref.t) @ immutable -> {u : unit | equation h2 rho2 x})) @ total ->
          (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
          {u : unit | Hm_effective_complete_helpers.matches rho2 right (T.eval xi at)} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
          Hm_effective_complete_helpers.matches_def rho2 right av;
          match result left with None -> () | Some f -> match result right with None -> () | Some a ->
          Hm_effective_driver_proofs.run_result h forest depth pool env left h1 pool1 f ();
          Hm_effective_membership.run_extends h1 depth pool1 env right h2 pool2 f ();
          Hm_effective_driver_proofs.run_result h1 forest1 depth pool1 env right h2 pool2 a (); equal2 f;

      let forest2 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let t = Hm_effective_forest.run_forest h1 forest1 depth pool1 env right h2 pool2 x () in t in
      let[@def] heads2 : E.heads = fun x -> let r = Forest_heads.select h2 forest2 x in r in
      let valid2 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2 heads2 x}) @ total = fun x ->
        heads2_def x; let _r = Forest_heads.select h2 forest2 x in E.valid_head_def h2 heads2 x;
        () in
                let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 heads2 depth pool2 x}) @ total = fun x ->
            Hm_effective_invariant.run_invariant h1 heads1 forest1 depth pool1 facts1 env right h2 heads2 valid2 pool2 x (); () in
          let nodes2 = node_model h2 rho2 model2 in
          let consume_application : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho3 x})) @ total ->
            (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
            {u : unit | ok && rho3 p === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 assigned ->
              let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
                Hm_effective_membership.run_extends h depth pool env left h1 pool1 x ();
                Hm_effective_membership.run_extends h1 depth pool1 env right h2 pool2 x ();
                equal1 x; equal2 x; equal3 x; () in
              let converted = copy_model after rho3 model3 in Hm_effective_complete_helpers.matches_def rho3 e value;
              let () = use rho3 converted equal () in () in
          let () = Hm_effective_complete_helpers.with_application_model h2 heads2 forest2 depth pool2 facts2 rho2 nodes2 f a p arrow ok after derivation value () claim consume_application in () in
        let () = with_run_model h1 heads1 depth pool1 facts1 forest1 env ts g rho1 model1 xi realize1 n right h2 pool2 at dr () claim consume_right in () in
      let () = with_run_model h heads depth pool facts forest env ts g rho model xi realize n left h1 pool1 ft dl () claim consume_left in ()
      | _ -> ())
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
      allocated_def h depth arg var;
      let forest1 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let trees = Hm_effective_forest.allocated_forest h forest depth arg var () in let t = trees x in t in
      let[@def] heads1 : E.heads = fun x -> let r = Forest_heads.select h1 forest1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let _r = Forest_heads.select h1 forest1 x in E.valid_head_def h1 heads1 x;
        () in
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
        facts x; A.allocate_runtime h heads heads1 depth pool arg var valid (refine_ valid1) x (); () in
            allocated_def h1 depth res var;
      let forest2 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let trees = Hm_effective_forest.allocated_forest h1 forest1 depth res var () in let t = trees x in t in
      let[@def] heads2 : E.heads = fun x -> let r = Forest_heads.select h2 forest2 x in r in
      let valid2 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2 heads2 x}) @ total = fun x ->
        heads2_def x; let _r = Forest_heads.select h2 forest2 x in E.valid_head_def h2 heads2 x;
        () in
      let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 heads2 depth pool2 x}) @ total = fun x ->
        facts1 x; A.allocate_runtime h1 heads1 heads2 depth pool1 res var valid1 (refine_ valid2) x (); () in
            allocated_def h2 depth self desc;
      let forest3 : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let trees = Hm_effective_forest.allocated_forest h2 forest2 depth self desc () in let t = trees x in t in
      let[@def] heads3 : E.heads = fun x -> let r = Forest_heads.select h3 forest3 x in r in
      let valid3 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h3 heads3 x}) @ total = fun x ->
        heads3_def x; let _r = Forest_heads.select h3 forest3 x in E.valid_head_def h3 heads3 x;
        () in
      valid1 arg; valid2 arg; valid2 res;
      A.allocated_below h depth arg var heads1 ();
      A.saved_below h1 heads1 heads2 res v arg depth ();
      A.allocated_below h1 depth res var heads2 ();
      let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 heads3 depth pool3 x}) @ total = fun x ->
        facts2 x; A.allocate_runtime h2 heads2 heads3 depth pool2 self desc valid2 (refine_ valid3) x (); () in
            C.allocation_environment h heads heads1 valid arg v (refine_ valid1) depth env ts ();
      C.allocation_environment h1 heads1 heads2 valid1 res v (refine_ valid2) depth env ts ();
      C.allocation_environment h2 heads2 heads3 valid2 self w (refine_ valid3) depth env ts ();
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      cell_def var depth; cell_def desc depth;
      below_def h3 arg depth; at_level_def h3 arg; below_def h3 self depth; at_level_def h3 self;
      valid3 arg; valid3 self;
      A.saved_below h2 heads2 heads3 self w arg depth ();
      A.allocated_below h2 depth self desc heads3 ();
      bind_environment h3 heads3 depth env ts g self target ();
      bind_environment h3 heads3 depth self_env self_ts self_g arg a ();
      let nodes = node_model h rho model in Copy_model_proofs.describes_def rho var av;
      let consume1 : ((rho1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model1 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h1 rho1 x})) @ total ->
        (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho1 x === rho x})) @ total ->
        {u : unit | rho1 arg === av} -> {u : unit | claim}) @ total = fun rho1 model1 equal1 fit1 ->
        Copy_model_proofs.describes_def rho1 var bv;
      let consume2 : ((rho2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model2 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h2 rho2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || rho2 x === rho1 x})) @ total ->
        {u : unit | rho2 res === bv} -> {u : unit | claim}) @ total = fun rho2 model2 equal2 fit2 ->
        equal2 arg; Copy_model_proofs.describes_def rho2 desc value;
      let consume3 : ((rho3 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model3 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h3 rho3 x})) @ total ->
        (equal3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || rho3 x === rho2 x})) @ total ->
        {u : unit | rho3 self === value} -> {u : unit | claim}) @ total = fun rho3 model3 equal3 fit3 ->
        equal3 arg; equal3 res;
        let old_equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho3 x === rho x}) @ total = fun x ->
          equal1 x; equal2 x; equal3 x; () in
        let boundaries : ((x : node Pref.t) @ immutable -> {u : unit | not (environment_boundary ts x) || rho x === rho3 x}) @ total = fun x ->
          let () = preserved_boundary h heads depth env ts rho rho3 old_equal x () in () in
        let realized : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_transport g ts rho rho3 xi realize boundaries i sigma schema args () claim use in () in
        let self_realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup self_g i === Some sigma && template_lookup self_ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_monomorphic g ts rho3 xi realized self target ()
            i sigma schema args () claim use in () in
        let realize3 : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g3 i === Some sigma && template_lookup ts3 i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho3 choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
          let () = Hm_environment_models.realize_monomorphic self_g self_ts rho3 xi self_realize arg a ()
            i sigma schema args () claim use in () in
      let consume_body : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (body_model : ((x : node Pref.t) @ immutable -> {u : unit | equation middle tau x})) @ total ->
        (equal4 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || tau x === rho3 x})) @ total ->
        {u : unit | Hm_effective_complete_helpers.matches tau body (T.eval xi b)} -> {u : unit | claim}) @ total = fun tau body_model equal4 fit ->
        Hm_effective_complete_helpers.matches_def tau body bv;
        match result body with None -> () | Some root -> match finish with Aborted -> () | Unified (ok, derivation) ->
        equal4 res; equal4 self; let body_nodes = node_model middle tau body_model in
        Hm_effective_complete_helpers.unify_complete middle tau body_nodes root res ok after derivation root ();
        let next_nodes : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x}) @ total = fun x ->
          let () = Hm_effective_complete_helpers.unify_complete middle tau body_nodes root res ok after derivation x () in () in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total = fun x ->
          old_equal x; equal4 x; () in
        let next = copy_model after tau next_nodes in Hm_effective_complete_helpers.matches_def tau e value;
        let () = use tau next equal () in () in
        let model3 = copy_model h3 rho3 model3 in
        let () = with_run_model h3 heads3 depth pool3 (facts3) (forest3) env3 ts3 g3 rho3 model3 xi realize3 n body middle body_pool b db () claim consume_body in () in
        let () = Hm_effective_complete_helpers.with_alloc h2 heads2 depth pool2 (facts2) rho2 model2 self desc value h3 () claim consume3 in () in
        let () = Hm_effective_complete_helpers.with_alloc h1 heads1 depth pool1 (facts1) rho1 model1 res var bv h2 () claim consume2 in () in
      let () = Hm_effective_complete_helpers.with_alloc h heads depth pool facts rho nodes arg var av h1 () claim consume1 in ()
      | _ -> ()))

let (with_closed_model @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} -> (claim : bool) ->
    (use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_effective_complete_helpers.matches rho e target} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool target d premise claim use -> ghost_ (
    let h = H.empty () in let z = D.Z in
    let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
    let ts = No_templates in let g = D.Empty_context in
    let forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable) @ total = fun x ->
      let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t; Level_unifier_spec.observe_def h x; t in
    let[@def] heads : E.heads = fun x -> let r = Forest_heads.select h forest x in r in
          let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads 0 empty x}) @ total = fun x ->
      runtime_at_def h heads 0 empty x; safe_def h heads x; depth_bound_def h heads 0 x; Representative_level.representative_covered_def h (-1) empty x;
      terminal_def h x; observe_def h x; E.effective_ordered_def h heads x; E.valid_head_def h heads x; () in
let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      equation_def h rho x; () in
    let[@def] xi : D.index @ immutable total -> ty @ immutable total = fun _i -> Boolean in
    let realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
      (schema : template) @ immutable -> (args : T.values) @ immutable ->
      {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
        && T.values_length args === D.arity sigma} -> (claim : bool) ->
      (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
        {u : unit | claim})) @ total -> {u : unit | claim}) @ total = fun i sigma schema args fit claim use ->
        let () = Hm_environment_models.realize_empty rho xi i sigma schema args () claim use in () in
    effective_env_def h heads 0 env ts; aligned_def g ts;
    T.eval_embed xi target; let mono = D.embed target in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | Hm_effective_complete_helpers.matches tau e (T.eval xi mono)} -> {u : unit | claim}) @ total = fun tau next _equal fit ->
        let () = use tau next () in () in
    let () = with_run_model h heads 0 empty facts forest env ts g rho model xi realize z e after pool mono d () claim consume in ())

let (closed_completes @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} ->
    {u : unit | not (result e === None)} @ ghost = fun e after pool target d premise -> ghost_ (
    let claim = not (result e === None) in
    let use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_effective_complete_helpers.matches rho e target} -> {u : unit | claim}) @ total = fun rho _model fit ->
        Hm_effective_complete_helpers.matches_def rho e target; () in
    let () = with_closed_model e after pool target d () claim use in ())

let (closed_reject @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === None && D.typed D.Z D.Empty_context (source e) (D.embed target) d} ->
    {u : unit | false} @ ghost = fun e after pool target d premise -> ghost_ (
    closed_completes e after pool target d (); ())

let (closed_factor @ total) : (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree : Level_finite_spec.tree) @ immutable ->
    (target : ty) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === Some p && Level_finite_spec.finite after tree && Level_finite_spec.tree_root tree === p
      && D.typed D.Z D.Empty_context (source e) (D.embed target) d} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool p tree target d premise claim use -> ghost_ (
    let consume : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
      {u : unit | Hm_effective_complete_helpers.matches rho e target} -> {u : unit | claim}) @ total = fun rho model fit ->
        Hm_effective_complete_helpers.matches_def rho e target; let nodes = node_model after rho model in
        Level_mgu_proofs.readback_factor after rho nodes tree ();
        let () = use rho () in () in
    let () = with_closed_model e after pool target d () claim consume in ())
