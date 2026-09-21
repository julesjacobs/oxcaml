open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
open Hm_effective_runtime
open Hm_effective_environment
open Level_finite_spec
module D = Hm_declarative
module T = Hm_type_proofs
module P = Hm_template_instance_proofs
module C = Hm_effective_environment_proofs
module E = Effective_level
module A = Hm_effective_allocation

let (boundary_scheme @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | P.scheme rho (Boundary p) === D.Forall (D.Z, T.embed (rho p))} @ ghost = fun rho p -> ghost_ (
    let schema = Boundary p in P.scheme_def rho schema; Hm_freshness_proofs.template_names_def schema;
    let names = Hm_abstraction.No_names in Hm_abstraction.count_def names; P.body_def names rho schema;
    let u = () in refine_ u)

let rec (run_sound @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads depth pool x})) @ total ->
    (env : env) @ immutable -> (schemas : templates) @ immutable ->
    (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && effective_env h heads depth env schemas && result e === Some p} ->
    {d : D.typing | D.typed D.Z (P.context rho schemas) (source e) (T.embed (rho p)) d} @ immutable ghost =
  fun h heads trees depth pool facts env schemas e after final_pool rho model p premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool;
     result_def e; source_def e; P.context_wf rho schemas;
    let valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x}) @ total = fun x ->
      facts x; runtime_at_def h heads depth pool x; safe_def h heads x; let u = () in refine_ u in
    let g = P.context rho schemas in let term = source e in let ty = rho p in let t = T.embed ty in
    let z = D.Z in T.embed_wf z ty; let u = () in match e with
    | RApp_left _ | RApp_right _ | RLet_left _ ->
      let d = D.Constant in refine_ d
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads child_depth empty x}) @ total = fun x ->
        facts x; let u = () in enter_runtime h heads depth pool x (refine_ u); refine_ u in
      C.environment_raise h heads depth child_depth env schemas (refine_ u);
      (match result rhs with None -> let d = D.Constant in refine_ d | Some original ->
      let middle_forest : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.run_forest h trees child_depth empty env rhs middle child_pool x (refine_ u) in refine_ t in
      let[@def] middle_heads : E.heads = fun x -> let refine_ r = Forest_heads.select middle middle_forest x in r in
      let middle_valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads x}) @ total = fun x ->
        middle_heads_def x; let refine_ r = Forest_heads.select middle middle_forest x in
        E.valid_head_def middle middle_heads x; let u = () in refine_ u in
                  let middle_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle middle_heads child_depth child_pool x}) @ total = fun x ->
        let u = () in Hm_effective_invariant.run_invariant h heads trees child_depth empty child_facts env rhs middle middle_heads middle_valid child_pool x (refine_ u); refine_ u in
let closed = Representative_pool_spec.close_heap middle depth child_pool in let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      let closed_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at closed middle_heads depth transferred x}) @ total = fun x ->
        let u = () in Hm_effective_closing.close_after_run h heads middle_heads depth pool facts env rhs middle child_pool (refine_ middle_facts) x (refine_ u); refine_ u in
      Representative_pool_spec.close_heap_def middle depth child_pool;
      let filtered = Representative_level.representatives middle child_pool in
      Representative_level.representatives_scoped middle child_pool (refine_ u);
      let closed_forest : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Forest_transport.closed_forest_at middle middle_forest depth filtered x (refine_ u) in refine_ t in
      Hm_effective_driver_proofs.run_result h trees child_depth empty env rhs middle child_pool original (refine_ u);
      middle_facts original; Hm_effective_result.result_below h trees child_depth empty env rhs middle child_pool middle_heads original (refine_ u);
      let refine_ finite_tree = middle_forest original in
      Forest_transport.unfolding_valid middle finite_tree (refine_ u); Forest_transport.unfolding_root finite_tree;
      let schema = Effective_template.scheme middle middle_heads depth finite_tree in
      let coverage : ((x : node Pref.t) @ immutable -> {u : unit | Representative_level.representative_covered middle depth child_pool x}) @ total = fun x ->
        middle_facts x; runtime_at_def middle middle_heads child_depth child_pool x; let u = () in refine_ u in
      let order : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads x && E.effective_ordered middle middle_heads x}) @ total = fun x ->
        middle_facts x; runtime_at_def middle middle_heads child_depth child_pool x; safe_def middle middle_heads x; let u = () in refine_ u in
      Effective_template.scheme_valid middle middle_heads middle_valid depth child_pool coverage finite_tree (refine_ u);
      Effective_template.scheme_root middle middle_heads depth finite_tree;
      let levels : ((x : node Pref.t) @ immutable ->
        {u : unit | match E.level middle middle_heads x with Generic -> true | Finite n -> n >= 0}) @ total = fun x ->
        middle_facts x; runtime_at_def middle middle_heads child_depth child_pool x;
        depth_bound_def middle middle_heads child_depth x; E.effective_below_def middle middle_heads x child_depth;
        E.level_def middle middle_heads x; let u = () in refine_ u in
      Effective_template.scheme_boundary middle middle_heads depth child_pool middle_valid levels finite_tree (refine_ u);
      C.run_environment h heads middle_heads valid child_depth empty env schemas rhs middle child_pool middle_valid depth (refine_ u);
      let close_frame : ((x : node Pref.t) @ immutable ->
        {u : unit | Effective_template.protected middle middle_heads closed middle_heads depth x}) @ total = fun x ->
        middle_valid x; let u = () in
        Effective_template.close_protected middle middle_heads depth child_pool depth x (refine_ u); refine_ u in
      transport middle middle_heads closed middle_heads depth close_frame env schemas (refine_ u);
      let next_env = Bind (original, env) in let next_schemas = Template_binding (schema, schemas) in
      effective_env_def closed middle_heads depth next_env next_schemas;
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in Hm_effective_model.run_restrict closed closed_forest depth transferred next_env body after final_pool rho model x (refine_ u);
        Generalize_proofs.closed_model middle depth filtered rho x (refine_ u);
        equation_def middle rho x; equation_def closed rho x; observe_def middle x; observe_def closed x;
        node_equation_def middle rho x; node_equation_def closed rho x; refine_ u in
      let general_forest : ((x : node Pref.t) @ immutable ->
        {t : tree | tree_root t === x && (not (H.mem middle x) || finite middle t)} @ immutable) @ total = refine_ middle_forest in
      let[@def] canonical : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
        let refine_ tree = general_forest x in readback tree in
      let values : ((x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = general_forest x in canonical x === readback t}) @ total = fun x ->
        canonical_def x; let u = () in refine_ u in
      let agrees : ((x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = middle_forest x in not (H.mem middle x) || canonical x === readback t}) @ total = fun x ->
        let refine_ tree = middle_forest x in let u = () in if H.mem middle x then (
          Hm_effective_generalization.canonical_value middle general_forest canonical values tree (refine_ u); refine_ u)
        else refine_ u in
      let canonical_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle canonical x}) @ total = fun x ->
        let refine_ u = Level_finite_proofs.readback_model_at middle middle_forest canonical agrees x in refine_ u in
      let refine_ rhs_d = run_sound h heads trees child_depth empty child_facts env schemas rhs middle child_pool canonical canonical_model original (refine_ u) in
      Hm_effective_generalization.canonical_value middle general_forest canonical values finite_tree (refine_ u);
      let rhs_term = source rhs in
      let refine_ generalized_d = Hm_effective_generalization.generalize_typing middle middle_heads depth child_depth order
        general_forest canonical values env schemas finite_tree rhs_term rhs_d (refine_ u) in
      let factor : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem middle x) || Level_mgu_spec.substitute rho (canonical x) === rho x}) @ total = fun x ->
        let refine_ tree = middle_forest x in
        let u = () in if H.mem middle x then (Hm_effective_generalization.canonical_value middle general_forest canonical values tree (refine_ u); Level_mgu_proofs.readback_factor middle rho mid_model tree (refine_ u); refine_ u)
        else refine_ u in
      let environment_factor : ((x : node Pref.t) @ immutable ->
        {u : unit | not (environment_boundary schemas x) || Level_mgu_spec.substitute rho (canonical x) === rho x}) @ total = fun x ->
        factor x; let u = () in if environment_boundary schemas x then (
          C.environment_boundary_owned middle middle_heads depth env schemas x (refine_ u); refine_ u) else refine_ u in
      let schema_factor : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member schema x) || Level_mgu_spec.substitute rho (canonical x) === rho x}) @ total = fun x ->
        factor x; let u = () in if boundary_member schema x then (
          C.boundary_below closed middle_heads depth schema x (refine_ u); E.effective_below_def closed middle_heads x depth;
          Generalize_proofs.closed_observe middle depth filtered x (refine_ u);
          closed_at_def middle closed depth filtered x; refine_ u) else refine_ u in
      let names = Hm_freshness_proofs.template_names schema in let k = Hm_abstraction.count names in
      let included : ((x : node Pref.t) @ immutable ->
        {u : unit | Hm_abstraction.position (Hm_freshness_proofs.template_names schema) x === None
          || not (Hm_abstraction.position names x === None)}) @ total = fun _x -> let u = () in refine_ u in
      P.parameters_subset schema names included;
      Hm_scheme_transport_proofs.substitute_body names rho canonical rho schema schema_factor (refine_ u);
      Hm_scheme_transport_proofs.substitute_context rho canonical rho schemas environment_factor;
      let canonical_g = P.context canonical schemas in let shifted_g = D.weaken_context k canonical_g in
      let canonical_body = P.body names canonical schema in
      Hm_substitution_proofs.substitution_typed rho k shifted_g rhs_term canonical_body generalized_d (refine_ u);
      Hm_substitution_proofs.substitute_weaken_context rho k canonical_g;
      let changed_rhs = Hm_substitution.substitute_typing rho generalized_d in
      let refine_ body_d = run_sound closed middle_heads closed_forest depth transferred closed_facts next_env next_schemas body after final_pool rho model p (refine_ u) in
      let sigma = P.scheme rho schema in P.scheme_def rho schema; P.context_def rho next_schemas;
      P.scheme_wf rho schema; Hm_abstraction_proofs.add_zero k;
      let d = D.Let_binding (sigma, changed_rhs, body_d) in D.typed_def z g term t d; refine_ d)
    | RShared (i, _, rep) ->
      let refine_ schema = lookup_schema h heads depth env schemas i p (refine_ u) in
      valid p; resolves_def h p rep.root rep.path; E.valid_head_def h heads p; let actual = heads p in
      Representative_level.unique h p rep.root rep.path actual.root actual.path (refine_ u);
      E.effective_below_def h heads p depth; E.level_def h heads p;
      active_def h rep.root; at_level_def h rep.root;
      Effective_template.valid_template_def h heads schema; root_def schema;
      (match schema with Boundary _ -> () | Parameter q | Constant q | Product (q, _, _) | Indirect (q, _) ->
        Effective_template.generic_def h heads q; ());
      P.lookup_context rho schemas i schema (refine_ u); boundary_scheme rho p;
      let args = D.No_arguments in let d = D.Variable args in let sigma = D.Forall (z, t) in
      D.typed_def z g term t d; D.length_def args; D.arity_def sigma;
      D.arguments_wf_def z args; D.open_scheme_def sigma args; T.open_empty t; refine_ d
    | RVar (i, _, epoch, history, certificate) -> (match lookup env i with None -> let d = D.Constant in refine_ d
      | Some original -> copy_heap_def h epoch depth history; Hm_execution_spec.copy_heap_def h epoch depth history;
        let refine_ d = Hm_effective_variable.variable_typing h heads valid certificate depth env schemas i original p epoch history rho
          (refine_ model) (refine_ u) in refine_ d)
    | RBool _ -> model p; node_equation_def after rho p; observe_def after p;
      let desc : desc = Bool in cell_def desc depth; T.embed_def ty;
      let d = D.Constant in D.typed_def z g term t d; refine_ d
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      allocated_def h depth arg var; cell_def var depth; allocatable_def h v;
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
      let[@def] heads1 : E.heads = fun x -> let refine_ r = Forest_heads.select h1 ts1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let refine_ r = Forest_heads.select h1 ts1 x in
        E.valid_head_def h1 heads1 x; let u = () in refine_ u in
            let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth (Entry (arg, pool)) x}) @ total = fun x ->
        facts x; let u = () in A.allocate_runtime h heads heads1 depth pool arg var valid (refine_ valid1) x (refine_ u); refine_ u in
      C.allocation_environment h heads heads1 valid arg v (refine_ valid1) depth env schemas (refine_ u);
      valid1 arg; A.allocated_below h depth arg var heads1 (refine_ u);
      bind_schema h1 heads1 depth env schemas arg (refine_ u);
      let schema = Boundary arg in let schemas1 = Template_binding (schema, schemas) in
      P.context_def rho schemas1; boundary_scheme rho arg;
      let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.run_forest h1 ts1 depth pool1 env1 body middle body_pool x (refine_ u) in refine_ t in
            (match result body with None -> let d = D.Constant in refine_ d | Some b -> match out with
      | None -> let d = D.Constant in refine_ d | Some _ ->
      let desc = Arrow (arg, b) in let w = cell desc depth in allocated_def middle depth p desc;
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in let refine_ _t = ts2 x in model x; Hm_effective_model.allocation_restrict middle p w rho x (refine_ u); refine_ u in
      let refine_ body_typing = run_sound h1 heads1 ts1 depth pool1 (refine_ facts1) env1 schemas1 body middle body_pool rho mid_model b (refine_ u) in
      model p; node_equation_def after rho p; observe_def after p; cell_def desc depth;
      T.embed_def ty; let a = T.embed (rho arg) in let d = D.Abstraction (a, body_typing) in
      D.typed_def z g term t d; refine_ d)
    | RApp (left, right, h1, pool1, h2, pool2, _, arrow, ok, derivation) ->
      let ts1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.run_forest h trees depth pool env left h1 pool1 x (refine_ u) in refine_ t in
      let[@def] heads1 : E.heads = fun x -> let refine_ r = Forest_heads.select h1 ts1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let refine_ r = Forest_heads.select h1 ts1 x in
        E.valid_head_def h1 heads1 x; let u = () in refine_ u in
            let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_effective_forest.run_forest h1 ts1 depth pool1 env right h2 pool2 x (refine_ u) in refine_ t in
            (match result left with None -> let d = D.Constant in refine_ d | Some f ->
      match result right with None -> let d = D.Constant in refine_ d | Some a ->
      let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
      let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
      allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
      cell_def var depth; allocatable_def h2 v;
      let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h2 ts2 p v x (refine_ u) in refine_ t in
            let model4 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h4 rho x}) @ total = fun x ->
        let u = () in Effective_unifier_model.success_forward_at h4 rho f arrow after derivation model x (refine_ u); refine_ u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in let refine_ _t = ts3 x in model4 x; Hm_effective_model.allocation_restrict h3 arrow w rho x (refine_ u); refine_ u in
      let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
        let u = () in let refine_ _t = ts2 x in model3 x; Hm_effective_model.allocation_restrict h2 p v rho x (refine_ u); refine_ u in
      let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
        let u = () in Hm_effective_model.run_restrict h1 ts1 depth pool1 env right h2 pool2 rho model2 x (refine_ u); refine_ u in
      let refine_ left_typing = run_sound h heads trees depth pool facts env schemas left h1 pool1 rho model1 f (refine_ u) in
      C.run_environment h heads heads1 valid depth pool env schemas left h1 pool1 valid1 depth (refine_ u);
      let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth pool1 x}) @ total = fun x ->
        let u = () in Hm_effective_invariant.run_invariant h heads trees depth pool facts env left h1 heads1 valid1 pool1 x (refine_ u); refine_ u in
      let refine_ right_typing = run_sound h1 heads1 ts1 depth pool1 facts1 env schemas right h2 pool2 rho model2 a (refine_ u) in
      Effective_unifier_model.success_forward_at h4 rho f arrow after derivation model arrow (refine_ u);
      node_equation_def h4 rho arrow; observe_def h4 arrow; cell_def desc depth;
      let ft = rho f in T.embed_def ft;
      let at = T.embed (rho a) in let d = D.Application (at, left_typing, right_typing) in
      D.typed_def z g term t d; refine_ d)
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
      let[@def] heads1 : E.heads = fun x -> let refine_ r = Forest_heads.select h1 ts1 x in r in
      let valid1 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 x}) @ total = fun x ->
        heads1_def x; let refine_ r = Forest_heads.select h1 ts1 x in
        E.valid_head_def h1 heads1 x; let u = () in refine_ u in
            let ts2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h1 ts1 res v x (refine_ u) in refine_ t in
      let[@def] heads2 : E.heads = fun x -> let refine_ r = Forest_heads.select h2 ts2 x in r in
      let valid2 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2 heads2 x}) @ total = fun x ->
        heads2_def x; let refine_ r = Forest_heads.select h2 ts2 x in
        E.valid_head_def h2 heads2 x; let u = () in refine_ u in
            let ts3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Level_finite_proofs.allocation_finite_at h2 ts2 self w x (refine_ u) in refine_ t in
      let[@def] heads3 : E.heads = fun x -> let refine_ r = Forest_heads.select h3 ts3 x in r in
      let valid3 : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h3 heads3 x}) @ total = fun x ->
        heads3_def x; let refine_ r = Forest_heads.select h3 ts3 x in
        E.valid_head_def h3 heads3 x; let u = () in refine_ u in
            let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in
      let self_env = Bind (self, env) in let env3 = Bind (arg, self_env) in
      let pool1 = Entry (arg, pool) in let pool2 = Entry (res, pool1) in
let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 heads1 depth (Entry (arg, pool)) x}) @ total = fun x ->
        facts x; let u = () in A.allocate_runtime h heads heads1 depth pool arg var valid (refine_ valid1) x (refine_ u); refine_ u in
      let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 heads2 depth (Entry (res, pool1)) x}) @ total = fun x ->
        facts1 x; let u = () in A.allocate_runtime h1 heads1 heads2 depth pool1 res var valid1 (refine_ valid2) x (refine_ u); refine_ u in
      valid1 arg; valid2 arg; valid2 res;
      A.allocated_below h depth arg var heads1 (refine_ u);
      A.saved_below h1 heads1 heads2 res v arg depth (refine_ u);
      A.allocated_below h1 depth res var heads2 (refine_ u);
      let facts3 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h3 heads3 depth (Entry (self, pool2)) x}) @ total = fun x ->
        facts2 x; let u = () in A.allocate_runtime h2 heads2 heads3 depth pool2 self desc valid2 (refine_ valid3) x (refine_ u); refine_ u in
      C.allocation_environment h heads heads1 valid arg v (refine_ valid1) depth env schemas (refine_ u);
      C.allocation_environment h1 heads1 heads2 valid1 res v (refine_ valid2) depth env schemas (refine_ u);
      C.allocation_environment h2 heads2 heads3 valid2 self w (refine_ valid3) depth env schemas (refine_ u);
      valid3 arg; valid3 self;
      A.saved_below h2 heads2 heads3 self w arg depth (refine_ u);
      A.allocated_below h2 depth self desc heads3 (refine_ u);
      bind_schema h3 heads3 depth env schemas self (refine_ u);
      let self_schema = Boundary self in let self_schemas = Template_binding (self_schema, schemas) in
      bind_schema h3 heads3 depth self_env self_schemas arg (refine_ u);
      let arg_schema = Boundary arg in let schemas3 = Template_binding (arg_schema, self_schemas) in
      P.context_def rho schemas3; P.context_def rho self_schemas; boundary_scheme rho self; boundary_scheme rho arg;
      (match result body with None -> let d = D.Constant in refine_ d | Some b -> match finish with
      | Aborted -> let d = D.Constant in refine_ d | Unified (_, derivation) ->
      let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
        let u = () in Effective_unifier_model.success_forward_at middle rho b res after derivation model x (refine_ u); refine_ u in
      let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
        let u = () in Hm_effective_model.run_restrict h3 ts3 depth pool3 env3 body middle body_pool rho mid_model x (refine_ u); refine_ u in
      let refine_ body_typing = run_sound h3 heads3 ts3 depth pool3 (refine_ facts3) env3 schemas3 body middle body_pool rho mid_model b (refine_ u) in
      Effective_unifier_model.success_forward_at middle rho b res after derivation model p (refine_ u);
      model3 self; node_equation_def h3 rho self; observe_def h3 self; T.embed_def ty;
      let at = T.embed (rho arg) in let bt = T.embed (rho res) in
      let d = D.Recursion (at, bt, body_typing) in D.typed_def z g term t d; refine_ d))

let (closed_sound @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (t : tree) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === Some p && finite after t && tree_root t === p} ->
    {d : D.typing | D.typed D.Z D.Empty_context (source e) (T.embed (readback t)) d} @ immutable ghost =
  fun e after pool p t premise -> ghost_ (
    let refine_ premise = premise in let h = H.empty () in
    let trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total = fun x ->
      let t = Free x in tree_root_def t; observe_def h x; refine_ t in
    let final_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = Hm_effective_forest.closed_forest e after pool x (refine_ u) in refine_ t in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let refine_ t = final_trees x in readback t in
    let agrees : ((x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = final_trees x in not (H.mem after x) || rho x === readback t}) @ total = fun x ->
      rho_def x; let u = () in refine_ u in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x}) @ total = fun x ->
      let refine_ u = Level_finite_proofs.readback_model_at after final_trees rho agrees x in refine_ u in
    let[@def] heads : E.heads = fun x -> let refine_ r = Forest_heads.select h trees x in r in
      let env : env = Hm_environment_spec.Empty in let empty : pool = Generalize_spec.Empty in
    let schemas = No_templates in effective_env_def h heads 0 env schemas; P.context_def rho schemas;
    let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h heads 0 empty x}) @ total = fun x ->
      runtime_at_def h heads 0 empty x; safe_def h heads x; depth_bound_def h heads 0 x; Representative_level.representative_covered_def h (-1) empty x; terminal_def h x; observe_def h x;
      E.effective_ordered_def h heads x; E.valid_head_def h heads x; let u = () in refine_ u in
    let u = () in
    let refine_ d = run_sound h heads trees 0 empty facts env schemas e after pool rho model p (refine_ u) in
    let refine_ actual = final_trees p in rho_def p; finite_def after t;
    Level_finite_proofs.finite_unique after t actual (refine_ u); refine_ d)


let (closed_principal @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree : tree) @ immutable ->
    (target : ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && result e === Some p && finite after tree && tree_root tree === p
      && D.typed D.Z D.Empty_context (source e) (T.embed target) typing} -> (claim : bool) ->
    (use : ((inferred : D.typing) @ immutable ->
      (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | D.typed D.Z D.Empty_context (source e) (T.embed (readback tree)) inferred
        && target === Level_mgu_spec.substitute delta (readback tree)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun e after pool p tree target typing premise claim use -> ghost_ (
    let refine_ premise = premise in let u = () in
    let refine_ inferred = closed_sound e after pool p tree (refine_ u) in
    let consume : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | target === Level_mgu_spec.substitute delta (readback tree)} -> {u : unit | claim}) @ total =
      fun delta factor -> let refine_ factor = factor in let u = () in let refine_ u = use inferred delta (refine_ u) in refine_ u in
    let refine_ u = Hm_effective_complete.closed_factor e after pool p tree target typing (refine_ u) claim consume in refine_ u)
