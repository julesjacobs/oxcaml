open Copy_spec
open Level_spec
open Generalize_spec
open Hm_execution_spec
open Hm_runtime_spec
module D = Hm_declarative
module T = Hm_type_proofs
module C = Hm_complete_proofs

let (identity_typing @ total) : (a : ty) @ immutable ->
    {d : D.typing | D.typed D.Z D.Empty_context (D.Lambda (D.Bound D.Z))
      (T.embed (Function (a, a))) d} @ immutable ghost = fun a -> ghost_ (
    let z = D.Z in let empty = D.Empty_context in let at = T.embed a in
    let scheme = D.Forall (z, at) in let env = D.Binding (scheme, empty) in
    let args = D.No_arguments in let variable = D.Variable args in let body = D.Bound z in
    let e = D.Lambda body in let target = Function (a, a) in T.embed_def target;
    let t = T.embed target in let d = D.Abstraction (at, variable) in
    T.embed_wf z a; T.embed_wf z target; D.context_wf_def z empty; D.context_wf_def z env;
    D.scheme_wf_def z scheme; D.add_def z z; D.lookup_def env z;
    D.length_def args; D.arity_def scheme; D.arguments_wf_def z args;
    D.open_scheme_def scheme args; T.open_empty at;
    D.typed_def z env body at variable; D.typed_def z empty e t d; refine_ d)


let (with_identity_choices @ total) : (e : execution) @ immutable ->
    (after : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (tree : bounded) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
    (a : ty) @ immutable ->
    {u : unit | ran (H.empty ()) 1 Generalize_spec.Empty Hm_environment_spec.Empty e after pool
      && let_free e && source e === D.Lambda (D.Bound D.Z)
      && unfolded after tree && result e === Some (bound_root tree)} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices (scheme after 0 tree) === Function (a, a)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool tree rho model a premise claim use -> ghost_ (
    let refine_ premise = premise in let h = H.empty () in
    let empty : pool = Generalize_spec.Empty in let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
    let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 empty x}) @ total = fun x ->
      runtime_at_def h 0 empty x; safe_def h x; depth_bound_def h 0 x;
      covered_def h (-1) empty x; ordered_def h x; let u = () in refine_ u in
    let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 1 empty x}) @ total = fun x ->
      facts x; let u = () in Hm_let_runtime_proofs.enter_runtime h 0 empty x (refine_ u); refine_ u in
    let[@def] initial : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let initial_model : ((x : node Pref.t) @ immutable ->
      {u : unit | Level_unifier_spec.node_equation h initial x}) @ total = fun x ->
      Level_unifier_spec.node_equation_def h initial x; Level_unifier_spec.observe_def h x;
      let u = () in refine_ u in
    Hm_sound_proofs.mono_env_def h env; Hm_sound_proofs.context_def initial env;
    let target = Function (a, a) in let refine_ typing = identity_typing a in
    let consume : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after eta x})) @ total ->
      (preserved : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || eta x === initial x})) @ total ->
      {u : unit | C.matches eta e target} -> {u : unit | claim}) @ total = fun eta next _preserved fit ->
      let refine_ fit = fit in
      let eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after eta x}) @ total = fun x ->
        next x; Level_unifier_spec.node_equation_def after eta x;
        Level_unifier_spec.observe_def after x; equation_def after eta x; let u = () in refine_ u in
      let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (below h x 0) || rho x === eta x}) @ total = fun x ->
        below_def h x 0; let u = () in refine_ u in
      let u = () in Hm_origin_proofs.rhs_interpret h 0 empty facts env e after pool rho model eta eta_model equal tree (refine_ u);
      C.matches_def eta e target;
      let refine_ u = use eta (refine_ u) in refine_ u in
    let u = () in let refine_ u = C.with_run_model h 1 empty child_facts env e after pool initial initial_model target typing (refine_ u) claim consume in refine_ u)

let (with_clean_instance @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable ->
      {s : template | not (H.mem h x) || (root s === x && template h s)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (schema : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && template h schema && target_for h d (root schema) q} ->
    (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices schema} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h scope trees rho model choices epoch depth d schema q premise claim use -> ghost_ (
    let refine_ premise = premise in copy_heap_def h epoch depth d; let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices schema} -> {u : unit | claim}) @ total = fun tau next equal fit ->
      let clean_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x}) @ total = fun x ->
        next x; let u = () in Clean_copy.result_at h epoch depth d x (refine_ u);
        Copy_cleanup_spec.swept_at_def raw after trail x; equation_def raw tau x; equation_def after tau x; refine_ u in
      let refine_ u = use tau (refine_ clean_model) equal fit in refine_ u in
    let u = () in let refine_ u = Copy_template_proofs.with_scheme_instance h scope trees rho model choices epoch depth d schema q (refine_ u) claim consume in refine_ u)

let (runtime_template @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (not (H.mem h x) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable ->
    {s : template | not (H.mem h x) || (root s === x && template h s)} @ immutable ghost =
  fun h depth pool facts forest x -> ghost_ (
    let refine_ tree = forest x in let t = Forest_transport.unfolding tree in
    Forest_transport.unfolding_root tree;
    let s = scheme h depth t in Generalize_scheme_proofs.scheme_root h depth t;
    let empty : pool = Generalize_spec.Empty in
    closed_heap_def h depth empty; pool_scoped_def h empty;
    let coverage : ((y : node Pref.t) @ immutable -> {u : unit | covered h depth empty y}) @ total = fun y ->
      facts y; runtime_at_def h depth pool y; depth_bound_def h depth y;
      covered_def h depth empty y; finite_node_def h y; below_def h y depth; at_level_def h y;
      let u = () in refine_ u in
    let u = () in if H.mem h x then (
      Forest_transport.unfolding_valid h tree (refine_ u);
      Generalize_scheme_proofs.scheme_valid h depth empty coverage t (refine_ u); refine_ s)
    else refine_ s)

let (with_identity_instance @ total) : (e : execution) @ immutable ->
    (rhs_heap : node Pref.heap) @ immutable -> (rhs_pool : pool) @ immutable -> (tree : bounded) @ immutable ->
    (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (not (H.mem h x) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (rhs_model : ((x : node Pref.t) @ immutable -> {u : unit | equation rhs_heap rho x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable -> (q : node Pref.t) @ immutable ->
    (a : ty) @ immutable ->
    {u : unit | ran (H.empty ()) 1 Generalize_spec.Empty Hm_environment_spec.Empty e rhs_heap rhs_pool
      && let_free e && source e === D.Lambda (D.Bound D.Z)
      && unfolded rhs_heap tree && result e === Some (bound_root tree)
      && valid h epoch depth d && template h (scheme rhs_heap 0 tree)
      && target_for h d (root (scheme rhs_heap 0 tree)) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === Function (a, a)} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e rhs_heap rhs_pool tree h depth pool facts forest rho model rhs_model epoch d q a premise claim use -> ghost_ (
    let refine_ premise = premise in let schema = scheme rhs_heap 0 tree in
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total = fun x ->
      facts x; runtime_at_def h depth pool x; safe_def h x; let u = () in refine_ u in
    let trees : ((x : node Pref.t) @ immutable ->
      {s : template | not (H.mem h x) || (root s === x && template h s)} @ immutable) @ total = fun x ->
      let refine_ s = runtime_template h depth pool facts forest x in refine_ s in
    let consume_choices : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices (scheme rhs_heap 0 tree) === Function (a, a)} ->
      {u : unit | claim}) @ total = fun choices fit ->
      let refine_ fit = fit in
      let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau q === interpret rho choices schema} -> {u : unit | claim}) @ total = fun tau next equal assigned ->
        let refine_ assigned = assigned in let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u in
      let u = () in let refine_ u = with_clean_instance h scope trees rho model choices epoch depth d schema q (refine_ u) claim consume in refine_ u in
    let u = () in let refine_ u = with_identity_choices e rhs_heap rhs_pool tree rho rhs_model a (refine_ u) claim consume_choices in refine_ u)

let (with_identity_pair @ total) : (e : execution) @ immutable ->
    (rhs_heap : node Pref.heap) @ immutable -> (rhs_pool : pool) @ immutable -> (tree : bounded) @ immutable ->
    (h : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 pool x})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (epoch1 : node Pref.t) @ immutable -> (d1 : history) @ immutable -> (q1 : node Pref.t) @ immutable ->
    (epoch2 : node Pref.t) @ immutable -> (d2 : history) @ immutable -> (q2 : node Pref.t) @ immutable ->
    (b : ty) @ immutable ->
    {u : unit | ran (H.empty ()) 1 Generalize_spec.Empty Hm_environment_spec.Empty e rhs_heap rhs_pool
      && let_free e && source e === D.Lambda (D.Bound D.Z)
      && unfolded rhs_heap tree && result e === Some (bound_root tree)
      && pool_scoped rhs_heap rhs_pool && h === closed_heap rhs_heap 0 rhs_pool
      && template h (scheme rhs_heap 0 tree) && Hm_environment_spec.boundary_bound h 0 (scheme rhs_heap 0 tree)
      && valid h epoch1 0 d1 && target_for h d1 (root (scheme rhs_heap 0 tree)) q1
      && valid (copy_heap h epoch1 0 d1) epoch2 0 d2
      && target_for (copy_heap h epoch1 0 d1) d2 (root (scheme rhs_heap 0 tree)) q2} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable ->
        {u : unit | equation (copy_heap (copy_heap h epoch1 0 d1) epoch2 0 d2) tau x})) @ total ->
      {u : unit | tau q1 === Function (Function (b, b), Function (b, b)) && tau q2 === Function (b, b)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e rhs_heap rhs_pool tree h pool facts forest rho model epoch1 d1 q1 epoch2 d2 q2 b premise claim use -> ghost_ (
    let refine_ premise = premise in let schema = scheme rhs_heap 0 tree in
    let original = root schema in let h1 = copy_heap h epoch1 0 d1 in let pool1 = Pooled_spec.registered pool epoch1 d1 in
    let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 0 pool1 x}) @ total = fun x ->
      let u = () in let refine_ u = Hm_runtime_proofs.copy_runtime h 0 pool facts epoch1 d1 x (refine_ u) in refine_ u in
    let forest1 : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem h1 x then Level_finite_spec.finite h1 t else Level_unifier_spec.observe h1 x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = Hm_forest_proofs.clean_copy_forest h forest epoch1 0 d1 x (refine_ u) in refine_ t in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | Hm_environment_spec.protected_at h h1 0 x}) @ total = fun x ->
      let u = () in if H.mem h x then (
        Hm_protected_proofs.copy_member h epoch1 0 d1 0 x (refine_ u); refine_ u)
      else (facts x; runtime_at_def h 0 pool x; safe_def h x;
        Hm_environment_spec.protected_at_def h h1 0 x; below_def h x 0; refine_ u) in
    let u = () in Hm_environment_proofs.template_transport h h1 0 frame schema (refine_ u);
    Hm_runtime_proofs.copy_target_active h 0 pool facts epoch1 d1 original q1 (refine_ u); active_def h1 q1;
    let rhs_model : ((x : node Pref.t) @ immutable -> {u : unit | equation rhs_heap rho x}) @ total = fun x ->
      model x; let u = () in Generalize_proofs.closed_model rhs_heap 0 rhs_pool rho x (refine_ u); refine_ u in
    let a = Function (b, b) in
    let consume1 : ((tau1 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next1 : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h epoch1 0 d1) tau1 x})) @ total ->
      (equal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau1 x === rho x})) @ total ->
      {u : unit | tau1 q1 === Function (a, a)} -> {u : unit | claim}) @ total = fun tau1 next1 _equal1 fit1 ->
      let refine_ fit1 = fit1 in
      let rhs_model1 : ((x : node Pref.t) @ immutable -> {u : unit | equation rhs_heap tau1 x}) @ total = fun x ->
        next1 x; facts x; runtime_at_def h 0 pool x; safe_def h x;
        equation_def h1 tau1 x; Level_unifier_spec.observe_def h1 x; Level_unifier_spec.node_equation_def h1 tau1 x;
        Level_unifier_spec.observe_def h x;
        let u = () in Hm_model_proofs.copy_restrict h epoch1 0 d1 tau1 x (refine_ u);
        Level_unifier_spec.node_equation_def h tau1 x; equation_def h tau1 x;
        Generalize_proofs.closed_model rhs_heap 0 rhs_pool tau1 x (refine_ u); refine_ u in
      let consume2 : ((tau2 : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next2 : ((x : node Pref.t) @ immutable -> {u : unit | equation (copy_heap h1 epoch2 0 d2) tau2 x})) @ total ->
        (equal2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || tau2 x === tau1 x})) @ total ->
        {u : unit | tau2 q2 === Function (b, b)} -> {u : unit | claim}) @ total = fun tau2 next2 equal2 fit2 ->
          let refine_ fit2 = fit2 in equal2 q1;
          let u = () in let refine_ u = use tau2 (refine_ next2) (refine_ u) in refine_ u in
      let u = () in let refine_ u = with_identity_instance e rhs_heap rhs_pool tree h1 0 pool1 facts1 (refine_ forest1)
        tau1 (refine_ next1) rhs_model1 epoch2 d2 q2 b (refine_ u) claim consume2 in refine_ u in
    let refine_ u = with_identity_instance e rhs_heap rhs_pool tree h 0 pool facts (refine_ forest)
      rho model rhs_model epoch1 d1 q1 a (refine_ u) claim consume1 in refine_ u)

let rec (scheme_boundary_bound @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (tree : bounded) @ immutable -> {u : unit | pool_scoped h pool && unfolded h tree} ->
    {u : unit | Hm_environment_spec.boundary_bound (closed_heap h cut pool) cut (scheme h cut tree)} @ ghost =
  fun h cut pool order tree premise -> ghost_ (
    let refine_ premise = premise in unfolded_def h tree; scheme_def h cut tree; bound_root_def tree;
    let p = bound_root tree in let s = scheme h cut tree in let after = closed_heap h cut pool in
    let level = at_level h p in close_level_def cut level;
    Hm_environment_spec.boundary_bound_def after cut s;
    let u = () in if not (close_level cut level === Generic) then (
      order p; ordered_def h p; at_level_def h p; below_def h p cut;
      Generalize_proofs.closed_below h cut pool p cut (refine_ u); refine_ u)
    else match tree with Tip _ -> refine_ u
    | Through (_, child) -> scheme_boundary_bound h cut pool order child (refine_ u); refine_ u
    | Fork (_, a, b) -> scheme_boundary_bound h cut pool order a (refine_ u);
      scheme_boundary_bound h cut pool order b (refine_ u); refine_ u)

let (identity_execution @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && source e === D.Lambda (D.Bound D.Z)} ->
    {u : unit | let_free e && not (result e === None)} @ ghost =
  fun h depth pool env e after final_pool premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; source_def e; result_def e; let_free_def e;
    let u = () in match e with RLam (_, body, _, _, _) -> source_def body; result_def body; let_free_def body; refine_ u
    | _ -> refine_ u)

let (with_application_model @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x})) @ total ->
    (f : node Pref.t) @ immutable -> (a : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (arrow : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable -> (d : Optimized_unifier_spec.derivation) @ immutable ->
    (target : ty) @ immutable ->
    {u : unit | H.mem h f && H.mem h a && rho f === Function (rho a, target)
      && allocated h depth p Var && allocated (H.put h p (cell Var depth)) depth arrow (Arrow (a, p))
      && Optimized_unifier_spec.unified (H.put (H.put h p (cell Var depth)) arrow (cell (Arrow (a, p)) depth)) f arrow ok after d} ->
    (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | ok && tau p === target} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h depth pool facts rho model f a p arrow ok after d target premise claim use -> ghost_ (
    let refine_ premise = premise in let var : desc = Var in let v = cell var depth in
    let h1 = H.put h p v in let pool1 = Entry (p, pool) in
    let desc = Arrow (a, p) in let h2 = H.put h1 arrow (cell desc depth) in
    let facts1 = C.allocation_facts h depth pool facts p var (refine_ ()) in
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
            let u = () in let refine_ u = C.unify_complete h2 rho2 model2 f arrow ok after d x (refine_ u) in refine_ u in
          let u = () in C.unify_complete h2 rho2 model2 f arrow ok after d p (refine_ u);
          let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || rho2 x === rho x}) @ total = fun x ->
            equal1 x; equal2 x; let u = () in refine_ u in
          let refine_ u = use rho2 next equal (refine_ u) in refine_ u in
      let u = () in let refine_ u = C.with_alloc h1 depth pool1 (refine_ facts1) rho1 model1 arrow desc value h2 (refine_ u) claim consume2 in refine_ u in
    Copy_model_proofs.describes_def rho var target;
    let u = () in let refine_ u = C.with_alloc h depth pool facts rho model p var target h1 (refine_ u) claim consume1 in refine_ u)

let[@def] (id_id @ total) (_u : unit) = D.Let (D.Lambda (D.Bound D.Z), D.Apply (D.Bound D.Z, D.Bound D.Z))

let (with_id_id_model @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (b : ty) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after final_pool && source e === id_id ()} ->
    (claim : bool) ->
    (use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho x})) @ total ->
      {u : unit | C.matches rho e (Function (b, b))} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after final_pool b premise claim use -> ghost_ (
    let refine_ premise = premise in let initial = H.empty () in let empty : pool = Generalize_spec.Empty in
    let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
    ran_def initial 0 empty env e after final_pool; source_def e; result_def e; id_id_def ();
    let u = () in match e with
    | RLet_left (rhs, _) -> identity_execution initial 1 empty env rhs after final_pool (refine_ u); refine_ u
    | RLet (rhs, body, rhs_heap, rhs_pool) ->
      identity_execution initial 1 empty env rhs rhs_heap rhs_pool (refine_ u);
      (match result rhs with None -> refine_ u | Some original ->
      let h = closed_heap rhs_heap 0 rhs_pool in let pool = Nested_pool_spec.transfer h rhs_pool empty in
      let env1 = Hm_environment_spec.Bind (original, env) in
      let facts0 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at initial 0 empty x}) @ total = fun x ->
        runtime_at_def initial 0 empty x; safe_def initial x; depth_bound_def initial 0 x;
        covered_def initial (-1) empty x; ordered_def initial x; let u = () in refine_ u in
      let child_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at initial 1 empty x}) @ total = fun x ->
        facts0 x; let u = () in Hm_let_runtime_proofs.enter_runtime initial 0 empty x (refine_ u); refine_ u in
      let rhs_facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at rhs_heap 1 rhs_pool x}) @ total = fun x ->
        let u = () in Hm_let_runtime_proofs.run_runtime initial 1 empty child_facts env rhs rhs_heap rhs_pool x (refine_ u); refine_ u in
      let facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h 0 pool x}) @ total = fun x ->
        let u = () in Hm_let_runtime_proofs.close_runtime initial 0 empty facts0 env rhs rhs_heap rhs_pool (refine_ rhs_facts) x (refine_ u); refine_ u in
      let initial_forest : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem initial x then Level_finite_spec.finite initial t else Level_unifier_spec.observe initial x === None)} @ immutable) @ total = fun x ->
        let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t; Level_unifier_spec.observe_def initial x; refine_ t in
      let rhs_forest : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem rhs_heap x then Level_finite_spec.finite rhs_heap t else Level_unifier_spec.observe rhs_heap x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Hm_forest_proofs.run_forest initial initial_forest 1 empty env rhs rhs_heap rhs_pool x (refine_ u) in refine_ t in
      let forest : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable) @ total = fun x ->
        let u = () in let refine_ t = Forest_transport.closed_forest_at rhs_heap rhs_forest 0 rhs_pool x (refine_ u) in refine_ t in
      Hm_execution_proofs.run_result initial 1 empty env rhs rhs_heap rhs_pool original (refine_ u);
      let refine_ finite_tree = rhs_forest original in let tree = Forest_transport.unfolding finite_tree in
      Forest_transport.unfolding_valid rhs_heap finite_tree (refine_ u); Forest_transport.unfolding_root finite_tree;
      let coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered rhs_heap 0 rhs_pool x}) @ total = fun x ->
        rhs_facts x; runtime_at_def rhs_heap 1 rhs_pool x; let u = () in refine_ u in
      let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered rhs_heap x}) @ total = fun x ->
        rhs_facts x; runtime_at_def rhs_heap 1 rhs_pool x; safe_def rhs_heap x; let u = () in refine_ u in
      Generalize_scheme_proofs.scheme_valid rhs_heap 0 rhs_pool coverage tree (refine_ u);
      Generalize_scheme_proofs.scheme_root rhs_heap 0 tree;
      scheme_boundary_bound rhs_heap 0 rhs_pool order tree (refine_ u);
      source_def body; result_def body; ran_def h 0 pool env1 body after final_pool;
      match body with
      | RApp_left (left, _) -> source_def left; result_def left; refine_ u
      | RApp_right (_, right, _, _) -> source_def right; result_def right; refine_ u
      | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, derivation) ->
        source_def left; source_def right; result_def left; result_def right;
        (match left with RVar (i1, q1, epoch1, d1) ->
          (match right with RVar (i2, q2, epoch2, d2) ->
          ran_def h 0 pool env1 left h1 pool1; ran_def h1 0 pool1 env1 right h2 pool2;
          Hm_environment_spec.lookup_def env1 i1; Hm_environment_spec.lookup_def env1 i2;
          let facts1 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1 0 pool1 x}) @ total = fun x ->
            let u = () in Hm_let_runtime_proofs.run_runtime h 0 pool facts env1 left h1 pool1 x (refine_ u); refine_ u in
          let facts2 : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2 0 pool2 x}) @ total = fun x ->
            let u = () in Hm_let_runtime_proofs.run_runtime h1 0 pool1 facts1 env1 right h2 pool2 x (refine_ u); refine_ u in
          Hm_execution_proofs.run_result h 0 pool env1 left h1 pool1 q1 (refine_ u);
          Hm_execution_proofs.run_extends h1 0 pool1 env1 right h2 pool2 q1 (refine_ u);
          Hm_execution_proofs.run_result h1 0 pool1 env1 right h2 pool2 q2 (refine_ u);
          let consume_model : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h rho x})) @ total ->
            {u : unit | claim}) @ total = fun rho model ->
            let converted : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
              model x; Level_unifier_spec.node_equation_def h rho x; Level_unifier_spec.observe_def h x; equation_def h rho x;
              let u = () in refine_ u in
            let consume_pair : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
              (next : ((x : node Pref.t) @ immutable ->
                {u : unit | equation (copy_heap (copy_heap h epoch1 0 d1) epoch2 0 d2) tau x})) @ total ->
              {u : unit | tau q1 === Function (Function (b, b), Function (b, b)) && tau q2 === Function (b, b)} ->
              {u : unit | claim}) @ total = fun tau next fit ->
              let refine_ fit = fit in
              let model2 : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation h2 tau x}) @ total = fun x ->
                next x; equation_def h2 tau x; Level_unifier_spec.node_equation_def h2 tau x; Level_unifier_spec.observe_def h2 x;
                let u = () in refine_ u in
              let target = Function (b, b) in
              let consume_application : ((final : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
                (final_model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after final x})) @ total ->
                (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || final x === tau x})) @ total ->
                {u : unit | ok && final p === target} -> {u : unit | claim}) @ total = fun final final_model _equal assigned ->
                  let refine_ assigned = assigned in C.matches_def final e target;
                  let u = () in let refine_ u = use final final_model (refine_ u) in refine_ u in
              let u = () in let refine_ u = with_application_model h2 0 pool2 facts2 tau model2 q1 q2 p arrow ok after derivation target (refine_ u) claim consume_application in refine_ u in
            let u = () in let refine_ u = with_identity_pair rhs rhs_heap rhs_pool tree h pool facts forest rho converted epoch1 d1 q1 epoch2 d2 q2 b (refine_ u) claim consume_pair in refine_ u in
          let refine_ u = Level_finite_proofs.with_finite_model h forest claim consume_model in refine_ u
          | _ -> refine_ u)
        | _ -> refine_ u)
      | _ -> refine_ u)
    | _ -> refine_ u)

let (id_id_completes @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool && source e === id_id ()} ->
    {u : unit | not (result e === None)} @ ghost = fun e after pool premise -> ghost_ (
    let refine_ premise = premise in let b = Boolean in
    let use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho x})) @ total ->
      {u : unit | C.matches rho e (Function (b, b))} -> {u : unit | not (result e === None)}) @ total = fun rho _model fit ->
        let refine_ fit = fit in let target = Function (b, b) in C.matches_def rho e target; let u = () in refine_ u in
    let claim = not (result e === None) in
    let u = () in let refine_ u = with_id_id_model e after pool b (refine_ u) claim (refine_ use) in refine_ u)

let (id_id_factor @ total) : (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable -> (tree : Level_finite_spec.tree) @ immutable ->
    (b : ty) @ immutable ->
    {u : unit | ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty e after pool && source e === id_id ()
      && result e === Some p && Level_finite_spec.finite after tree && Level_finite_spec.tree_root tree === p} -> (claim : bool) ->
    (use : ((delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | Function (b, b) === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun e after pool p tree b premise claim use -> ghost_ (
    let refine_ premise = premise in
    let consume : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | Level_unifier_spec.node_equation after rho x})) @ total ->
      {u : unit | C.matches rho e (Function (b, b))} -> {u : unit | claim}) @ total = fun rho model fit ->
        let refine_ fit = fit in let target = Function (b, b) in C.matches_def rho e target;
        let u = () in Level_mgu_proofs.readback_factor after rho model tree (refine_ u);
        let refine_ u = use rho (refine_ u) in refine_ u in
    let u = () in let refine_ u = with_id_id_model e after pool b (refine_ u) claim consume in refine_ u)
