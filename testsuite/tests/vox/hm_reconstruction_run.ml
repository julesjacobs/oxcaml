module Ty = Copy_spec
module H = Pref.Heap
module U = Level_unifier_spec
module F = Level_finite_spec
module E = Effective_level
module R = Representative_level
module Pool = Generalize_spec
module Env = Hm_environment_spec
module Environment = Hm_effective_environment
module C = Hm_effective_environment_proofs
module Run = Hm_effective_execution_spec
module Runtime = Hm_effective_runtime
module Forest = Hm_effective_forest
module Model = Hm_effective_model
module Fresh = Hm_elaboration_freshness
module Generic = Hm_effective_generic
module Scope = Hm_elaboration
module D = Hm_declarative
module G = Hm_generalization
module A = Hm_annotation_trace
module Trace = Hm_annotation_trace_spec
module Eq = Hm_annotation_equations
module Instances = Hm_reconstruction_instances
module Templates = Hm_template_instance_proofs

let rec (run @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : Pool.pool) @ immutable ->
    (facts : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at heap heads depth pool p})) @ total ->
    (env : Env.env) @ immutable -> (schemas : Env.templates) @ immutable ->
    (execution : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable ->
    (final : Ty.node Pref.heap) @ immutable ->
    (final_trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem final p then F.finite final t else U.observe final p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (final_trees p)})) @ total ->
    (model : ((p : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation after rho p})) @ total ->
    (owned : ((p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem after p) || H.mem final p})) @ total ->
    (future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || Fresh.generic_at after p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected p})) @ total ->
    (preserve : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path after p r desc} ->
      {s : R.representative | Generic.generic_path final p s desc} @ immutable)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (support : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (G.in_context p context) || G.in_context p (Templates.context rho schemas)})) @ total ->
    (instances : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup schemas index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (fresh : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      {u : unit | Fresh.generic_at after p r} -> {u : unit | Scope.parameter p scope === None})) @ total ->
    (trace : A.trace) @ immutable ->
    {u : unit | Run.ran heap depth pool env execution after final_pool
      && Environment.effective_env heap heads depth env schemas
      && not (Run.result execution === None) && Trace.records trace execution
      && D.context_wf (Scope.depth scope) context} ->
    {u : unit | Instances.satisfied rho scope context (Run.source execution) trace} @ ghost =
  fun heap heads trees depth pool facts env schemas execution after final_pool final final_trees rho values model owned future preserve scope context support instances fresh trace premise -> ghost_ (
    Run.ran_def heap depth pool env execution after final_pool;
    Run.result_def execution; Run.source_def execution; Trace.records_def trace execution;
    Instances.satisfied_def rho scope context (Run.source execution) trace;
    let valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p}) @ total = fun p ->
      facts p; Runtime.runtime_at_def heap heads depth pool p; Runtime.safe_def heap heads p in
    match execution, trace with
    | Run.RShared (index, p, rep), A.Variable_use _ ->
      Hm_reconstruction_variable.shared heap heads valid depth env schemas rho scope context instances index p rep ();
      Instances.satisfied_def rho scope context (D.Bound index) (A.Variable_use p)
    | Run.RVar (index, p, epoch, history, certificate), A.Variable_use _ ->
      (match Env.lookup env index with None -> () | Some original ->
        Run.copy_heap_def heap epoch depth history; Hm_execution_spec.copy_heap_def heap epoch depth history;
        let copy_model : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | U.node_equation (Hm_execution_spec.copy_heap heap epoch depth history) rho q}) @ total = fun q -> model q in
        Hm_reconstruction_variable.copied heap heads valid depth env schemas rho scope context instances index p
          original certificate epoch history copy_model ();
        Instances.satisfied_def rho scope context (D.Bound index) (A.Variable_use p))
    | Run.RBool _, A.Boolean_literal _ | Run.RFalse _, A.False_literal _
    | Run.RWord _, A.Word_literal _ | Run.RNil _, A.Empty_list_literal _ -> ()
    | Run.RIf (_, _, _, body), A.Conditional child
    | Run.RCaseList (_, _, _, body), A.List_case child ->
      run heap heads trees depth pool facts env schemas body after final_pool final final_trees rho values model owned
        future preserve scope context support instances fresh child ()
    | Run.RPrimitive (op, _, _, body, middle, body_pool, out), A.Primitive (_, _, child) ->
      (match Run.result body, out with
      | Some _, Some p ->
        let desc = Run.primitive_desc op in let node = Ty.cell desc depth in
        Run.allocated_def middle depth p desc;
        let middle_trees : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem middle q then F.finite middle t else U.observe middle q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest heap trees depth pool env body middle body_pool q () in
        let middle_model : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation middle rho q}) @ total = fun q ->
          let _tree = middle_trees q in model q; Model.allocation_restrict middle p node rho q () in
        let middle_owned : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem middle q) || H.mem final q}) @ total = fun q ->
          owned q in
        let middle_preserve : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.allocate middle p node q r desc (); preserve q r desc () in
        let middle_fresh : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at middle q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Fresh.generic_allocate middle p node q r (); fresh q r () in
        let middle_future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at middle protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at middle q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            closed x; Ty.cell_def desc depth; Run.primitive_desc_def op;
            Fresh.write middle protected p node x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_allocate middle p node x r (); r in
          future protected next_closed next_generic q in
        run heap heads trees depth pool facts env schemas body middle body_pool final final_trees rho values
          middle_model middle_owned middle_future middle_preserve scope context support instances middle_fresh child ()
      | _ -> ())
    | Run.RLam (argument, body, middle, body_pool, out), A.Abstraction (_, _, child) ->
      (match Run.result body, out with
      | Some result, Some p ->
        let var = Ty.cell Ty.Var depth in let start = H.put heap argument var in
        let child_pool = Pool.Entry (argument, pool) in
        let child_env = Env.Bind (argument, env) in
        let schema = Ty.Boundary argument in
        let child_schemas = Env.Template_binding (schema, schemas) in
        Run.allocated_def heap depth argument Ty.Var; Ty.cell_def Ty.Var depth;
        let allocated = Forest.allocated_forest heap trees depth argument Ty.Var () in
        let child_trees : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem start q then F.finite start t else U.observe start q === None)} @ immutable) @ total = fun q -> allocated q in
        let[@def] child_heads : E.heads = fun q -> Forest_heads.select start child_trees q in
        let child_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head start child_heads q}) @ total = fun q ->
          child_heads_def q; let _r = Forest_heads.select start child_trees q in E.valid_head_def start child_heads q in
        let child_facts : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | Runtime.runtime_at start child_heads depth child_pool q}) @ total = fun q ->
          facts q; Hm_effective_allocation.allocate_runtime heap heads child_heads depth pool argument Ty.Var valid (fun x -> child_valid x) q () in
        C.allocation_environment heap heads child_heads valid argument var (fun x -> child_valid x) depth env schemas ();
        child_valid argument; Hm_effective_allocation.allocated_below heap depth argument Ty.Var child_heads ();
        Environment.bind_schema start child_heads depth env schemas argument ();
        let middle_trees : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem middle q then F.finite middle t else U.observe middle q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest start child_trees depth child_pool child_env body middle body_pool q () in
        let[@def] middle_heads : E.heads = fun q -> Forest_heads.select middle middle_trees q in
        let middle_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads q}) @ total = fun q ->
          middle_heads_def q; let _r = Forest_heads.select middle middle_trees q in E.valid_head_def middle middle_heads q in
        C.run_environment start child_heads middle_heads child_valid depth child_pool child_env child_schemas body middle body_pool middle_valid depth ();
        Environment.effective_env_def middle middle_heads depth child_env child_schemas;
        Effective_template.valid_template_def middle middle_heads schema;
        Effective_template.finite_def middle middle_heads argument;
        middle_valid argument; E.valid_head_def middle middle_heads argument; E.level_def middle middle_heads argument;
        let finite_argument = middle_heads argument in
        Ty.finite_node_def middle finite_argument.R.root; Level_spec.at_level_def middle finite_argument.R.root;
        Hm_effective_result.finite_path_def middle argument finite_argument;
        let finite_result = Hm_effective_result.run start child_trees depth child_pool child_env body middle body_pool result () in
        let desc = Ty.Arrow (argument, result) in let node = Ty.cell desc depth in
        Run.allocated_def middle depth p desc;
        let middle_model : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation middle rho q}) @ total = fun q ->
          let _tree = middle_trees q in model q; Model.allocation_restrict middle p node rho q () in
        let middle_owned : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem middle q) || H.mem final q}) @ total = fun q ->
          owned q in
        let middle_preserve : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.allocate middle p node q r desc (); preserve q r desc () in
        let middle_fresh : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at middle q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Fresh.generic_allocate middle p node q r (); fresh q r () in
        let middle_future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at middle protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at middle q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          Fresh.finite_unprotected middle protected generic argument finite_argument ();
          Fresh.finite_unprotected middle protected generic result finite_result ();
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            closed x; Ty.cell_def desc depth; Fresh.write middle protected p node x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_allocate middle p node x r (); r in
          future protected next_closed next_generic q in
        let a = Scope.interpret scope (rho argument) in
        let scheme = D.Forall (D.Z, a) in let inner = D.Binding (scheme, context) in
        Scope.interpret_wf scope (rho argument);
        Hm_conditional_constraints.binding_wf (Scope.depth scope) context a ();
        D.context_wf_def (Scope.depth scope) inner;
        let child_support : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | not (G.in_context q inner) || G.in_context q (Templates.context rho child_schemas)}) @ total = fun q ->
          support q; Hm_elaboration_instance_scope.monomorphic_context_free scope context schemas rho argument q () in
        let head : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {d : D.typing | D.typed (Scope.depth scope) inner (D.Bound D.Z)
            (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable) @ total = fun choices ->
          Hm_reconstruction_environment.monomorphic rho scope context argument choices () in
        let child_instances : ((index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
          (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {u : unit | Env.template_lookup child_schemas index === Some selected} ->
          {d : D.typing | D.typed (Scope.depth scope) inner (D.Bound index)
            (Scope.interpret scope (Ty.interpret rho choices selected)) d} @ immutable) @ total = fun index selected choices premise ->
          Hm_reconstruction_environment.extend rho scope context schemas instances schema scheme (fun choices -> head choices) index selected choices () in
        run start child_heads child_trees depth child_pool child_facts child_env child_schemas body middle body_pool
          final final_trees rho values middle_model middle_owned middle_future middle_preserve scope inner child_support child_instances middle_fresh child ()
      | _ -> ())
    | Run.RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, derivation), A.Application (_, l, r) ->
      (match Run.result left, Run.result right with
      | Some f, Some a ->
        let trees1 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h1 q then F.finite h1 t else U.observe h1 q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest heap trees depth pool env left h1 pool1 q () in
        let[@def] heads1 : E.heads = fun q -> Forest_heads.select h1 trees1 q in
        let heads1_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 q}) @ total = fun q ->
          heads1_def q; let _r = Forest_heads.select h1 trees1 q in E.valid_head_def h1 heads1 q in
        let facts1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at h1 heads1 depth pool1 q}) @ total = fun q ->
          Hm_effective_invariant.run_invariant heap heads trees depth pool facts env left h1 heads1 heads1_valid pool1 q () in
        C.run_environment heap heads heads1 valid depth pool env schemas left h1 pool1 heads1_valid depth ();
        let trees2 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h2 q then F.finite h2 t else U.observe h2 q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest h1 trees1 depth pool1 env right h2 pool2 q () in
        let finite_child = Hm_effective_result.run h1 trees1 depth pool1 env right h2 pool2 a () in
        let node3 = Ty.cell Ty.Var depth in let h3 = H.put h2 p node3 in
        let node4 = Ty.cell (Ty.Arrow (a, p)) depth in let h4 = H.put h3 arrow node4 in
        Run.allocated_def h2 depth p Ty.Var; Run.allocated_def h3 depth arrow (Ty.Arrow (a, p));
        let allocated3 = Forest.allocated_forest h2 trees2 depth p Ty.Var () in
        let trees3 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h3 q then F.finite h3 t else U.observe h3 q === None)} @ immutable) @ total = fun q ->
          allocated3 q in
        let model4 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h4 rho q}) @ total = fun q ->
          Effective_unifier_model.success_forward_at h4 rho f arrow after derivation model q () in
        let model3 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h3 rho q}) @ total = fun q ->
          let _tree = trees3 q in model4 q; Model.allocation_restrict h3 arrow node4 rho q () in
        let model2 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h2 rho q}) @ total = fun q ->
          let _tree = trees2 q in model3 q; Model.allocation_restrict h2 p node3 rho q () in
        let model1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h1 rho q}) @ total = fun q ->
          Model.run_restrict h1 trees1 depth pool1 env right h2 pool2 rho model2 q () in
        let owned2 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem h2 q) || H.mem final q}) @ total = fun q ->
          Effective_unifier_frame.unified_frame h4 f arrow ok after derivation q (); owned q in
        let owned1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem h1 q) || H.mem final q}) @ total = fun q ->
          Hm_effective_membership.run_extends h1 depth pool1 env right h2 pool2 q (); owned2 q in
        let to_after : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h2 q r desc} ->
          {s : R.representative | Generic.generic_path after q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.allocate h2 p node3 q r desc (); Generic.allocate h3 arrow node4 q r desc ();
          Generic.unify h4 f arrow ok after derivation q r desc () in
        let preserve2 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h2 q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = to_after q r desc () in preserve q next desc () in
        let fresh2 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at h2 q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Hm_elaboration_continuation.scope_before h2 after scope to_after fresh q r () in
        let future2 : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h2 protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at h2 q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          Fresh.finite_unprotected h2 protected generic a finite_child ();
          Fresh.fresh_unprotected h2 protected generic p ();
          Ty.cell_def Ty.Var depth; Ty.cell_def (Ty.Arrow (a, p)) depth;
          let closed3 : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h3 protected x}) @ total = fun x ->
            closed x; Fresh.write h2 protected p node3 x () in
          let generic3 : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at h3 x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_allocate h2 p node3 x r (); r in
          let closed4 : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h4 protected x}) @ total = fun x ->
            closed3 x; Fresh.write h3 protected arrow node4 x () in
          let generic4 : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at h4 x r} @ immutable) @ total = fun x ->
            let r = generic3 x in if protected x then Fresh.generic_allocate h3 arrow node4 x r (); r in
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            Fresh.unification h4 f arrow ok after derivation protected closed4 generic4 x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic4 x in if protected x then Fresh.generic_unify h4 f arrow ok after derivation x r () else r in
          future protected next_closed next_generic q in
        let preserve1 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h1 q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = Generic.run h1 depth pool1 env right h2 pool2 q r desc () in preserve2 q next desc () in
        let fresh1 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at h1 q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          let next = Fresh.generic_run h1 depth pool1 env right h2 pool2 q r () in fresh2 q next () in
        let future1 : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h1 protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at h1 q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h2 protected x}) @ total = fun x ->
            Fresh.run h1 trees1 depth pool1 env right h2 pool2 protected closed generic x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at h2 x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_run h1 depth pool1 env right h2 pool2 x r () else r in
          future2 protected next_closed next_generic q in
        run heap heads trees depth pool facts env schemas left h1 pool1 final final_trees rho values model1 owned1
          future1 preserve1 scope context support instances fresh1 l ();
        run h1 heads1 trees1 depth pool1 facts1 env schemas right h2 pool2 final final_trees rho values model2 owned2
          future2 preserve2 scope context support instances fresh2 r ()
      | _ -> ())
    | Run.RCons (left, right, h1, pool1, h2, pool2, p, ok, derivation), A.List_constructor (_, l, r) ->
      (match Run.result left, Run.result right with
      | Some f, Some a ->
        let trees1 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h1 q then F.finite h1 t else U.observe h1 q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest heap trees depth pool env left h1 pool1 q () in
        let[@def] heads1 : E.heads = fun q -> Forest_heads.select h1 trees1 q in
        let heads1_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 q}) @ total = fun q ->
          heads1_def q; let _r = Forest_heads.select h1 trees1 q in E.valid_head_def h1 heads1 q in
        let facts1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at h1 heads1 depth pool1 q}) @ total = fun q ->
          Hm_effective_invariant.run_invariant heap heads trees depth pool facts env left h1 heads1 heads1_valid pool1 q () in
        C.run_environment heap heads heads1 valid depth pool env schemas left h1 pool1 heads1_valid depth ();
        let trees2 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h2 q then F.finite h2 t else U.observe h2 q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest h1 trees1 depth pool1 env right h2 pool2 q () in
        let[@def] heads2 : E.heads = fun q -> Forest_heads.select h2 trees2 q in
        let heads2_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h2 heads2 q}) @ total = fun q ->
          heads2_def q; let _r = Forest_heads.select h2 trees2 q in E.valid_head_def h2 heads2 q in
        facts1 f; Hm_effective_result.result_below heap trees depth pool env left h1 pool1 heads1 f ();
        heads1_valid f; heads2_valid f;
        Hm_effective_paths.run_below h1 heads1 heads2 depth pool1 env right h2 pool2 f depth ();
        E.effective_below_def h2 heads2 f depth; E.valid_head_def h2 heads2 f; E.level_def h2 heads2 f;
        let finite_child = heads2 f in
        Ty.finite_node_def h2 finite_child.R.root; Level_spec.at_level_def h2 finite_child.R.root;
        Hm_effective_result.finite_path_def h2 f finite_child;
        let node3 = Ty.cell (Ty.List f) depth in let h3 = H.put h2 p node3 in
        Run.allocated_def h2 depth p (Ty.List f);
        let model3 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h3 rho q}) @ total = fun q ->
          Effective_unifier_model.success_forward_at h3 rho a p after derivation model q () in
        let model2 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h2 rho q}) @ total = fun q ->
          let _tree = trees2 q in model3 q; Model.allocation_restrict h2 p node3 rho q () in
        let model1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation h1 rho q}) @ total = fun q ->
          Model.run_restrict h1 trees1 depth pool1 env right h2 pool2 rho model2 q () in
        let owned2 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem h2 q) || H.mem final q}) @ total = fun q ->
          Effective_unifier_frame.unified_frame h3 a p ok after derivation q (); owned q in
        let owned1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem h1 q) || H.mem final q}) @ total = fun q ->
          Hm_effective_membership.run_extends h1 depth pool1 env right h2 pool2 q (); owned2 q in
        let to_after : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h2 q r desc} ->
          {s : R.representative | Generic.generic_path after q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.allocate h2 p node3 q r desc (); Generic.unify h3 a p ok after derivation q r desc () in
        let preserve2 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h2 q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = to_after q r desc () in preserve q next desc () in
        let fresh2 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at h2 q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Hm_elaboration_continuation.scope_before h2 after scope to_after fresh q r () in
        let future2 : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h2 protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at h2 q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          Fresh.finite_unprotected h2 protected generic f finite_child ();
          Ty.cell_def (Ty.List f) depth;
          let closed3 : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h3 protected x}) @ total = fun x ->
            closed x; Fresh.write h2 protected p node3 x () in
          let generic3 : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at h3 x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_allocate h2 p node3 x r (); r in
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            Fresh.unification h3 a p ok after derivation protected closed3 generic3 x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic3 x in if protected x then Fresh.generic_unify h3 a p ok after derivation x r () else r in
          future protected next_closed next_generic q in
        let preserve1 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path h1 q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = Generic.run h1 depth pool1 env right h2 pool2 q r desc () in preserve2 q next desc () in
        let fresh1 : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at h1 q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          let next = Fresh.generic_run h1 depth pool1 env right h2 pool2 q r () in fresh2 q next () in
        let future1 : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h1 protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at h1 q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at h2 protected x}) @ total = fun x ->
            Fresh.run h1 trees1 depth pool1 env right h2 pool2 protected closed generic x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at h2 x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_run h1 depth pool1 env right h2 pool2 x r () else r in
          future2 protected next_closed next_generic q in
        run heap heads trees depth pool facts env schemas left h1 pool1 final final_trees rho values model1 owned1
          future1 preserve1 scope context support instances fresh1 l ();
        run h1 heads1 trees1 depth pool1 facts1 env schemas right h2 pool2 final final_trees rho values model2 owned2
          future2 preserve2 scope context support instances fresh2 r ()
      | _ -> ())
    | Run.RRec (argument, result, self, body, middle, body_pool, finish), A.Recursion (_, _, _, child) ->
      (match Run.result body, finish with
      | Some body_root, Run.Unified (ok, derivation) ->
        let var = Ty.cell Ty.Var depth in
        let h1 = H.put heap argument var in let pool1 = Pool.Entry (argument, pool) in
        let h2 = H.put h1 result var in let pool2 = Pool.Entry (result, pool1) in
        let desc = Ty.Arrow (argument, result) in let node = Ty.cell desc depth in
        let h3 = H.put h2 self node in let pool3 = Pool.Entry (self, pool2) in
        let self_env = Env.Bind (self, env) in let env3 = Env.Bind (argument, self_env) in
        Run.allocated_def heap depth argument Ty.Var; Run.allocated_def h1 depth result Ty.Var;
        Run.allocated_def h2 depth self desc;
        Ty.cell_def Ty.Var depth; Ty.cell_def desc depth;
        let allocated1 = Forest.allocated_forest heap trees depth argument Ty.Var () in
        let trees1 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h1 q then F.finite h1 t else U.observe h1 q === None)} @ immutable) @ total = fun q ->
          allocated1 q in
        let[@def] heads1 : E.heads = fun q -> Forest_heads.select h1 trees1 q in
        let heads1_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h1 heads1 q}) @ total = fun q ->
          heads1_def q; let _r = Forest_heads.select h1 trees1 q in E.valid_head_def h1 heads1 q in
        let facts1 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at h1 heads1 depth pool1 q}) @ total = fun q ->
          facts q; Hm_effective_allocation.allocate_runtime heap heads heads1 depth pool argument Ty.Var valid (fun x -> heads1_valid x) q () in
        C.allocation_environment heap heads heads1 valid argument var (fun x -> heads1_valid x) depth env schemas ();
        let allocated2 = Forest.allocated_forest h1 trees1 depth result Ty.Var () in
        let trees2 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h2 q then F.finite h2 t else U.observe h2 q === None)} @ immutable) @ total = fun q ->
          allocated2 q in
        let[@def] heads2 : E.heads = fun q -> Forest_heads.select h2 trees2 q in
        let heads2_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h2 heads2 q}) @ total = fun q ->
          heads2_def q; let _r = Forest_heads.select h2 trees2 q in E.valid_head_def h2 heads2 q in
        let facts2 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at h2 heads2 depth pool2 q}) @ total = fun q ->
          facts1 q; Hm_effective_allocation.allocate_runtime h1 heads1 heads2 depth pool1 result Ty.Var heads1_valid (fun x -> heads2_valid x) q () in
        C.allocation_environment h1 heads1 heads2 heads1_valid result var (fun x -> heads2_valid x) depth env schemas ();
        let allocated3 = Forest.allocated_forest h2 trees2 depth self desc () in
        let trees3 : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem h3 q then F.finite h3 t else U.observe h3 q === None)} @ immutable) @ total = fun q ->
          allocated3 q in
        let[@def] heads3 : E.heads = fun q -> Forest_heads.select h3 trees3 q in
        let heads3_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head h3 heads3 q}) @ total = fun q ->
          heads3_def q; let _r = Forest_heads.select h3 trees3 q in E.valid_head_def h3 heads3 q in
        heads1_valid argument; heads2_valid argument; heads2_valid result;
        Hm_effective_allocation.allocated_below heap depth argument Ty.Var heads1 ();
        Hm_effective_allocation.saved_below h1 heads1 heads2 result var argument depth ();
        Hm_effective_allocation.allocated_below h1 depth result Ty.Var heads2 ();
        let facts3 : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at h3 heads3 depth pool3 q}) @ total = fun q ->
          facts2 q; Hm_effective_allocation.allocate_runtime h2 heads2 heads3 depth pool2 self desc heads2_valid (fun x -> heads3_valid x) q () in
        C.allocation_environment h2 heads2 heads3 heads2_valid self node (fun x -> heads3_valid x) depth env schemas ();
        heads3_valid argument; heads3_valid self;
        Hm_effective_allocation.saved_below h2 heads2 heads3 self node argument depth ();
        Hm_effective_allocation.allocated_below h2 depth self desc heads3 ();
        Environment.bind_schema h3 heads3 depth env schemas self ();
        let self_schema = Ty.Boundary self in let self_schemas = Env.Template_binding (self_schema, schemas) in
        Environment.bind_schema h3 heads3 depth self_env self_schemas argument ();
        let argument_schema = Ty.Boundary argument in let schemas3 = Env.Template_binding (argument_schema, self_schemas) in
        let middle_model : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation middle rho q}) @ total = fun q ->
          Effective_unifier_model.success_forward_at middle rho body_root result after derivation model q () in
        let middle_owned : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem middle q) || H.mem final q}) @ total = fun q ->
          Effective_unifier_frame.unified_frame middle body_root result ok after derivation q (); owned q in
        let to_after : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path after q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.unify middle body_root result ok after derivation q r desc () in
        let middle_preserve : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = to_after q r desc () in preserve q next desc () in
        let middle_fresh : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at middle q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Hm_elaboration_continuation.scope_before middle after scope to_after fresh q r () in
        let middle_future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at middle protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at middle q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            Fresh.unification middle body_root result ok after derivation protected closed generic x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_unify middle body_root result ok after derivation x r () else r in
          future protected next_closed next_generic q in
        Eq.run heap trees depth pool env execution after final_pool rho model trace ();
        Eq.equations_def rho trace;
        Scope.interpret_def scope (rho self);
        let self_scheme = D.Forall (D.Z, Scope.interpret scope (rho self)) in
        let outer = D.Binding (self_scheme, context) in
        Scope.interpret_wf scope (rho self);
        Hm_conditional_constraints.binding_wf (Scope.depth scope) context (Scope.interpret scope (rho self)) ();
        D.context_wf_def (Scope.depth scope) outer;
        let self_support : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | not (G.in_context q outer) || G.in_context q (Templates.context rho self_schemas)}) @ total = fun q ->
          support q; Hm_elaboration_instance_scope.monomorphic_context_free scope context schemas rho self q () in
        let head_self : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {d : D.typing | D.typed (Scope.depth scope) outer (D.Bound D.Z)
            (Scope.interpret scope (Ty.interpret rho choices self_schema)) d} @ immutable) @ total = fun choices ->
          Hm_reconstruction_environment.monomorphic rho scope context self choices () in
        let self_instances : ((index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
          (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {u : unit | Env.template_lookup self_schemas index === Some selected} ->
          {d : D.typing | D.typed (Scope.depth scope) outer (D.Bound index)
            (Scope.interpret scope (Ty.interpret rho choices selected)) d} @ immutable) @ total = fun index selected choices premise ->
          Hm_reconstruction_environment.extend rho scope context schemas instances self_schema self_scheme
            (fun choices -> head_self choices) index selected choices () in
        let argument_scheme = D.Forall (D.Z, Scope.interpret scope (rho argument)) in
        let inner = D.Binding (argument_scheme, outer) in
        Scope.interpret_wf scope (rho argument);
        Hm_conditional_constraints.binding_wf (Scope.depth scope) outer (Scope.interpret scope (rho argument)) ();
        D.context_wf_def (Scope.depth scope) inner;
        let child_support : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | not (G.in_context q inner) || G.in_context q (Templates.context rho schemas3)}) @ total = fun q ->
          self_support q; Hm_elaboration_instance_scope.monomorphic_context_free scope outer self_schemas rho argument q () in
        let head_argument : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {d : D.typing | D.typed (Scope.depth scope) inner (D.Bound D.Z)
            (Scope.interpret scope (Ty.interpret rho choices argument_schema)) d} @ immutable) @ total = fun choices ->
          Hm_reconstruction_environment.monomorphic rho scope outer argument choices () in
        let child_instances : ((index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
          (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {u : unit | Env.template_lookup schemas3 index === Some selected} ->
          {d : D.typing | D.typed (Scope.depth scope) inner (D.Bound index)
            (Scope.interpret scope (Ty.interpret rho choices selected)) d} @ immutable) @ total = fun index selected choices premise ->
          Hm_reconstruction_environment.extend rho scope outer self_schemas self_instances argument_schema argument_scheme
            (fun choices -> head_argument choices) index selected choices () in
        run h3 heads3 trees3 depth pool3 facts3 env3 schemas3 body middle body_pool final final_trees rho values middle_model middle_owned
          middle_future middle_preserve scope inner child_support child_instances middle_fresh child ()
      | _ -> ())
    | Run.RLet (rhs, body, middle, child_pool), A.Let_binding (rtrace, btrace) ->
      (match Run.result rhs with
      | Some original ->
        let child_depth = depth + 1 in let empty = Pool.Empty in
        Run.ran_def heap child_depth empty env rhs middle child_pool;
        let child_facts : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at heap heads child_depth empty q}) @ total = fun q ->
          facts q; Runtime.enter_runtime heap heads depth pool q () in
        C.environment_raise heap heads depth child_depth env schemas ();
        let middle_trees : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem middle q then F.finite middle t else U.observe middle q === None)} @ immutable) @ total = fun q ->
          Forest.run_forest heap trees child_depth empty env rhs middle child_pool q () in
        let[@def] middle_heads : E.heads = fun q -> Forest_heads.select middle middle_trees q in
        let middle_heads_valid : ((q : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head middle middle_heads q}) @ total = fun q ->
          middle_heads_def q; let _r = Forest_heads.select middle middle_trees q in E.valid_head_def middle middle_heads q in
        let middle_facts : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at middle middle_heads child_depth child_pool q}) @ total = fun q ->
          Hm_effective_invariant.run_invariant heap heads trees child_depth empty child_facts env rhs middle middle_heads middle_heads_valid child_pool q () in
        let middle_safe : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.safe middle middle_heads q}) @ total = fun q ->
          middle_facts q; Runtime.runtime_at_def middle middle_heads child_depth child_pool q in
        let start = Representative_pool_spec.close_heap middle depth child_pool in
        let transferred = Representative_pool_spec.transfer_rep start child_pool pool in
        let body_env = Env.Bind (original, env) in
        let start_facts : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at start middle_heads depth transferred q}) @ total = fun q ->
          Hm_effective_closing.close_after_run heap heads middle_heads depth pool facts env rhs middle child_pool (fun x -> middle_facts x) q () in
        let start_safe : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.safe start middle_heads q}) @ total = fun q ->
          start_facts q; Runtime.runtime_at_def start middle_heads depth transferred q in
        let start_trees : ((q : Ty.node Pref.t) @ immutable ->
          {t : F.tree | F.tree_root t === q &&
            (if H.mem start q then F.finite start t else U.observe start q === None)} @ immutable) @ total = fun q ->
          Forest.representative_closed_forest middle middle_trees depth child_pool q () in
        let finite_result = Hm_effective_result.run heap trees child_depth empty env rhs middle child_pool original () in
        Hm_effective_result.finite_path_def middle original finite_result;
        U.resolves_def middle original finite_result.R.root finite_result.R.path;
        middle_facts original;
        Hm_effective_result.result_below heap trees child_depth empty env rhs middle child_pool middle_heads original ();
        let finite_tree = middle_trees original in
        Forest_transport.unfolding_valid middle finite_tree (); Forest_transport.unfolding_root finite_tree;
        let schema = Effective_template.scheme middle middle_heads depth finite_tree in
        let coverage : ((q : Ty.node Pref.t) @ immutable -> {u : unit | R.representative_covered middle depth child_pool q}) @ total = fun q ->
          middle_facts q; Runtime.runtime_at_def middle middle_heads child_depth child_pool q in
        Effective_template.scheme_valid middle middle_heads middle_heads_valid depth child_pool coverage finite_tree ();
        Effective_template.scheme_root middle middle_heads depth finite_tree;
        let levels : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | match E.level middle middle_heads q with Ty.Generic -> true | Ty.Finite n -> n >= 0}) @ total = fun q ->
          middle_facts q; Runtime.runtime_at_def middle middle_heads child_depth child_pool q;
          Runtime.depth_bound_def middle middle_heads child_depth q; E.effective_below_def middle middle_heads q child_depth;
          E.level_def middle middle_heads q in
        Effective_template.scheme_boundary middle middle_heads depth child_pool middle_heads_valid levels finite_tree ();
        C.run_environment heap heads middle_heads valid child_depth empty env schemas rhs middle child_pool middle_heads_valid depth ();
        let close_frame : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | Effective_template.protected middle middle_heads start middle_heads depth q}) @ total = fun q ->
          middle_heads_valid q; Effective_template.close_protected middle middle_heads depth child_pool depth q () in
        Environment.transport middle middle_heads start middle_heads depth close_frame env schemas ();
        let body_schemas = Env.Template_binding (schema, schemas) in
        Environment.effective_env_def start middle_heads depth body_env body_schemas;
        let start_model : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation start rho q}) @ total = fun q ->
          Model.run_restrict start start_trees depth transferred body_env body after final_pool rho model q () in
        let middle_model : ((q : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation middle rho q}) @ total = fun q ->
          start_model q; Eq.closed_model middle depth child_pool rho q () in
        let start_owned : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem start q) || H.mem final q}) @ total = fun q ->
          Hm_effective_membership.run_extends start depth transferred body_env body after final_pool q (); owned q in
        let middle_owned : ((q : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem middle q) || H.mem final q}) @ total = fun q ->
          Representative_pool_spec.close_heap_def middle depth child_pool;
          R.representatives_scoped middle child_pool ();
          let filtered = R.representatives middle child_pool in
          Generalize_proofs.closed_observe middle depth filtered q ();
          Pool.closed_at_def middle start depth filtered q; start_owned q in
        let start_to_after : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path start q r desc} ->
          {s : R.representative | Generic.generic_path after q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.run start depth transferred body_env body after final_pool q r desc () in
        let start_preserve : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path start q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = start_to_after q r desc () in preserve q next desc () in
        let start_fresh : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at start q r} -> {u : unit | Scope.parameter q scope === None}) @ total = fun q r premise ->
          Hm_elaboration_continuation.scope_before start after scope start_to_after fresh q r () in
        let start_future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at start protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at start q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          let next_closed : ((x : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected x}) @ total = fun x ->
            Fresh.run start start_trees depth transferred body_env body after final_pool protected closed generic x () in
          let next_generic : ((x : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected x) || Fresh.generic_at after x r} @ immutable) @ total = fun x ->
            let r = generic x in if protected x then Fresh.generic_run start depth transferred body_env body after final_pool x r () else r in
          future protected next_closed next_generic q in
        let middle_to_after : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path after q s desc} @ immutable) @ total = fun q r desc premise ->
          Generic.close middle depth child_pool q r desc (); start_to_after q r desc () in
        let middle_preserve : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path middle q r desc} ->
          {s : R.representative | Generic.generic_path final q s desc} @ immutable) @ total = fun q r desc premise ->
          let next = middle_to_after q r desc () in preserve q next desc () in
        let middle_future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
          (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at middle protected q})) @ total ->
          (generic : ((q : Ty.node Pref.t) @ immutable ->
            {r : R.representative | not (protected q) || Fresh.generic_at middle q r} @ immutable)) @ total ->
          (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected q}) @ total = fun protected closed generic q ->
          Hm_elaboration_continuation.let_suffix middle middle_trees depth child_pool transferred body_env body after final_pool final
            future protected closed generic q () in
        let g = G.generalize context (Scope.interpret scope (rho original)) in
        let scheme = g.G.scheme in let extended = Scope.Quantifiers (g.G.variables, scope) in
        let rhs_context = D.weaken_context (D.arity scheme) context in
        let body_context = D.Binding (scheme, context) in
        Hm_elaboration_preparation.generalization scope context (rho original) ();
        let rhs_fresh : ((q : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
          {u : unit | Fresh.generic_at middle q r} -> {u : unit | Scope.parameter q extended === None}) @ total = fun q r premise ->
          Hm_elaboration_continuation.scope_before middle after scope middle_to_after fresh q r ();
          Hm_elaboration_continuation.generalized_let_scope middle middle_heads middle_safe final middle_owned middle_trees depth child_pool
            transferred body_env body after final_pool future final_trees rho values scope context original finite_result q r () in
        let rhs_support : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | not (G.in_context q rhs_context) || G.in_context q (Templates.context rho schemas)}) @ total = fun q ->
          support q; Hm_elaboration_instance_scope.rhs_context_free scope context schemas rho (rho original) q () in
        let rhs_instances : ((index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
          (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {u : unit | Env.template_lookup schemas index === Some selected} ->
          {d : D.typing | D.typed (Scope.depth extended) rhs_context (D.Bound index)
            (Scope.interpret extended (Ty.interpret rho choices selected)) d} @ immutable) @ total = fun index selected choices premise ->
          Hm_reconstruction_environment.generalized_rhs rho scope context schemas instances (rho original) index selected choices () in
        let body_support : ((q : Ty.node Pref.t) @ immutable ->
          {u : unit | not (G.in_context q body_context) || G.in_context q (Templates.context rho body_schemas)}) @ total = fun q ->
          support q; Hm_elaboration_instance_scope.generalized_context_free scope context schemas rho (rho original) schema q () in
        let head : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {d : D.typing | D.typed (Scope.depth scope) body_context (D.Bound D.Z)
            (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable) @ total = fun choices ->
          Hm_elaboration_binding.generalized start middle_heads start_safe final start_owned start_future start_preserve final_trees rho values
            depth env schemas scope context support start_fresh schema choices () in
        let body_instances : ((index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
          (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
          {u : unit | Env.template_lookup body_schemas index === Some selected} ->
          {d : D.typing | D.typed (Scope.depth scope) body_context (D.Bound index)
            (Scope.interpret scope (Ty.interpret rho choices selected)) d} @ immutable) @ total = fun index selected choices premise ->
          Hm_reconstruction_environment.extend rho scope context schemas instances schema scheme (fun choices -> head choices) index selected choices () in
        Trace.root_agrees rtrace rhs ();
        run heap heads trees child_depth empty child_facts env schemas rhs middle child_pool final final_trees rho values middle_model middle_owned
          middle_future middle_preserve extended rhs_context rhs_support rhs_instances rhs_fresh rtrace ();
        run start middle_heads start_trees depth transferred start_facts body_env body_schemas body after final_pool final final_trees rho values model owned
          future preserve scope body_context body_support body_instances fresh btrace ()
      | None -> ())
    | _ -> ())

let (closed @ total) : (execution : Run.execution) @ immutable ->
    (heap : Ty.node Pref.heap) @ immutable -> (pool : Pool.pool) @ immutable ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (trace : A.trace) @ immutable ->
    {u : unit | Run.ran (H.empty ()) 0 Pool.Empty Env.Empty execution heap pool
      && not (Run.result execution === None) && Trace.records trace execution} ->
    {u : unit | Instances.satisfied rho Scope.Empty D.Empty_context (Run.source execution) trace} @ ghost =
  fun execution heap pool trees rho values trace premise -> ghost_ (
    let empty = H.empty () in
    let initial_trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem empty p then F.finite empty t else U.observe empty p === None)} @ immutable) @ total = fun p ->
      let t = F.Free p in F.tree_root_def t; U.observe_def empty p; t in
    let[@def] heads : E.heads = fun p -> Forest_heads.select empty initial_trees p in
    let facts : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Runtime.runtime_at empty heads 0 Pool.Empty p}) @ total = fun p ->
      Runtime.runtime_at_def empty heads 0 Pool.Empty p; Runtime.safe_def empty heads p;
      Runtime.depth_bound_def empty heads 0 p; R.representative_covered_def empty (-1) Pool.Empty p;
      U.terminal_def empty p; U.observe_def empty p;
      E.effective_ordered_def empty heads p; E.valid_head_def empty heads p in
    let model : ((p : Ty.node Pref.t) @ immutable -> {u : unit | U.node_equation heap rho p}) @ total = fun p ->
      Level_finite_proofs.readback_model_at heap trees rho (fun q -> values q) p in
    let owned : ((p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem heap p) || H.mem heap p}) @ total = fun _p -> () in
    let future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || Fresh.generic_at heap p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected p}) @ total = fun _protected closed _generic p -> closed p in
    let preserve : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path heap p r desc} ->
      {s : R.representative | Generic.generic_path heap p s desc} @ immutable) @ total = fun _p r _desc _premise -> r in
    let support : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (G.in_context p D.Empty_context) || G.in_context p (Templates.context rho Env.No_templates)}) @ total = fun p ->
      G.in_context_def p D.Empty_context in
    let instances : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup Env.No_templates index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth Scope.Empty) D.Empty_context (D.Bound index)
        (Scope.interpret Scope.Empty (Ty.interpret rho choices schema)) d} @ immutable) @ total = fun index schema choices premise ->
      Hm_reconstruction_environment.empty rho Scope.Empty index schema choices () in
    let fresh : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      {u : unit | Fresh.generic_at heap p r} -> {u : unit | Scope.parameter p Scope.Empty === None}) @ total = fun p _r _premise ->
      Scope.parameter_def p Scope.Empty in
    Environment.effective_env_def empty heads 0 Env.Empty Env.No_templates;
    Scope.depth_def Scope.Empty; D.context_wf_def D.Z D.Empty_context;
    run empty heads initial_trees 0 Pool.Empty (fun p -> facts p) Env.Empty Env.No_templates execution heap pool heap trees rho values
      model owned future preserve Scope.Empty D.Empty_context (fun p -> support p)
      (fun index schema choices u -> instances index schema choices u) (fun p r u -> fresh p r u) trace ())
