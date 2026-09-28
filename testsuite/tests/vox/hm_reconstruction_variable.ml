module Ty = Copy_spec
module H = Pref.Heap
module U = Level_unifier_spec
module E = Effective_level
module T = Effective_template
module Env = Hm_environment_spec
module Environment = Hm_effective_environment
module Scope = Hm_elaboration
module D = Hm_declarative
module R = Representative_level
module A = Hm_annotation_trace
module Instances = Hm_reconstruction_instances

let (shared @ total) :
    (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (depth : int) -> (env : Env.env) @ immutable -> (schemas : Env.templates) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (instances : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup schemas index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (index : D.index) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    (rep : R.representative) @ immutable ->
    {u : unit | Environment.effective_env heap heads depth env schemas
      && Env.lookup env index === Some p && U.resolves heap p rep.R.root rep.R.path
      && Level_spec.active heap rep.R.root} ->
    {u : unit | Instances.satisfied rho scope context (D.Bound index) (A.Variable_use p)} @ ghost =
  fun heap heads valid depth env schemas rho scope context instances index p rep premise -> ghost_ (
    let schema = Environment.lookup_schema heap heads depth env schemas index p () in
    valid p; U.resolves_def heap p rep.R.root rep.R.path;
    E.valid_head_def heap heads p; let actual = heads p in
    R.unique heap p rep.R.root rep.R.path actual.R.root actual.R.path ();
    E.level_def heap heads p;
    Level_spec.active_def heap rep.R.root; Level_spec.at_level_def heap rep.R.root;
    T.valid_template_def heap heads schema; Ty.root_def schema;
    (match schema with
    | Ty.Boundary _ -> ()
    | Ty.Parameter q | Ty.Constant q | Ty.Word_constant q | Ty.List_template (q, _)
    | Ty.Product (q, _, _) | Ty.Indirect (q, _) -> T.generic_def heap heads q);
    let choices = Hm_freshness_proofs.variable_choice in
    let witness = instances index schema choices () in
    Ty.interpret_def rho choices schema;
    Instances.variable rho scope context index p witness ())

let (copied @ total) :
    (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p})) @ total ->
    (depth : int) -> (env : Env.env) @ immutable -> (schemas : Env.templates) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (instances : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup schemas index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (index : D.index) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    (original : Ty.node Pref.t) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : Ty.node Pref.t) @ immutable -> (history : Ty.history) @ immutable ->
    (model : ((q : Ty.node Pref.t) @ immutable ->
      {u : unit | U.node_equation (Hm_execution_spec.copy_heap heap epoch depth history) rho q})) @ total ->
    {u : unit | Environment.effective_env heap heads depth env schemas
      && Env.lookup env index === Some original
      && Copy_certificate_spec.certifies heap certificate epoch depth history original p} ->
    {u : unit | Instances.satisfied rho scope context (D.Bound index) (A.Variable_use p)} @ ghost =
  fun heap heads valid depth env schemas rho scope context instances index p original certificate epoch history model premise -> ghost_ (
    let schema = Environment.lookup_schema heap heads depth env schemas index original () in
    let selected : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable) @ total = fun choices ->
      instances index schema choices () in
    let witness = Hm_elaboration_binding.copied heap heads valid certificate depth epoch history schema p rho model
      scope context index selected () in
    Instances.variable rho scope context index p witness ())
