module Ty = Copy_spec
module Env = Hm_environment_spec
module Scope = Hm_elaboration
module D = Hm_declarative
module G = Hm_generalization
module Instances = Hm_elaboration_instance_scope

let (empty @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (index : D.index) @ immutable ->
    (schema : Ty.template) @ immutable ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | Env.template_lookup Env.No_templates index === Some schema} ->
    {d : D.typing | D.typed (Scope.depth scope) D.Empty_context (D.Bound index)
      (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable ghost =
  fun rho scope index schema choices premise -> ghost_ (
    Env.template_lookup_def Env.No_templates index; unreachable_ ())

let (monomorphic @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (root : Ty.node Pref.t) @ immutable ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | D.context_wf (Scope.depth scope) context} ->
    {d : D.typing | D.typed (Scope.depth scope)
      (D.Binding (D.Forall (D.Z, Scope.interpret scope (rho root)), context)) (D.Bound D.Z)
      (Scope.interpret scope (Ty.interpret rho choices (Ty.Boundary root))) d} @ immutable ghost =
  fun rho scope context root choices premise -> ghost_ (
    let ty = Scope.interpret scope (rho root) in
    let scheme = D.Forall (D.Z, ty) in
    let inner = D.Binding (scheme, context) in
    let args = D.No_arguments in let d = D.Variable args in
    Scope.interpret_wf scope (rho root);
    Hm_conditional_constraints.binding_wf (Scope.depth scope) context ty ();
    Ty.interpret_def rho choices (Ty.Boundary root);
    D.typed_def (Scope.depth scope) inner (D.Bound D.Z) ty d;
    D.lookup_def inner D.Z; D.length_def args; D.arity_def scheme;
    D.arguments_wf_def (Scope.depth scope) args; D.open_scheme_def scheme args;
    Hm_type_proofs.open_empty ty; d)

let (extend @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (schemas : Env.templates) @ immutable ->
    (old : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup schemas index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (schema : Ty.template) @ immutable -> (scheme : D.scheme) @ immutable ->
    (head : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {d : D.typing | D.typed (Scope.depth scope) (D.Binding (scheme, context)) (D.Bound D.Z)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (index : D.index) @ immutable -> (selected : Ty.template) @ immutable ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | D.scheme_wf (Scope.depth scope) scheme
      && Env.template_lookup (Env.Template_binding (schema, schemas)) index === Some selected} ->
    {d : D.typing | D.typed (Scope.depth scope) (D.Binding (scheme, context)) (D.Bound index)
      (Scope.interpret scope (Ty.interpret rho choices selected)) d} @ immutable ghost =
  fun rho scope context schemas old schema scheme head index selected choices premise -> ghost_ (
    Env.template_lookup_def (Env.Template_binding (schema, schemas)) index;
    match index with
    | D.Z -> head choices
    | D.S previous ->
      let witness = old previous selected choices () in
      Instances.under_binding (Scope.depth scope) context scheme previous
        (Scope.interpret scope (Ty.interpret rho choices selected)) witness ())

let (generalized_rhs @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (schemas : Env.templates) @ immutable ->
    (old : ((index : D.index) @ immutable -> (schema : Ty.template) @ immutable ->
      (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {u : unit | Env.template_lookup schemas index === Some schema} ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    (rhs : Ty.ty) @ immutable -> (index : D.index) @ immutable ->
    (schema : Ty.template) @ immutable ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | Env.template_lookup schemas index === Some schema} ->
    {d : D.typing | let g = G.generalize context (Scope.interpret scope rhs) in
      D.typed (Scope.depth (Scope.Quantifiers (g.G.variables, scope)))
        (D.weaken_context (D.arity g.G.scheme) context) (D.Bound index)
        (Scope.interpret (Scope.Quantifiers (g.G.variables, scope)) (Ty.interpret rho choices schema)) d} @ immutable ghost =
  fun rho scope context schemas old rhs index schema choices premise -> ghost_ (
    let witness = old index schema choices () in
    Instances.generalized_rhs scope context rhs index (Ty.interpret rho choices schema) witness ())
