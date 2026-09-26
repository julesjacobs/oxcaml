module D = Hm_declarative
module E = Hm_elaboration
module G = Hm_generalization
module Ty = Copy_spec

let (generalization @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (root : Ty.ty) @ immutable ->
    {u : unit | D.context_wf (E.depth scope) context} ->
    {u : unit | let g = G.generalize context (E.interpret scope root) in
      let arity = D.arity g.G.scheme in
      let extended = E.Quantifiers (g.G.variables, scope) in
      D.scheme_wf (E.depth scope) g.G.scheme
      && D.context_wf (E.depth extended) (D.weaken_context arity context)
      && D.depth (D.weaken_context arity context) === D.depth context
      && D.context_wf (E.depth scope) (D.Binding (g.G.scheme, context))
      && D.depth (D.Binding (g.G.scheme, context)) === D.S (D.depth context)
      && E.depth extended === D.add arity (E.depth scope)
      && (match g.G.scheme with D.Forall (k, body) -> arity === k && E.interpret extended root === body)} @ ghost =
  fun scope context root premise -> ghost_ (
    let value = E.interpret scope root in
    let g = G.generalize context value in
    let scheme = g.G.scheme in let arity = D.arity scheme in
    let extended = E.Quantifiers (g.G.variables, scope) in
    let body_context = D.Binding (scheme, context) in
    D.depth_def body_context; Hm_type_proofs.weaken_depth arity context;
    E.interpret_wf scope root; G.scheme_wf (E.depth scope) g.G.variables value ();
    D.arity_def scheme; E.depth_def extended;
    Hm_type_proofs.weaken_context_wf arity (E.depth scope) context ();
    D.context_wf_def (E.depth scope) body_context;
    E.selected_scope scope context root;
    E.interpret_generalized scope g.G.variables root ())

let (recursive_context @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (argument : Ty.ty) @ immutable -> (result : Ty.ty) @ immutable ->
    {u : unit | D.context_wf (E.depth scope) context} ->
    {u : unit | let a = E.interpret scope argument in let b = E.interpret scope result in
      let outer = D.Binding (D.Forall (D.Z, D.Function (a, b)), context) in
      let inner = D.Binding (D.Forall (D.Z, a), outer) in
      D.context_wf (E.depth scope) inner && D.depth inner === D.S (D.S (D.depth context))
      && D.mono_wf (E.depth scope) a && D.mono_wf (E.depth scope) b} @ ghost =
  fun scope context argument result premise -> ghost_ (
    let a = E.interpret scope argument in let b = E.interpret scope result in
    let self = D.Function (a, b) in
    let outer = D.Binding (D.Forall (D.Z, self), context) in
    let inner = D.Binding (D.Forall (D.Z, a), outer) in
    D.depth_def outer; D.depth_def inner;
    E.interpret_wf scope argument; E.interpret_wf scope result;
    D.mono_wf_def (E.depth scope) self;
    Hm_conditional_constraints.binding_wf (E.depth scope) context self ();
    Hm_conditional_constraints.binding_wf (E.depth scope) outer a ())
