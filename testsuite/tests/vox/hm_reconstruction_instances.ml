module D = Hm_declarative
module A = Hm_annotation_trace
module Scope = Hm_elaboration
module G = Hm_generalization
module Ty = Copy_spec

let[@def] rec (satisfied @ total)
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total) @ total)
    (scope : Scope.scope @ immutable) (context : D.context @ immutable)
    (source : D.term @ immutable) (trace : A.trace @ immutable) = ghost_ (
  match source, trace with
  | D.Bound index, A.Variable_use p ->
    (match D.lookup context index with
    | None -> false
    | Some scheme -> not (Hm_instantiation.infer scheme (Scope.interpret scope (rho p)) === None))
  | D.Truth, A.Boolean_literal _ | D.False, A.False_literal _
  | D.Word _, A.Word_literal _ | D.Nil, A.Empty_list_literal _ -> true
  | D.Lambda body, A.Abstraction (_, argument, child) ->
    satisfied rho scope (D.Binding (D.Forall (D.Z, Scope.interpret scope (rho argument)), context)) body child
  | D.Apply (left, right), A.Application (_, l, r)
  | D.Cons (left, right), A.List_constructor (_, l, r) ->
    satisfied rho scope context left l && satisfied rho scope context right r
  | D.Recursive body, A.Recursion (_, argument, result, child) ->
    let a = Scope.interpret scope (rho argument) in
    let b = Scope.interpret scope (rho result) in
    let outer = D.Binding (D.Forall (D.Z, D.Function (a, b)), context) in
    satisfied rho scope (D.Binding (D.Forall (D.Z, a), outer)) body child
  | D.If (condition, yes, no), A.Conditional child ->
    satisfied rho scope context (Hm_conditional_constraints.encoded condition yes no) child
  | D.CaseList (scrutinee, empty, nonempty), A.List_case child ->
    satisfied rho scope context (Hm_list_case_constraints.encoded scrutinee empty nonempty) child
  | D.Primitive (_, left, right), A.Primitive (_, _, child) ->
    satisfied rho scope context (Hm_primitive_constraints.arguments left right) child
  | D.Let (rhs, body), A.Let_binding (r, b) ->
    (match A.root r with
    | None -> false
    | Some p ->
      let g = G.generalize context (Scope.interpret scope (rho p)) in
      satisfied rho (Scope.Quantifiers (g.G.variables, scope))
        (D.weaken_context (D.arity g.G.scheme) context) rhs r
      && satisfied rho scope (D.Binding (g.G.scheme, context)) body b)
  | _ -> false)

let (variable @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (index : D.index) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    (witness : D.typing) @ immutable ->
    {u : unit | D.typed (Scope.depth scope) context (D.Bound index) (Scope.interpret scope (rho p)) witness} ->
    {u : unit | satisfied rho scope context (D.Bound index) (A.Variable_use p)} @ ghost =
  fun rho scope context index p witness premise -> ghost_ (
    satisfied_def rho scope context (D.Bound index) (A.Variable_use p);
    Hm_elaboration_instance_scope.variable (Scope.depth scope) context index (Scope.interpret scope (rho p)) witness ())
