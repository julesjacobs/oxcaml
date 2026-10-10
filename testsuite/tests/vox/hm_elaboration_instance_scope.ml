module D = Hm_declarative
module E = Hm_elaboration
module G = Hm_generalization
module Ty = Copy_spec

let (variable @ total) : (n : D.index) @ immutable -> (context : D.context) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    {u : unit | D.typed n context (D.Bound index) ty derivation} ->
    {u : unit | match D.lookup context index with None -> false | Some scheme ->
      match Hm_instantiation.infer scheme ty with None -> false | Some _ -> true} @ ghost =
  fun n context index ty derivation premise -> ghost_ (
    D.typed_def n context (D.Bound index) ty derivation;
    match derivation with
    | D.Variable args -> (match D.lookup context index with
      | None -> () | Some scheme -> Hm_instantiation.complete scheme ty args ())
    | _ -> ())

let (generalized @ total) : (variables : G.variables) @ immutable -> (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (index : D.index) @ immutable -> (ty : Ty.ty) @ immutable ->
    (derivation : D.typing) @ immutable ->
    {u : unit | D.typed (E.depth scope) context (D.Bound index) (E.interpret scope ty) derivation
      && G.excludes_context context variables && E.scope_avoids scope variables} ->
    {u : unit | match D.lookup (D.weaken_context (G.count variables) context) index with
      | None -> false | Some scheme ->
        match Hm_instantiation.infer scheme (E.interpret (E.Quantifiers (variables, scope)) ty) with
        | None -> false | Some _ -> true} @ ghost =
  fun variables scope context index ty derivation premise -> ghost_ (
    let next = Hm_generalization_proofs.typing variables (E.depth scope) context
      (D.Bound index) (E.interpret scope ty) derivation () in
    E.interpret_generalized scope variables ty ();
    variable (D.add (G.count variables) (E.depth scope)) (D.weaken_context (G.count variables) context)
      index (E.interpret (E.Quantifiers (variables, scope)) ty) next ())

let rec (interpret_free @ total) : (p : Ty.node Pref.t) @ immutable ->
    (scope : E.scope) @ immutable -> (ty : Ty.ty) @ immutable ->
    {u : unit | G.occurs p (E.interpret scope ty) =
      (G.occurs p (D.embed ty) && E.parameter p scope === None)} @ ghost =
  fun p scope ty -> ghost_ (
    D.embed_def ty; E.interpret_def scope ty;
    G.occurs_def p (D.embed ty); G.occurs_def p (E.interpret scope ty);
    match ty with
    | Ty.Variable q -> let same = Pref.equal p q in
      (match E.parameter q scope with None -> G.occurs_def p (D.Free q)
      | Some index -> G.occurs_def p (D.Parameter index));
      if same then () else ()
    | Ty.List_type element -> interpret_free p scope element
    | Ty.Function (argument, result) -> interpret_free p scope argument; interpret_free p scope result
    | Ty.Boolean | Ty.Word64 -> ())

let rec (shift_free @ total) : (p : Ty.node Pref.t) @ immutable ->
    (cut : D.index) @ immutable -> (count : D.index) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | G.occurs p (D.shift cut count ty) = G.occurs p ty} @ ghost =
  fun p cut count ty -> ghost_ (
    D.shift_def cut count ty; G.occurs_def p (D.shift cut count ty); G.occurs_def p ty;
    match ty with
    | D.List_type element -> shift_free p cut count element
    | D.Function (argument, result) -> shift_free p cut count argument; shift_free p cut count result
    | D.Free _ | D.Parameter _ | D.Boolean | D.Word64 -> ())

let rec (weaken_free @ total) : (p : Ty.node Pref.t) @ immutable ->
    (count : D.index) @ immutable -> (context : D.context) @ immutable ->
    {u : unit | G.in_context p (D.weaken_context count context) = G.in_context p context} @ ghost =
  fun p count context -> ghost_ (
    D.weaken_context_def count context; G.in_context_def p context;
    G.in_context_def p (D.weaken_context count context);
    match context with
    | D.Empty_context -> ()
    | D.Binding (D.Forall (arity, ty), rest) ->
      D.weaken_scheme_def count (D.Forall (arity, ty));
      shift_free p arity count ty; weaken_free p count rest)

let (generalized_scope_unbound @ total) : (p : Ty.node Pref.t) @ immutable ->
    (scope : E.scope) @ immutable -> (context : D.context) @ immutable -> (ty : Ty.ty) @ immutable ->
    {u : unit | E.parameter p scope === None && not (G.occurs p (E.interpret scope ty))} ->
    {u : unit | E.parameter p (E.Quantifiers ((G.generalize context (E.interpret scope ty)).G.variables, scope)) === None} @ ghost =
  fun p scope context ty premise -> ghost_ (
    let generalized = G.generalize context (E.interpret scope ty) in
    G.collect_member p context (E.interpret scope ty) G.Empty; G.find_def p G.Empty;
    E.parameter_def p (E.Quantifiers (generalized.G.variables, scope)))

let (instance_witness @ total) : (n : D.index) @ immutable ->
    (context : D.context) @ immutable -> (index : D.index) @ immutable ->
    (ty : D.mono) @ immutable ->
    {u : unit | D.context_wf n context && D.mono_wf n ty
      && (match D.lookup context index with None -> false | Some scheme ->
        match Hm_instantiation.infer scheme ty with None -> false | Some _ -> true)} ->
    {d : D.typing | D.typed n context (D.Bound index) ty d} @ immutable ghost =
  fun n context index ty premise -> ghost_ (
    match D.lookup context index with
    | None -> unreachable_ ()
    | Some scheme ->
      Hm_instantiation.well_formed n scheme ty ();
      match Hm_instantiation.infer scheme ty with
      | None -> unreachable_ ()
      | Some args ->
        let d = D.Variable args in
        D.typed_def n context (D.Bound index) ty d; d)

let (under_binding @ total) : (n : D.index) @ immutable ->
    (context : D.context) @ immutable -> (scheme : D.scheme) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable ->
    (derivation : D.typing) @ immutable ->
    {u : unit | D.typed n context (D.Bound index) ty derivation
      && D.scheme_wf n scheme} ->
    {d : D.typing | D.typed n (D.Binding (scheme, context))
      (D.Bound (D.S index)) ty d} @ immutable ghost =
  fun n context scheme index ty derivation premise -> ghost_ (
    D.typed_def n context (D.Bound index) ty derivation;
    D.context_wf_def n (D.Binding (scheme, context));
    D.lookup_def (D.Binding (scheme, context)) (D.S index);
    D.typed_def n (D.Binding (scheme, context)) (D.Bound (D.S index)) ty derivation;
    derivation)

let (generalized_rhs @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (rhs : Ty.ty) @ immutable ->
    (index : D.index) @ immutable -> (ty : Ty.ty) @ immutable ->
    (derivation : D.typing) @ immutable ->
    {u : unit | D.typed (E.depth scope) context (D.Bound index)
      (E.interpret scope ty) derivation} ->
    {d : D.typing | let g = G.generalize context (E.interpret scope rhs) in
      D.typed (E.depth (E.Quantifiers (g.G.variables, scope)))
        (D.weaken_context (D.arity g.G.scheme) context) (D.Bound index)
        (E.interpret (E.Quantifiers (g.G.variables, scope)) ty) d} @ immutable ghost =
  fun scope context rhs index ty derivation premise -> ghost_ (
    let g = G.generalize context (E.interpret scope rhs) in
    E.selected_scope scope context rhs;
    E.interpret_generalized scope g.G.variables ty ();
    E.depth_def (E.Quantifiers (g.G.variables, scope)); D.arity_def g.G.scheme;
    Hm_generalization_proofs.typing g.G.variables (E.depth scope) context
      (D.Bound index) (E.interpret scope ty) derivation ())

let (generalized_binding @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable ->
    (unbound : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Hm_abstraction.position (Hm_freshness_proofs.template_names schema) p === None
        || (not (G.in_context p context) && E.parameter p scope === None)})) @ total ->
    {u : unit | D.context_wf (E.depth scope) context
      && Hm_template_instance_proofs.boundaries_avoid
        (Hm_freshness_proofs.template_names schema) rho schema} ->
    {d : D.typing | D.typed (E.depth scope)
      (D.Binding ((G.generalize context
        (E.interpret scope (Ty.interpret rho Hm_freshness_proofs.variable_choice schema))).G.scheme, context))
      (D.Bound D.Z) (E.interpret scope (Ty.interpret rho choices schema)) d} @ immutable ghost =
  fun scope context rho choices schema unbound premise -> ghost_ (
    let base = Ty.interpret rho Hm_freshness_proofs.variable_choice schema in
    let target = E.interpret scope (Ty.interpret rho choices schema) in
    let g = G.generalize context (E.interpret scope base) in
    Hm_template_generalization.generalized_complete scope context rho choices schema
      (fun p -> unbound p) ();
    E.interpret_wf scope base; G.scheme_wf (E.depth scope) g.G.variables (E.interpret scope base) ();
    E.interpret_wf scope (Ty.interpret rho choices schema);
    D.context_wf_def (E.depth scope) (D.Binding (g.G.scheme, context));
    D.lookup_def (D.Binding (g.G.scheme, context)) D.Z;
    instance_witness (E.depth scope) (D.Binding (g.G.scheme, context)) D.Z target ())

module Templates = Hm_template_instance_proofs
module Env = Hm_environment_spec

let (monomorphic_context_free @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (schemas : Env.templates) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (root : Ty.node Pref.t) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | not (G.in_context p context) || G.in_context p (Templates.context rho schemas)} ->
    {u : unit | not (G.in_context p (D.Binding (D.Forall (D.Z, E.interpret scope (rho root)), context)))
      || G.in_context p (Templates.context rho (Env.Template_binding (Ty.Boundary root, schemas)))} @ ghost =
  fun scope context schemas rho root p premise -> ghost_ (
    Templates.context_def rho (Env.Template_binding (Ty.Boundary root, schemas));
    Templates.scheme_def rho (Ty.Boundary root);
    Hm_freshness_proofs.template_names_def (Ty.Boundary root);
    Hm_abstraction.count_def Hm_abstraction.No_names;
    Templates.body_def Hm_abstraction.No_names rho (Ty.Boundary root);
    G.in_context_def p (D.Binding (D.Forall (D.Z, E.interpret scope (rho root)), context));
    G.in_context_def p (D.Binding (D.Forall (D.Z, D.embed (rho root)), Templates.context rho schemas));
    interpret_free p scope (rho root))

let (generalized_context_free @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (schemas : Env.templates) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (ty : Ty.ty) @ immutable -> (schema : Ty.template) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | not (G.in_context p context) || G.in_context p (Templates.context rho schemas)} ->
    {u : unit | not (G.in_context p (D.Binding ((G.generalize context (E.interpret scope ty)).G.scheme, context)))
      || G.in_context p (Templates.context rho (Env.Template_binding (schema, schemas)))} @ ghost =
  fun scope context schemas rho ty schema p premise -> ghost_ (
    Hm_generalization_instances.binding_free p context (E.interpret scope ty);
    Templates.context_def rho (Env.Template_binding (schema, schemas));
    let scheme = Templates.scheme rho schema in
    G.in_context_def p (D.Binding (scheme, Templates.context rho schemas));
    match scheme with D.Forall (_, _) -> ())

let (rhs_context_free @ total) : (scope : E.scope) @ immutable ->
    (context : D.context) @ immutable -> (schemas : Env.templates) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (ty : Ty.ty) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | not (G.in_context p context) || G.in_context p (Templates.context rho schemas)} ->
    {u : unit | not (G.in_context p (D.weaken_context
      (D.arity (G.generalize context (E.interpret scope ty)).G.scheme) context))
      || G.in_context p (Templates.context rho schemas)} @ ghost =
  fun scope context schemas rho ty p premise -> ghost_ (
    weaken_free p (D.arity (G.generalize context (E.interpret scope ty)).G.scheme) context)
