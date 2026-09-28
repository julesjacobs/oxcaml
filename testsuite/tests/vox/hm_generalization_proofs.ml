module D = Hm_declarative
module G = Hm_generalization
module A = Hm_abstraction

type variables = G.variables

let[@def] rec (names @ total) (variables : variables @ immutable) = ghost_ (
  match variables with G.Empty -> A.No_names | G.Variable (p, rest) -> A.Name (p, names rest))

let rec (names_count @ total) : (variables : variables) @ immutable ->
    {u : unit | A.count (names variables) === G.count variables} @ ghost =
  fun variables -> ghost_ (
    names_def variables; A.count_def (names variables); G.count_def variables;
    match variables with G.Empty -> () | G.Variable (_, rest) -> names_count rest)

let rec (names_position @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | A.position (names variables) p === G.find p variables} @ ghost =
  fun p variables -> ghost_ (
    names_def variables; A.position_def (names variables) p; G.find_def p variables;
    match variables with G.Empty -> () | G.Variable (q, rest) ->
      let same = Pref.equal p q in if same then () else names_position p rest)

let rec (abstract_body @ total) : (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | A.abstract_type (names variables) D.Z ty === G.abstract (G.count variables) variables ty} @ ghost =
  fun variables ty -> ghost_ (
    names_count variables; A.abstract_type_def (names variables) D.Z ty;
    G.abstract_def (G.count variables) variables ty;
    match ty with
    | D.Free p -> names_position p variables; A.abstract_free_def (names variables) D.Z p;
      (match G.find p variables with None -> () | Some i -> D.add_def D.Z i)
    | D.Parameter i -> D.shift_index_def D.Z (G.count variables) i
    | D.List_type a -> abstract_body variables a
    | D.Function (a, b) -> abstract_body variables a; abstract_body variables b
    | D.Boolean | D.Word64 -> ())

let[@def] rec (excluded_type @ total) (variables : variables @ immutable) (ty : D.mono @ immutable) = ghost_ (
  match variables with G.Empty -> true | G.Variable (p, rest) -> not (G.occurs p ty) && excluded_type rest ty)

let rec (binding_excludes @ total) : (variables : variables) @ immutable -> (arity : D.index) @ immutable ->
    (ty : D.mono) @ immutable -> (rest : D.context) @ immutable ->
    {u : unit | G.excludes_context (D.Binding (D.Forall (arity, ty), rest)) variables} ->
    {u : unit | excluded_type variables ty && G.excludes_context rest variables} @ ghost =
  fun variables arity ty rest premise -> ghost_ (
    let context = D.Binding (D.Forall (arity, ty), rest) in
    G.excludes_context_def context variables; G.excludes_context_def rest variables; excluded_type_def variables ty;
    match variables with G.Empty -> () | G.Variable (p, tail) ->
      G.in_context_def p context; binding_excludes tail arity ty rest ())

let rec (excluded_children @ total) : (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | excluded_type variables ty} ->
    {u : unit | match ty with D.List_type a -> excluded_type variables a
      | D.Function (a, b) -> excluded_type variables a && excluded_type variables b | _ -> true} @ ghost =
  fun variables ty premise -> ghost_ (
    excluded_type_def variables ty;
    match ty with
    | D.List_type a -> excluded_type_def variables a;
      (match variables with G.Empty -> () | G.Variable (p, rest) -> G.occurs_def p ty; excluded_children rest ty ())
    | D.Function (a, b) -> excluded_type_def variables a; excluded_type_def variables b;
      (match variables with G.Empty -> () | G.Variable (p, rest) -> G.occurs_def p ty; excluded_children rest ty ())
    | _ -> ())

let rec (excluded_free @ total) : (variables : variables) @ immutable -> (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | excluded_type variables (D.Free p)} -> {u : unit | G.find p variables === None} @ ghost =
  fun variables p premise -> ghost_ (
    excluded_type_def variables (D.Free p); G.find_def p variables;
    match variables with G.Empty -> () | G.Variable (q, rest) ->
      G.occurs_def q (D.Free p);
      let same = Pref.equal p q in let other = Pref.equal q p in
      if same || other then () else excluded_free rest p ())

let rec (avoids_type @ total) : (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | excluded_type variables ty} -> {u : unit | A.avoids (names variables) ty} @ ghost =
  fun variables ty premise -> ghost_ (
    A.avoids_def (names variables) ty; excluded_children variables ty ();
    match ty with
    | D.Free p -> excluded_free variables p (); names_position p variables
    | D.List_type a -> avoids_type variables a ()
    | D.Function (a, b) -> avoids_type variables a (); avoids_type variables b ()
    | D.Parameter _ | D.Boolean | D.Word64 -> ())

let rec (avoids_context @ total) : (variables : variables) @ immutable -> (context : D.context) @ immutable ->
    {u : unit | G.excludes_context context variables} -> {u : unit | A.context_avoids (names variables) context} @ ghost =
  fun variables context premise -> ghost_ (
    A.context_avoids_def (names variables) context;
    match context with D.Empty_context -> () | D.Binding (D.Forall (arity, ty), rest) ->
      binding_excludes variables arity ty rest (); avoids_type variables ty (); avoids_context variables rest ();
      A.scheme_avoids_def (names variables) (D.Forall (arity, ty)))

let (typing @ total) : (variables : variables) @ immutable -> (n : D.index) @ immutable ->
    (context : D.context) @ immutable -> (term : D.term) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    {u : unit | D.typed n context term ty derivation && G.excludes_context context variables} ->
    {out : D.typing | D.typed (D.add (G.count variables) n) (D.weaken_context (G.count variables) context)
      term (G.abstract (G.count variables) variables ty) out} @ immutable ghost =
  fun variables n context term ty derivation premise -> ghost_ (
    names_count variables; abstract_body variables ty; avoids_context variables context ();
    Hm_abstraction_proofs.generalize_typing (names variables) n context term ty derivation ();
    A.abstract_typing (names variables) D.Z derivation)
