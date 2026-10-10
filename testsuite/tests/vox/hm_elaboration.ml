module D = Hm_declarative
module G = Hm_generalization

type scope = Empty | Quantifiers of G.variables * scope [@@inductive]

let[@def] rec (parameter @ total) (p : Copy_spec.node Pref.t @ immutable)
    (scope : scope @ immutable) =
  match scope with
  | Empty -> None
  | Quantifiers (variables, rest) ->
    match G.find p variables with
    | Some i -> Some i
    | None ->
      match parameter p rest with
      | None -> None
      | Some i -> Some (D.add (G.count variables) i)

let[@def] rec (interpret @ total) (scope : scope @ immutable)
    (ty : Copy_spec.ty @ immutable) =
  match ty with
  | Copy_spec.Variable p ->
    (match parameter p scope with
     | None -> D.Free p | Some i -> D.Parameter i)
  | Copy_spec.Boolean -> D.Boolean
  | Copy_spec.Word64 -> D.Word64
  | Copy_spec.List_type a -> D.List_type (interpret scope a)
  | Copy_spec.Function (a, b) ->
    D.Function (interpret scope a, interpret scope b)

let[@def] rec (depth @ total) (scope : scope @ immutable) = ghost_ (
  match scope with Empty -> D.Z | Quantifiers (variables, rest) -> D.add (G.count variables) (depth rest))

let rec (parameter_bound @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (scope : scope) @ immutable ->
    {u : unit | match parameter p scope with None -> true | Some i -> D.present (depth scope) i} @ ghost =
  fun p scope -> ghost_ (
    parameter_def p scope; depth_def scope;
    match scope with Empty -> () | Quantifiers (variables, rest) ->
      G.found_bound p variables;
      match G.find p variables with
      | Some i -> G.present_prefix (G.count variables) (depth rest) i ()
      | None -> parameter_bound p rest;
        match parameter p rest with None -> () | Some i -> G.present_shifted (G.count variables) (depth rest) i ())

let rec (interpret_wf @ total) : (scope : scope) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    {u : unit | D.mono_wf (depth scope) (interpret scope ty)} @ ghost =
  fun scope ty -> ghost_ (
    interpret_def scope ty; D.mono_wf_def (depth scope) (interpret scope ty);
    match ty with
    | Copy_spec.Variable p -> parameter_bound p scope;
      (match parameter p scope with
      | None -> D.mono_wf_def (depth scope) (D.Free p)
      | Some i -> D.mono_wf_def (depth scope) (D.Parameter i))
    | Copy_spec.List_type a -> interpret_wf scope a
    | Copy_spec.Function (a, b) -> interpret_wf scope a; interpret_wf scope b
    | Copy_spec.Boolean | Copy_spec.Word64 -> ())

let rec (interpret_empty @ total) : (ty : Copy_spec.ty) @ immutable ->
    {u : unit | interpret Empty ty === D.embed ty} @ ghost =
  fun ty -> ghost_ (
    interpret_def Empty ty; D.embed_def ty;
    match ty with
    | Copy_spec.Variable p -> parameter_def p Empty
    | Copy_spec.List_type a -> interpret_empty a
    | Copy_spec.Function (a, b) -> interpret_empty a; interpret_empty b
    | Copy_spec.Boolean | Copy_spec.Word64 -> ())

let rec (free_origin @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (scope : scope) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    {u : unit | G.occurs p (interpret scope ty)} -> {u : unit | parameter p scope === None} @ ghost =
  fun p scope ty premise -> ghost_ (
    interpret_def scope ty; G.occurs_def p (interpret scope ty);
    match ty with
    | Copy_spec.Variable q -> (match parameter q scope with
      | Some i -> G.occurs_def p (D.Parameter i)
      | None -> G.occurs_def p (D.Free q); let same = Pref.equal p q in if same then () else ())
    | Copy_spec.List_type a -> free_origin p scope a ()
    | Copy_spec.Function (a, b) -> if G.occurs p (interpret scope a) then free_origin p scope a () else free_origin p scope b ()
    | Copy_spec.Boolean | Copy_spec.Word64 -> ())

let[@def] rec (scope_avoids @ total) (scope : scope @ immutable) (variables : G.variables @ immutable) = ghost_ (
  match variables with G.Empty -> true | G.Variable (p, rest) -> parameter p scope === None && scope_avoids scope rest)

let rec (selected_scope_suffix @ total) : (scope : scope) @ immutable -> (context : D.context) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    (selected : G.variables) @ immutable -> (rest : G.variables) @ immutable ->
    {u : unit | selected === G.collect context (interpret scope ty) G.Empty && G.suffix rest selected} ->
    {u : unit | scope_avoids scope rest} @ ghost =
  fun scope context ty selected rest premise -> ghost_ (
    scope_avoids_def scope rest;
    match rest with G.Empty -> () | G.Variable (p, tail) ->
      G.find_def p rest; let _same = Pref.equal p p in
      G.suffix_find p rest selected ();
      G.collect_member p context (interpret scope ty) G.Empty; G.find_def p G.Empty;
      free_origin p scope ty ();
      G.suffix_refl tail; G.suffix_def tail rest; G.suffix_trans tail rest selected ();
      selected_scope_suffix scope context ty selected tail ())

let (selected_scope @ total) : (scope : scope) @ immutable -> (context : D.context) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    {u : unit | scope_avoids scope (G.collect context (interpret scope ty) G.Empty)} @ ghost =
  fun scope context ty -> ghost_ (
    let selected = G.collect context (interpret scope ty) G.Empty in
    G.suffix_refl selected; selected_scope_suffix scope context ty selected selected ())

let rec (selected_parameter @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (scope : scope) @ immutable -> (variables : G.variables) @ immutable ->
    {u : unit | scope_avoids scope variables && not (G.find p variables === None)} ->
    {u : unit | parameter p scope === None} @ ghost =
  fun p scope variables premise -> ghost_ (
    scope_avoids_def scope variables; G.find_def p variables;
    match variables with G.Empty -> () | G.Variable (q, rest) ->
      let same = Pref.equal p q in if same then () else selected_parameter p scope rest ())

let rec (interpret_generalized @ total) : (scope : scope) @ immutable -> (variables : G.variables) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    {u : unit | scope_avoids scope variables} ->
    {u : unit | interpret (Quantifiers (variables, scope)) ty ===
      G.abstract (G.count variables) variables (interpret scope ty)} @ ghost =
  fun scope variables ty premise -> ghost_ (
    let extended = Quantifiers (variables, scope) in
    interpret_def extended ty; interpret_def scope ty;
    G.abstract_def (G.count variables) variables (interpret scope ty);
    match ty with
    | Copy_spec.Variable p -> parameter_def p extended;
      (match G.find p variables with
      | Some _ -> selected_parameter p scope variables ();
        G.abstract_def (G.count variables) variables (D.Free p)
      | None -> (match parameter p scope with
        | None -> G.abstract_def (G.count variables) variables (D.Free p)
        | Some i -> G.abstract_def (G.count variables) variables (D.Parameter i)))
    | Copy_spec.List_type a -> interpret_generalized scope variables a ()
    | Copy_spec.Function (a, b) -> interpret_generalized scope variables a (); interpret_generalized scope variables b ()
    | Copy_spec.Boolean | Copy_spec.Word64 -> ())
