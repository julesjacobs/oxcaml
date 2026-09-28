open Hm_elaboration
module A = Hm_annotation_trace

let read_member :
    (all : A.trace) @ immutable ghost ->
    (within : {t : A.trace | A.subtrace t all}) @ immutable ghost ->
    (read : ((p : {p : Copy_spec.node Pref.t | A.contains p all}) @ immutable -> Copy_spec.ty @ immutable)) @ local ->
    (p : {p : Copy_spec.node Pref.t | A.contains p within}) @ immutable -> Copy_spec.ty @ immutable =
  fun all within read p ->
    ghost_ (A.subtrace_contains p within all ()); read p

let rec build :
    (annotations : A.trace) @ immutable ghost ->
    (scope : scope) @ immutable -> (context : D.context) @ immutable ->
    (source : D.term) @ immutable -> (trace : {t : A.trace | A.subtrace t annotations}) @ immutable ->
    (read : ((p : {p : Copy_spec.node Pref.t | A.contains p annotations}) @ immutable -> Copy_spec.ty @ immutable))
      @ local -> D.typing option @ immutable =
  fun annotations scope context source trace read ->
  ghost_ (A.children trace annotations ());
  match source, trace with
  | D.Bound i, A.Variable_use p ->
    ghost_ (A.contains_def p trace);
    (match D.lookup context i with
     | None -> None
     | Some scheme ->
       match Hm_instantiation.infer scheme (interpret scope (read_member annotations trace read p)) with
       | None -> None | Some args -> Some (D.Variable args))
  | D.Truth, A.Boolean_literal _ | D.False, A.False_literal _ -> Some D.Constant
  | D.Word word, A.Word_literal (other, _) ->
    if Hmc_word64.equal word other then Some D.Word_constant else None
  | D.Nil, A.Empty_list_literal p ->
    ghost_ (A.contains_def p trace);
    (match interpret scope (read_member annotations trace read p) with
    | D.List_type a -> Some (D.Empty_list a) | _ -> None)
  | D.Lambda source, A.Abstraction (_, argument, body) ->
    ghost_ (A.contains_def argument trace);
    let a = interpret scope (read_member annotations trace read argument) in
    (match build annotations scope (D.Binding (D.Forall (D.Z, a), context))
       source body read with
     | None -> None | Some d -> Some (D.Abstraction (a, d)))
  | D.Cons (f, x), A.List_constructor (Some _, left, right) ->
    (match A.root left with
     | None -> None
     | Some p ->
       ghost_ (A.root_contains left p ());
       let a = interpret scope (read_member annotations left read p) in
       match build annotations scope context f left read with
       | None -> None
       | Some left ->
         match build annotations scope context x right read with
         | None -> None
         | Some right -> Some (D.List_cons (a, left, right)))
  | D.Apply (f, x), A.Application (Some _, left, right) ->
    (match A.root right with
     | None -> None
     | Some p ->
       ghost_ (A.root_contains right p ());
       let a = interpret scope (read_member annotations right read p) in
       match build annotations scope context f left read with
       | None -> None
       | Some left ->
         match build annotations scope context x right read with
         | None -> None
         | Some right -> Some (D.Application (a, left, right)))
  | D.CaseList (scrutinee, empty, nonempty), A.List_case (
      (A.Application (Some _, (A.Application (Some _, (A.Application (Some _, _, st) as c3), et) as c2),
        (A.Abstraction (_, tail, (A.Abstraction (_, head, nt) as head_lambda)) as tail_lambda)) as c1)) ->
    ghost_ (A.children c1 annotations (); A.children c2 annotations (); A.children c3 annotations ();
      A.children tail_lambda annotations (); A.children head_lambda annotations ();
      A.contains_def head head_lambda; A.contains_def tail tail_lambda);
    let a = interpret scope (read_member annotations head_lambda read head) in
    let list = interpret scope (read_member annotations tail_lambda read tail) in
    let inner = D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, list), context)) in
    (match build annotations scope context scrutinee st read with None -> None | Some ds ->
      match build annotations scope context empty et read with None -> None | Some de ->
      match build annotations scope inner nonempty nt read with None -> None | Some dn ->
      Some (D.List_case (a, ds, de, dn)))
  | D.If (c, yes, no), A.Conditional (
      (A.Application (Some _, (A.Application (Some _, (A.Application (Some _, _,
        (A.List_constructor (Some _, ct, _) as c4)) as c3), yt) as c2), nt) as c1)) ->
    ghost_ (A.children c1 annotations (); A.children c2 annotations (); A.children c3 annotations (); A.children c4 annotations ());
    (match build annotations scope context c ct read with None -> None | Some dc ->
      match build annotations scope context yes yt read with None -> None | Some dy ->
      match build annotations scope context no nt read with None -> None | Some dn ->
      Some (D.Conditional (dc, dy, dn)))
  | D.Primitive (op, left, right),
      A.Primitive (other, Some _, (A.List_constructor (_, left_trace,
        (A.List_constructor (_, right_trace, _) as c2)) as c1)) ->
    ghost_ (A.children c1 annotations (); A.children c2 annotations ());
    if op <> other then None else
    (match build annotations scope context left left_trace read with None -> None | Some dl ->
      match build annotations scope context right right_trace read with None -> None | Some dr ->
      Some (D.Word_primitive (dl, dr)))
  | D.Recursive source, A.Recursion (Some _, argument, result, body) ->
    ghost_ (A.contains_def argument trace);
    let a = interpret scope (read_member annotations trace read argument) in
    ghost_ (A.contains_def result trace);
    let b = interpret scope (read_member annotations trace read result) in
    let context = D.Binding (D.Forall (D.Z, a),
      D.Binding (D.Forall (D.Z, D.Function (a, b)), context)) in
    (match build annotations scope context source body read with
     | None -> None | Some d -> Some (D.Recursion (a, b, d)))
  | D.Let (r, b), A.Let_binding (rhs, body) ->
    (match A.root rhs with
     | None -> None
     | Some p ->
       ghost_ (A.root_contains rhs p ());
       let ty = interpret scope (read_member annotations rhs read p) in
       let generalized = G.generalize context ty in
         let scheme = generalized.G.scheme in
         let arity = D.arity scheme in
         let rhs_scope = Quantifiers (generalized.G.variables, scope) in
         match build annotations rhs_scope (D.weaken_context arity context) r rhs read with
         | None -> None
         | Some rhs ->
           match build annotations scope (D.Binding (scheme, context)) b body read with
           | None -> None
           | Some body -> Some (D.Let_binding (scheme, rhs, body)))
  | _ -> None

let elaborate :
    (source : D.term) @ immutable -> (trace : A.trace) @ immutable ->
    (read : (Copy_spec.node Pref.t @ immutable -> Copy_spec.ty @ immutable)) @ local ->
    (root : D.mono) @ immutable ->
    {r : D.typing option |
      match r with None -> true
      | Some d -> D.typed D.Z D.Empty_context source root d} @ immutable =
  fun source trace read root ->
    ghost_ (A.subtrace_refl trace);
    let recorded_read : ((p : {p : Copy_spec.node Pref.t | A.contains p trace}) @ immutable -> Copy_spec.ty @ immutable) @ local =
      fun p -> read p in
    match build trace Empty D.Empty_context source trace recorded_read with
    | None -> None
    | Some d ->
      if Hm_elaboration_check.check D.Z D.Empty_context source root d
      then Some d else None
