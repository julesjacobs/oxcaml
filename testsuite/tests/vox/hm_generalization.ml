module D = Hm_declarative

type variables = Empty | Variable of Copy_spec.node Pref.t * variables
[@@inductive]

type generalized = { scheme : D.scheme; variables : variables }

let[@def] rec (find @ total) (p : Copy_spec.node Pref.t @ immutable)
    (variables : variables @ immutable) =
  match variables with
  | Empty -> None
  | Variable (q, rest) ->
    if Pref.equal p q then Some D.Z else
    match find p rest with None -> None | Some i -> Some (D.S i)

let[@def] rec (count @ total) (variables : variables @ immutable) =
  match variables with Empty -> D.Z | Variable (_, rest) -> D.S (count rest)

let[@def] rec (occurs @ total) (p : Copy_spec.node Pref.t @ immutable)
    (ty : D.mono @ immutable) =
  match ty with
  | D.Free q -> Pref.equal p q
  | D.List_type a -> occurs p a
  | D.Function (a, b) -> occurs p a || occurs p b
  | D.Parameter _ | D.Boolean | D.Word64 -> false

let[@def] rec (in_context @ total) (p : Copy_spec.node Pref.t @ immutable)
    (context : D.context @ immutable) =
  match context with
  | D.Empty_context -> false
  | D.Binding (D.Forall (_, ty), rest) -> occurs p ty || in_context p rest

let[@def] rec (collect @ total) (context : D.context @ immutable)
    (ty : D.mono @ immutable) (variables : variables @ immutable) =
  match ty with
  | D.Free p ->
    if in_context p context then variables else
    (match find p variables with
     | Some _ -> variables | None -> Variable (p, variables))
  | D.List_type a -> collect context a variables
  | D.Function (a, b) -> collect context b (collect context a variables)
  | D.Parameter _ | D.Boolean | D.Word64 -> variables

let[@def] rec (abstract @ total) (arity : D.index @ immutable)
    (variables : variables @ immutable) (ty : D.mono @ immutable) =
  match ty with
  | D.Free p ->
    (match find p variables with None -> ty | Some i -> D.Parameter i)
  | D.Parameter i -> D.Parameter (D.add arity i)
  | D.Boolean -> D.Boolean
  | D.Word64 -> D.Word64
  | D.List_type a -> D.List_type (abstract arity variables a)
  | D.Function (a, b) ->
    D.Function (abstract arity variables a, abstract arity variables b)

let[@def] rec (arguments @ total) (variables : variables @ immutable) =
  match variables with
  | Empty -> D.No_arguments
  | Variable (p, rest) -> D.Argument (D.Free p, arguments rest)

let rec (argument_count @ total) : (variables : variables) @ immutable ->
    {u : unit | D.length (arguments variables) === count variables} @ ghost =
  fun variables -> ghost_ (
    arguments_def variables; count_def variables; D.length_def (arguments variables);
    match variables with Empty -> () | Variable (_, rest) -> argument_count rest)
let rec (open_found @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (variables : variables) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | find p variables === Some index} ->
    {u : unit | D.open_index (arguments variables) index === D.Free p} @ ghost =
  fun p variables index premise -> ghost_ (
    find_def p variables; arguments_def variables;
    D.open_index_def (arguments variables) index;
    match variables with
    | Empty -> ()
    | Variable (q, rest) ->
      let same = Pref.equal p q in
      if same then () else (match find p rest with
      | None -> () | Some inner -> open_found p rest inner ()))
let rec (open_shifted @ total) : (variables : variables) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | D.open_index (arguments variables) (D.add (count variables) index) === D.Parameter index} @ ghost =
  fun variables index -> ghost_ (
    count_def variables; arguments_def variables; D.add_def (count variables) index;
    D.open_index_def (arguments variables) (D.add (count variables) index);
    match variables with Empty -> () | Variable (_, rest) -> open_shifted rest index)
let rec (restore @ total) : (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | D.open_type (arguments variables) (abstract (count variables) variables ty) === ty} @ ghost =
  fun variables ty -> ghost_ (
    abstract_def (count variables) variables ty;
    D.open_type_def (arguments variables) (abstract (count variables) variables ty);
    match ty with
    | D.Free p -> (match find p variables with None -> () | Some index -> open_found p variables index ())
    | D.Parameter index -> open_shifted variables index
    | D.List_type element -> restore variables element
    | D.Function (argument, result) -> restore variables argument; restore variables result
    | D.Boolean | D.Word64 -> ())

let[@def] rec (excludes_context @ total) (context : D.context @ immutable) (variables : variables @ immutable) = ghost_ (
  match variables with Empty -> true | Variable (p, rest) -> not (in_context p context) && excludes_context context rest)

let[@def] rec (unique @ total) (variables : variables @ immutable) = ghost_ (
  match variables with Empty -> true | Variable (p, rest) -> find p rest === None && unique rest)

let[@def] rec (suffix @ total) (before : variables @ immutable) (after : variables @ immutable) = ghost_ (
  before === after || match after with Empty -> false | Variable (_, rest) -> suffix before rest)

let (suffix_refl @ total) : (variables : variables) @ immutable -> {u : unit | suffix variables variables} @ ghost =
  fun variables -> ghost_ (suffix_def variables variables)

let rec (suffix_trans @ total) : (a : variables) @ immutable -> (b : variables) @ immutable -> (c : variables) @ immutable ->
    {u : unit | suffix a b && suffix b c} -> {u : unit | suffix a c} @ ghost =
  fun a b c premise -> ghost_ (
    suffix_def a c; suffix_def b c;
    if b === c then () else match c with Empty -> () | Variable (_, rest) -> suffix_trans a b rest ())

let rec (suffix_find @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (before : variables) @ immutable -> (after : variables) @ immutable ->
    {u : unit | suffix before after && not (find p before === None)} -> {u : unit | not (find p after === None)} @ ghost =
  fun p before after premise -> ghost_ (
    suffix_def before after; find_def p after;
    if before === after then () else match after with
    | Empty -> ()
    | Variable (q, rest) -> let same = Pref.equal p q in
      if same then () else suffix_find p before rest ())

let rec (collect_suffix @ total) : (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | suffix variables (collect context ty variables)} @ ghost =
  fun context ty variables -> ghost_ (
    collect_def context ty variables; suffix_refl variables;
    match ty with
    | D.Free p -> if in_context p context then () else (match find p variables with
      | Some _ -> () | None -> suffix_def variables (Variable (p, variables)))
    | D.List_type a -> collect_suffix context a variables
    | D.Function (a, b) -> collect_suffix context a variables;
      let middle = collect context a variables in
      collect_suffix context b middle; suffix_trans variables middle (collect context b middle) ()
    | _ -> ())

let rec (collect_excludes @ total) : (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | excludes_context context variables} ->
    {u : unit | excludes_context context (collect context ty variables)} @ ghost =
  fun context ty variables premise -> ghost_ (
    collect_def context ty variables;
    match ty with
    | D.Free p -> if in_context p context then () else (match find p variables with
      | Some _ -> () | None -> excludes_context_def context (Variable (p, variables)))
    | D.List_type a -> collect_excludes context a variables ()
    | D.Function (a, b) -> collect_excludes context a variables (); collect_excludes context b (collect context a variables) ()
    | _ -> ())

let rec (collect_unique @ total) : (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | unique variables} -> {u : unit | unique (collect context ty variables)} @ ghost =
  fun context ty variables premise -> ghost_ (
    collect_def context ty variables;
    match ty with
    | D.Free p -> if in_context p context then () else (match find p variables with
      | Some _ -> () | None -> unique_def (Variable (p, variables)))
    | D.List_type a -> collect_unique context a variables ()
    | D.Function (a, b) -> collect_unique context a variables (); collect_unique context b (collect context a variables) ()
    | _ -> ())

let[@def] rec (covers @ total) (context : D.context @ immutable) (ty : D.mono @ immutable) (variables : variables @ immutable) = ghost_ (
  match ty with
  | D.Free p -> in_context p context || not (find p variables === None)
  | D.List_type a -> covers context a variables
  | D.Function (a, b) -> covers context a variables && covers context b variables
  | _ -> true)

let rec (covers_suffix @ total) : (context : D.context) @ immutable -> (ty : D.mono) @ immutable ->
    (before : variables) @ immutable -> (after : variables) @ immutable ->
    {u : unit | covers context ty before && suffix before after} -> {u : unit | covers context ty after} @ ghost =
  fun context ty before after premise -> ghost_ (
    covers_def context ty before; covers_def context ty after;
    match ty with
    | D.Free p -> if in_context p context then () else suffix_find p before after ()
    | D.List_type a -> covers_suffix context a before after ()
    | D.Function (a, b) -> covers_suffix context a before after (); covers_suffix context b before after ()
    | _ -> ())

let rec (collect_covers @ total) : (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | covers context ty (collect context ty variables)} @ ghost =
  fun context ty variables -> ghost_ (
    collect_def context ty variables; covers_def context ty (collect context ty variables);
    match ty with
    | D.Free p -> if in_context p context then () else (match find p variables with
      | Some _ -> () | None -> find_def p (Variable (p, variables)); let same = Pref.equal p p in if same then () else ())
    | D.List_type a -> collect_covers context a variables
    | D.Function (a, b) -> collect_covers context a variables;
      let middle = collect context a variables in
      collect_covers context b middle; collect_suffix context b middle;
      covers_suffix context a middle (collect context b middle) ()
    | _ -> ())

let rec (collect_member @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | not (find p (collect context ty variables) === None) =
      (not (find p variables === None) || (occurs p ty && not (in_context p context)))} @ ghost =
  fun p context ty variables -> ghost_ (
    collect_def context ty variables; occurs_def p ty;
    match ty with
    | D.Free q -> let _same = Pref.equal p q in
      if in_context q context then () else (match find q variables with
      | Some _ -> () | None -> find_def p (Variable (q, variables)))
    | D.List_type a -> collect_member p context a variables
    | D.Function (a, b) -> collect_member p context a variables;
      collect_member p context b (collect context a variables)
    | _ -> ())

let rec (found_bound @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (variables : variables) @ immutable ->
    {u : unit | match find p variables with None -> true | Some i -> D.present (count variables) i} @ ghost =
  fun p variables -> ghost_ (
    find_def p variables; count_def variables;
    match variables with Empty -> () | Variable (q, rest) ->
      let same = Pref.equal p q in
      if same then D.present_def (count variables) D.Z else (
        found_bound p rest;
        match find p rest with None -> () | Some i -> D.present_def (count variables) (D.S i)))

let rec (present_prefix @ total) : (prefix : D.index) @ immutable -> (rest : D.index) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | D.present prefix index} -> {u : unit | D.present (D.add prefix rest) index} @ ghost =
  fun prefix rest index premise -> ghost_ (
    D.present_def prefix index; D.add_def prefix rest; D.present_def (D.add prefix rest) index;
    match prefix, index with D.S prefix, D.S index -> present_prefix prefix rest index () | _ -> ())

let rec (present_shifted @ total) : (prefix : D.index) @ immutable -> (rest : D.index) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | D.present rest index} -> {u : unit | D.present (D.add prefix rest) (D.add prefix index)} @ ghost =
  fun prefix rest index premise -> ghost_ (
    D.add_def prefix rest; D.add_def prefix index;
    match prefix with D.Z -> () | D.S prefix ->
      D.present_def (D.add (D.S prefix) rest) (D.add (D.S prefix) index); present_shifted prefix rest index ())

let rec (abstract_wf @ total) : (n : D.index) @ immutable -> (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | D.mono_wf n ty} ->
    {u : unit | D.mono_wf (D.add (count variables) n) (abstract (count variables) variables ty)} @ ghost =
  fun n variables ty premise -> ghost_ (
    abstract_def (count variables) variables ty; D.mono_wf_def n ty;
    D.mono_wf_def (D.add (count variables) n) (abstract (count variables) variables ty);
    match ty with
    | D.Free p -> found_bound p variables;
      (match find p variables with None -> D.mono_wf_def (D.add (count variables) n) ty
      | Some i -> present_prefix (count variables) n i ();
        D.mono_wf_def (D.add (count variables) n) (D.Parameter i))
    | D.Parameter i -> present_shifted (count variables) n i ()
    | D.List_type a -> abstract_wf n variables a ()
    | D.Function (a, b) -> abstract_wf n variables a (); abstract_wf n variables b ()
    | D.Boolean | D.Word64 -> ())

let (scheme_wf @ total) : (n : D.index) @ immutable -> (variables : variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | D.mono_wf n ty} ->
    {u : unit | D.scheme_wf n (D.Forall (count variables, abstract (count variables) variables ty))} @ ghost =
  fun n variables ty premise -> ghost_ (
    abstract_wf n variables ty (); D.scheme_wf_def n (D.Forall (count variables, abstract (count variables) variables ty)))

let (generalize @ total) :
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable ->
    {r : generalized | D.length (arguments r.variables) === D.arity r.scheme
        && D.open_scheme r.scheme (arguments r.variables) === ty
        && excludes_context context r.variables && unique r.variables
        && covers context ty r.variables && r.variables === collect context ty Empty
        && r.scheme === D.Forall (count r.variables, abstract (count r.variables) r.variables ty)}
    @ immutable = fun context ty ->
  let variables = collect context ty Empty in
  let arity = count variables in
  let scheme = D.Forall (arity, abstract arity variables ty) in
  ghost_ (excludes_context_def context Empty; unique_def Empty;
    collect_excludes context ty Empty (); collect_unique context ty Empty (); collect_covers context ty Empty;
    argument_count variables; restore variables ty;
    D.arity_def scheme; D.open_scheme_def scheme (arguments variables));
  {scheme; variables}
