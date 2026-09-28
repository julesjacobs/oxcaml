module D = Hm_declarative
module G = Hm_generalization
module Ty = Copy_spec

let[@def] rec (arguments @ total) (variables : G.variables @ immutable)
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total) @ total) = ghost_ (
  match variables with
  | G.Empty -> D.No_arguments
  | G.Variable (p, rest) -> D.Argument (replacement p, arguments rest replacement))

let[@def] rec (substitute @ total) (variables : G.variables @ immutable)
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total) @ total)
    (ty : D.mono @ immutable) = ghost_ (
  match ty with
  | D.Free p -> (match G.find p variables with None -> ty | Some _ -> replacement p)
  | D.Parameter _ | D.Boolean | D.Word64 -> ty
  | D.List_type element -> D.List_type (substitute variables replacement element)
  | D.Function (argument, result) ->
    D.Function (substitute variables replacement argument, substitute variables replacement result))

let rec (count @ total) : (variables : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    {u : unit | D.length (arguments variables replacement) === G.count variables} @ ghost =
  fun variables replacement -> ghost_ (
    arguments_def variables replacement; G.count_def variables; D.length_def (arguments variables replacement);
    match variables with G.Empty -> () | G.Variable (_, rest) -> count rest replacement)

let rec (found @ total) : (variables : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | G.find p variables === Some index} ->
    {u : unit | D.open_index (arguments variables replacement) index === replacement p} @ ghost =
  fun variables replacement p index premise -> ghost_ (
    G.find_def p variables; arguments_def variables replacement;
    D.open_index_def (arguments variables replacement) index;
    match variables with
    | G.Empty -> ()
    | G.Variable (q, rest) ->
      let same = Pref.equal p q in
      if same then () else match G.find p rest with
      | None -> () | Some inner -> found rest replacement p inner ())

let rec (shifted @ total) : (variables : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (index : D.index) @ immutable ->
    {u : unit | D.open_index (arguments variables replacement) (D.add (G.count variables) index) === D.Parameter index} @ ghost =
  fun variables replacement index -> ghost_ (
    G.count_def variables; arguments_def variables replacement; D.add_def (G.count variables) index;
    D.open_index_def (arguments variables replacement) (D.add (G.count variables) index);
    match variables with G.Empty -> () | G.Variable (_, rest) -> shifted rest replacement index)

let rec (open_abstract @ total) : (variables : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (ty : D.mono) @ immutable ->
    {u : unit | D.open_type (arguments variables replacement) (G.abstract (G.count variables) variables ty)
      === substitute variables replacement ty} @ ghost =
  fun variables replacement ty -> ghost_ (
    G.abstract_def (G.count variables) variables ty; substitute_def variables replacement ty;
    D.open_type_def (arguments variables replacement) (G.abstract (G.count variables) variables ty);
    match ty with
    | D.Free p -> (match G.find p variables with None -> () | Some index -> found variables replacement p index ())
    | D.Parameter index -> shifted variables replacement index
    | D.List_type element -> open_abstract variables replacement element
    | D.Function (argument, result) -> open_abstract variables replacement argument; open_abstract variables replacement result
    | D.Boolean | D.Word64 -> ())

let (complete @ total) : (variables : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (ty : D.mono) @ immutable ->
    {u : unit | match Hm_instantiation.infer (D.Forall (G.count variables, G.abstract (G.count variables) variables ty))
        (substitute variables replacement ty) with None -> false | Some _ -> true} @ ghost =
  fun variables replacement ty -> ghost_ (
    count variables replacement; open_abstract variables replacement ty;
    let scheme = D.Forall (G.count variables, G.abstract (G.count variables) variables ty) in
    D.arity_def scheme; D.open_scheme_def scheme (arguments variables replacement);
    Hm_instantiation.complete scheme (substitute variables replacement ty) (arguments variables replacement) ())

let rec (substitute_subset @ total) : (selected : G.variables) @ immutable ->
    (larger : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (expanded : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | expanded p === (match G.find p selected with None -> D.Free p | Some _ -> replacement p)})) @ total ->
    (included : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | G.find p selected === None || not (G.find p larger === None)})) @ total ->
    (ty : D.mono) @ immutable ->
    {u : unit | substitute larger expanded ty === substitute selected replacement ty} @ ghost =
  fun selected larger replacement expanded agrees included ty -> ghost_ (
    substitute_def selected replacement ty; substitute_def larger expanded ty;
    match ty with
    | D.Free p -> included p; agrees p
    | D.List_type element -> substitute_subset selected larger replacement expanded agrees included element
    | D.Function (argument, result) ->
      substitute_subset selected larger replacement expanded agrees included argument;
      substitute_subset selected larger replacement expanded agrees included result
    | D.Parameter _ | D.Boolean | D.Word64 -> ())

let (widen_complete @ total) : (selected : G.variables) @ immutable ->
    (larger : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (included : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | G.find p selected === None || not (G.find p larger === None)})) @ total ->
    (ty : D.mono) @ immutable ->
    {u : unit | match Hm_instantiation.infer (D.Forall (G.count larger, G.abstract (G.count larger) larger ty))
        (substitute selected replacement ty) with None -> false | Some _ -> true} @ ghost =
  fun selected larger replacement included ty -> ghost_ (
    let[@def] expanded : Ty.node Pref.t @ immutable total -> D.mono @ immutable total = fun p ->
      match G.find p selected with None -> D.Free p | Some _ -> replacement p in
    let agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | expanded p === (match G.find p selected with None -> D.Free p | Some _ -> replacement p)}) @ total =
      fun p -> expanded_def p in
    substitute_subset selected larger replacement expanded agrees included ty;
    complete larger expanded ty)

module A = Hm_abstraction

let[@def] rec (variables @ total) (names : A.names @ immutable) = ghost_ (
  match names with A.No_names -> G.Empty | A.Name (p, rest) -> G.Variable (p, variables rest))

let rec (variables_count @ total) : (names : A.names) @ immutable ->
    {u : unit | G.count (variables names) === A.count names} @ ghost =
  fun names -> ghost_ (
    variables_def names; G.count_def (variables names); A.count_def names;
    match names with A.No_names -> () | A.Name (_, rest) -> variables_count rest)

let rec (variables_find @ total) : (names : A.names) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | G.find p (variables names) === A.position names p} @ ghost =
  fun names p -> ghost_ (
    variables_def names; G.find_def p (variables names); A.position_def names p;
    match names with A.No_names -> () | A.Name (q, rest) ->
      let same = Pref.equal p q in if same then () else variables_find rest p)

let rec (abstract_agrees @ total) : (names : A.names) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | G.abstract (G.count (variables names)) (variables names) ty === A.abstract_type names D.Z ty} @ ghost =
  fun names ty -> ghost_ (
    variables_count names;
    G.abstract_def (G.count (variables names)) (variables names) ty;
    A.abstract_type_def names D.Z ty;
    match ty with
    | D.Free p -> variables_find names p; A.abstract_free_def names D.Z p;
      (match A.position names p with None -> () | Some i -> D.add_def D.Z i)
    | D.Parameter index -> D.shift_index_def D.Z (A.count names) index
    | D.List_type element -> abstract_agrees names element
    | D.Function (argument, result) -> abstract_agrees names argument; abstract_agrees names result
    | D.Boolean | D.Word64 -> ())

let rec (open_after_arguments @ total) : (args : D.arguments) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | D.open_index args (D.add (D.length args) index) === D.Parameter index} @ ghost =
  fun args index -> ghost_ (
    D.length_def args; D.add_def (D.length args) index;
    D.open_index_def args (D.add (D.length args) index);
    match args with D.No_arguments -> () | D.Argument (_, rest) -> open_after_arguments rest index)

let rec (open_given @ total) : (selected : G.variables) @ immutable -> (args : D.arguments) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | replacement p === (match G.find p selected with
        None -> D.Free p | Some index -> D.open_index args index)})) @ total ->
    (ty : D.mono) @ immutable -> {u : unit | D.length args === G.count selected} ->
    {u : unit | D.open_type args (G.abstract (G.count selected) selected ty) === substitute selected replacement ty} @ ghost =
  fun selected args replacement agrees ty premise -> ghost_ (
    G.abstract_def (G.count selected) selected ty;
    D.open_type_def args (G.abstract (G.count selected) selected ty);
    substitute_def selected replacement ty;
    match ty with
    | D.Free p -> agrees p;
      (match G.find p selected with None -> D.open_type_def args ty
      | Some index -> D.open_type_def args (D.Parameter index))
    | D.Parameter index -> open_after_arguments args index
    | D.List_type element -> open_given selected args replacement agrees element ()
    | D.Function (argument, result) ->
      open_given selected args replacement agrees argument ();
      open_given selected args replacement agrees result ()
    | D.Boolean | D.Word64 -> ())

let (widen_instance @ total) : (selected : G.variables) @ immutable -> (larger : G.variables) @ immutable ->
    (included : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | G.find p selected === None || not (G.find p larger === None)})) @ total ->
    (ty : D.mono) @ immutable -> (args : D.arguments) @ immutable ->
    {u : unit | D.length args === G.count selected} ->
    {u : unit | match Hm_instantiation.infer (D.Forall (G.count larger, G.abstract (G.count larger) larger ty))
        (D.open_type args (G.abstract (G.count selected) selected ty)) with None -> false | Some _ -> true} @ ghost =
  fun selected larger included ty args premise -> ghost_ (
    let[@def] replacement : Ty.node Pref.t @ immutable total -> D.mono @ immutable total = fun p ->
      match G.find p selected with None -> D.Free p | Some index -> D.open_index args index in
    let agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | replacement p === (match G.find p selected with
        None -> D.Free p | Some index -> D.open_index args index)}) @ total = fun p -> replacement_def p in
    open_given selected args replacement agrees ty ();
    widen_complete selected larger replacement included ty)

let rec (abstract_occurs @ total) : (p : Ty.node Pref.t) @ immutable ->
    (arity : D.index) @ immutable -> (selected : G.variables) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | G.occurs p (G.abstract arity selected ty) =
      (G.occurs p ty && G.find p selected === None)} @ ghost =
  fun p arity selected ty -> ghost_ (
    G.abstract_def arity selected ty; G.occurs_def p ty;
    G.occurs_def p (G.abstract arity selected ty);
    match ty with
    | D.Free q -> let same = Pref.equal p q in
      (match G.find q selected with None -> G.occurs_def p (D.Free q)
      | Some index -> G.occurs_def p (D.Parameter index));
      if same then () else ()
    | D.Parameter index -> G.occurs_def p (D.Parameter (D.add arity index))
    | D.List_type element -> abstract_occurs p arity selected element
    | D.Function (argument, result) ->
      abstract_occurs p arity selected argument; abstract_occurs p arity selected result
    | D.Boolean | D.Word64 -> ())

let (generalized_free @ total) : (p : Ty.node Pref.t) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | match (G.generalize context ty).G.scheme with D.Forall (_, body) ->
      not (G.occurs p body) || G.in_context p context} @ ghost =
  fun p context ty -> ghost_ (
    let generalized = G.generalize context ty in
    abstract_occurs p (G.count generalized.G.variables) generalized.G.variables ty;
    G.collect_member p context ty G.Empty; G.find_def p G.Empty)

let (binding_free @ total) : (p : Ty.node Pref.t) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | G.in_context p (D.Binding ((G.generalize context ty).G.scheme, context)) =
      G.in_context p context} @ ghost =
  fun p context ty -> ghost_ (
    generalized_free p context ty;
    G.in_context_def p (D.Binding ((G.generalize context ty).G.scheme, context));
    match (G.generalize context ty).G.scheme with D.Forall (_, _) -> ())
