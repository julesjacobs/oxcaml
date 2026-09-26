module D = Hm_declarative
module M = Hmc_manifest
module R = Hmc_reference_tree
module T = Hmc_templates

type term = Local of D.index | Global of D.index * D.index * D.index
  | Truth | False | Word of Hmc_word64.t | Nil
  | Lambda of term | Recursive of term | Apply of term * term
  | Cons of term * term | Primitive of D.word_operation * term * term
  | If of term * term * term | CaseList of term * term * term
  | Let of term * term [@@inductive]

let[@def] rec (erase @ total) (term : term @ immutable) = match term with
  | Local i | Global (i, _, _) -> D.Bound i
  | Truth -> D.Truth | False -> D.False | Word w -> D.Word w | Nil -> D.Nil
  | Lambda (a) -> D.Lambda (erase a)
  | Recursive (a) -> D.Recursive (erase a)
  | Apply (a, b) -> D.Apply (erase a, erase b)
  | Cons (a, b) -> D.Cons (erase a, erase b)
  | Let (a, b) -> D.Let (erase a, erase b)
  | If (a, b, c) -> D.If (erase a, erase b, erase c)
  | CaseList (a, b, c) -> D.CaseList (erase a, erase b, erase c)
  | Primitive (op, a, b) -> D.Primitive (op, erase a, erase b)

let[@def] rec (links @ total) (term : term @ immutable) = match term with
  | Local _ | Truth | False | Word _ | Nil -> M.Empty
  | Global (_, index, id) -> M.Reference (index, id)
  | Lambda a | Recursive a -> M.Child (links a)
  | Apply (a, b) | Cons (a, b) | Let (a, b) | Primitive (_, a, b) -> M.Pair (links a, links b)
  | If (a, b, c) | CaseList (a, b, c) -> M.Triple (links a, links b, links c)

let rec (rewrite @ total) : (table : M.table) @ immutable -> (catalog : T.catalog) @ immutable ->
    (locals : D.context) @ immutable -> (source : D.term) @ immutable ->
    (d : D.typing) @ immutable -> (refs : M.references) @ immutable ->
    {u : unit | M.bounded (M.size table) refs && R.records catalog locals source d (M.resolve table refs)} ->
    {r : term | erase r === source && links r === refs} @ immutable =
  fun table catalog locals source d refs premise ->
    ghost_ (M.bounded_def (M.size table) refs; M.resolve_def table refs; R.records_def catalog locals source d (M.resolve table refs));
    ghost_ (match refs with M.Reference (_, id) -> M.lookup_present table id () | _ -> ());
    let out = match source, d, refs with
    | D.Bound i, D.Variable _, M.Empty -> Local i
    | D.Bound i, D.Variable _, M.Reference (index, id) ->
      ghost_ (M.lookup_present table id ());
      (match M.lookup table id with None -> unreachable_ () | Some _ -> Global (i, index, id))
    | D.Truth, D.Constant, M.Empty -> Truth
    | D.False, D.Constant, M.Empty -> False
    | D.Word w, D.Word_constant, M.Empty -> Word w
    | D.Nil, D.Empty_list _, M.Empty -> Nil
    | D.Lambda body, D.Abstraction (a, db), M.Child child ->
      Lambda (rewrite table catalog (D.Binding (D.Forall (D.Z, a), locals)) body db child ())
    | D.Recursive body, D.Recursion (a, b, db), M.Child child ->
      Recursive (rewrite table catalog (D.Binding (D.Forall (D.Z, a),
        D.Binding (D.Forall (D.Z, D.Function (a, b)), locals))) body db child ())
    | D.Apply (a, b), D.Application (_, da, db), M.Pair (ra, rb) ->
      Apply (rewrite table catalog locals a da ra (), rewrite table catalog locals b db rb ())
    | D.Cons (a, b), D.List_cons (_, da, db), M.Pair (ra, rb) ->
      Cons (rewrite table catalog locals a da ra (), rewrite table catalog locals b db rb ())
    | D.Primitive (op, a, b), D.Word_primitive (da, db), M.Pair (ra, rb) ->
      Primitive (op, rewrite table catalog locals a da ra (), rewrite table catalog locals b db rb ())
    | D.If (a, b, c), D.Conditional (da, db, dc), M.Triple (ra, rb, rc) ->
      If (rewrite table catalog locals a da ra (), rewrite table catalog locals b db rb (),
        rewrite table catalog locals c dc rc ())
    | D.CaseList (s, a, b), D.List_case (element, ds, da, db), M.Triple (rs, ra, rb) ->
      CaseList (rewrite table catalog locals s ds rs (), rewrite table catalog locals a da ra (),
        rewrite table catalog (D.Binding (D.Forall (D.Z, element),
          D.Binding (D.Forall (D.Z, D.List_type element), locals))) b db rb ())
    | D.Let (a, b), D.Let_binding (D.Forall (D.Z, ty), da, db), M.Pair (ra, rb) ->
      Let (rewrite table catalog locals a da ra (),
        rewrite table catalog (D.Binding (D.Forall (D.Z, ty), locals)) b db rb ())
    | _ -> unreachable_ () in
    ghost_ (erase_def out; links_def out);
    out

type definition = {body : Hmc_specialized_body.t; code : term}
type definitions = No_definitions | Definition of definition * definitions [@@inductive]
let[@def] rec (manifest @ total) (definitions : definitions @ immutable) = match definitions with
  | No_definitions -> M.Nil
  | Definition (d, rest) -> M.Add ({M.body = d.body; dependencies = links d.code}, manifest rest)
let[@def] rec (origins @ total) (definitions : definitions @ immutable) = ghost_ (match definitions with
  | No_definitions -> true
  | Definition (d, rest) -> erase d.code === d.body.Hmc_specialized_body.origin.Hmc_instance.definition.T.source
    && origins rest)

let rec (lower @ total) : (table : M.table) @ immutable -> {u : unit | M.valid table} ->
    {r : definitions | manifest r === table && origins r} @ immutable = fun table premise ->
  ghost_ (M.valid_def table);
  match table with
  | M.Nil -> ghost_ (manifest_def No_definitions; origins_def No_definitions); No_definitions
  | M.Add (entry, rest) ->
    let previous = lower rest () in
    let body = entry.M.body in
    let origin = body.Hmc_specialized_body.origin in
    let code = rewrite rest origin.Hmc_instance.earlier D.Empty_context
      origin.Hmc_instance.definition.T.source body.Hmc_specialized_body.derivation entry.M.dependencies () in
    let out = Definition ({body; code}, previous) in
    ghost_ (manifest_def out; origins_def out);
    out

type payload = {source : T.program; definitions : definitions; entry : term}
let[@def] (ready @ total) (p : payload @ immutable) = ghost_ (
  T.ready p.source && origins p.definitions && M.valid (manifest p.definitions)
  && M.bounded (M.size (manifest p.definitions)) (links p.entry)
  && erase p.entry === p.source.T.entry
  && R.records p.source.T.globals D.Empty_context p.source.T.entry p.source.T.derivation
    (M.resolve (manifest p.definitions) (links p.entry)))
type program = {p : payload | ready p}
let (build @ total) : (p : M.program) @ immutable ->
    {r : program | r.source === p.M.program && manifest r.definitions === p.M.definitions} @ immutable = fun p ->
  ghost_ (M.ready_def p);
  let definitions = lower p.M.definitions () in
  let entry = rewrite p.M.definitions p.M.program.T.globals D.Empty_context
    p.M.program.T.entry p.M.program.T.derivation p.M.entry () in
  let out = {source = p.M.program; definitions; entry} in
  ghost_ (ready_def out);
  let out : program = refine_ out in out

let[@def] rec (selection @ total) (definitions : definitions @ immutable) (id : D.index @ immutable) =
  match definitions with No_definitions -> None | Definition (d, rest) ->
    if Hm_elaboration_check.index_equal id (M.size (manifest rest)) then Some d else selection rest id

let rec (lookup @ total) : (definitions : {d : definitions | origins d}) @ immutable -> (id : D.index) @ immutable ->
    {r : definition option | r === selection definitions id && (match r with
      | None -> M.lookup (manifest definitions) id === None
      | Some d -> M.lookup (manifest definitions) id === Some {M.body = d.body; dependencies = links d.code}
        && erase d.code === d.body.Hmc_specialized_body.origin.Hmc_instance.definition.T.source)} @ immutable =
  fun definitions id ->
    ghost_ (manifest_def definitions; origins_def definitions; selection_def definitions id);
    match definitions with
    | No_definitions -> ghost_ (M.lookup_def M.Nil id); None
    | Definition (d, rest) ->
      ghost_ (M.lookup_def (manifest definitions) id);
      if Hm_elaboration_check.index_equal id (M.size (manifest rest)) then Some d else lookup (refine_ rest) id
