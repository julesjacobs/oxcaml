module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module R = Hmc_reference_tree
module T = Hmc_templates
module I = Hmc_instance

let[@def] rec (strip @ total) (depth : D.index @ immutable) (index : D.index @ immutable) =
  match depth with D.Z -> Some index | D.S n -> match index with D.Z -> None | D.S i -> strip n i
let[@def] rec (depth @ total) (locals : D.context @ immutable) = match locals with
  | D.Empty_context -> D.Z | D.Binding (_, rest) -> D.S (depth rest)
let rec (strip_context @ total) : (locals : D.context) @ immutable -> (index : D.index) @ immutable ->
    {u : unit | strip (depth locals) index === R.global_index locals index} @ ghost = fun locals index -> ghost_ (
  depth_def locals; strip_def (depth locals) index; R.global_index_def locals index;
  match locals, index with D.Binding (_, rest), D.S i -> strip_context rest i | _ -> ())

let[@def] rec (linked @ total) (table : M.table @ immutable) (catalog : T.catalog @ immutable)
    (depth : D.index @ immutable) (code : C.term @ immutable) = ghost_ (match code with
  | C.Local i -> strip depth i === None
  | C.Global (i, j, id) -> strip depth i === Some j && (match M.lookup table id with
    | None -> false | Some entry -> T.selection catalog j === Some
      {T.definition = entry.M.body.Hmc_specialized_body.origin.I.definition;
       earlier = entry.M.body.Hmc_specialized_body.origin.I.earlier})
  | C.Truth | C.False | C.Word _ | C.Nil -> true
  | C.Lambda a -> linked table catalog (D.S depth) a
  | C.Recursive a -> linked table catalog (D.S (D.S depth)) a
  | C.Apply (a, b) | C.Cons (a, b) | C.Primitive (_, a, b) ->
    linked table catalog depth a && linked table catalog depth b
  | C.Let (a, b) -> linked table catalog depth a && linked table catalog (D.S depth) b
  | C.If (a, b, c) -> linked table catalog depth a && linked table catalog depth b && linked table catalog depth c
  | C.CaseList (s, a, b) -> linked table catalog depth s && linked table catalog depth a
    && linked table catalog (D.S (D.S depth)) b)

let rec (from_records @ total) : (table : M.table) @ immutable -> (catalog : T.catalog) @ immutable ->
    (locals : D.context) @ immutable -> (code : C.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | M.bounded (M.size table) (C.links code)
      && R.records catalog locals (C.erase code) d (M.resolve table (C.links code))} ->
    {u : unit | linked table catalog (depth locals) code} @ ghost = fun table catalog locals code d premise -> ghost_ (
  C.erase_def code; C.links_def code; M.resolve_def table (C.links code);
  M.bounded_def (M.size table) (C.links code);
  R.records_def catalog locals (C.erase code) d (M.resolve table (C.links code));
  linked_def table catalog (depth locals) code;
  match code, d with
  | C.Local i, D.Variable _ -> strip_context locals i
  | C.Global (i, _, id), D.Variable _ -> strip_context locals i; M.lookup_present table id ()
  | C.Lambda a, D.Abstraction (arg, da) ->
    let next = D.Binding (D.Forall (D.Z, arg), locals) in depth_def next;
    from_records table catalog next a da ()
  | C.Recursive a, D.Recursion (arg, ret, da) ->
    let self = D.Binding (D.Forall (D.Z, D.Function (arg, ret)), locals) in
    let next = D.Binding (D.Forall (D.Z, arg), self) in depth_def self; depth_def next;
    from_records table catalog next a da ()
  | C.Apply (a, b), D.Application (_, da, db)
  | C.Cons (a, b), D.List_cons (_, da, db)
  | C.Primitive (_, a, b), D.Word_primitive (da, db) ->
    from_records table catalog locals a da (); from_records table catalog locals b db ()
  | C.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
    let next = D.Binding (D.Forall (D.Z, arg), locals) in depth_def next;
    from_records table catalog locals a da (); from_records table catalog next b db ()
  | C.If (a, b, c), D.Conditional (da, db, dc) ->
    from_records table catalog locals a da (); from_records table catalog locals b db ();
    from_records table catalog locals c dc ()
  | C.CaseList (s, a, b), D.List_case (arg, ds, da, db) ->
    let tail = D.Binding (D.Forall (D.Z, D.List_type arg), locals) in
    let next = D.Binding (D.Forall (D.Z, arg), tail) in depth_def tail; depth_def next;
    from_records table catalog locals s ds (); from_records table catalog locals a da ();
    from_records table catalog next b db ()
  | _ -> (match code with C.Global (_, _, id) -> M.lookup_present table id () | _ -> ()))

let (fetch @ total) : (p : C.program) @ immutable ->
    (id : {i : D.index | D.present (M.size (C.manifest p.C.definitions)) i}) @ immutable ->
    {d : C.definition | Hmc_monomorphic_globals.callable d.C.code
      && M.lookup (C.manifest p.C.definitions) id === Some {M.body = d.C.body; dependencies = C.links d.C.code}
      && C.erase d.C.code === d.C.body.Hmc_specialized_body.origin.I.definition.T.source
      && linked (C.manifest p.C.definitions) d.C.body.Hmc_specialized_body.origin.I.earlier D.Z d.C.code} @ immutable =
  fun p id ->
    ghost_ (C.ready_def p);
    let defs : {d : C.definitions | C.origins d} = refine_ p.C.definitions in
    let table = C.manifest defs in
    ghost_ (M.lookup_present table id ());
    match C.lookup defs id with None -> unreachable_ () | Some d ->
      ghost_ (
        let entry = {M.body = d.C.body; dependencies = C.links d.C.code} in
        M.lookup_closed table id entry ();
        from_records table d.C.body.Hmc_specialized_body.origin.I.earlier D.Empty_context d.C.code
          d.C.body.Hmc_specialized_body.derivation ();
        depth_def D.Empty_context;
        I.valid_def d.C.body.Hmc_specialized_body.origin;
        T.definition_valid_def d.C.body.Hmc_specialized_body.origin.I.earlier
          d.C.body.Hmc_specialized_body.origin.I.definition;
        Hmc_admission.callable_def d.C.body.Hmc_specialized_body.origin.I.definition.T.source;
        C.erase_def d.C.code; Hmc_monomorphic_globals.callable_def d.C.code);
      d
