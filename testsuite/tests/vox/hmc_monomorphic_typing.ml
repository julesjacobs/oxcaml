module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module R = Hmc_reference_tree
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module G = Hmc_ground_type

let[@def] rec (typed @ total) (table : M.table @ immutable) (g : D.context @ immutable)
    (e : C.term @ immutable) (t : D.mono @ immutable) (d : D.typing @ immutable) = ghost_ (
  match d with
  | D.Variable args -> (match e with C.Local i -> (match D.lookup g i with
    | None -> false | Some s -> D.length args === D.arity s
      && t === D.open_scheme s args)
    | C.Global (_, _, id) -> (match M.lookup table id with
      | None -> false | Some entry -> t === G.mono entry.M.body.Hmc_specialized_body.origin.I.ty)
    | _ -> false)
  | D.Constant -> (e === C.Truth || e === C.False) && t === D.Boolean
  | D.Word_constant -> (match e with C.Word _ -> t === D.Word64 | _ -> false)
  | D.Empty_list a -> e === C.Nil && t === D.List_type a
  | D.List_cons (a, head, tail) -> (match e with
    | C.Cons (h, r) -> t === D.List_type a && typed table g h a head && typed table g r t tail
    | _ -> false)
  | D.List_case (a, scrutinee, empty, nonempty) -> (match e with
    | C.CaseList (s, l, r) -> typed table g s (D.List_type a) scrutinee
      && typed table g l t empty
      && typed table (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), g))) r t nonempty
    | _ -> false)
  | D.Conditional (condition, yes, no) -> (match e with
    | C.If (c, a, b) -> typed table g c D.Boolean condition && typed table g a t yes && typed table g b t no
    | _ -> false)
  | D.Word_primitive (left, right) -> (match e with
    | C.Primitive (op, a, b) -> t === D.operation_type op && typed table g a D.Word64 left && typed table g b D.Word64 right
    | _ -> false)
  | D.Abstraction (a, body) -> (match e, t with
    | C.Lambda e, D.Function (x, b) -> a === x
      && typed table (D.Binding (D.Forall (D.Z, a), g)) e b body
    | _ -> false)
  | D.Application (a, left, right) -> (match e with
    | C.Apply (f, x) -> typed table g f (D.Function (a, t)) left && typed table g x a right
    | _ -> false)
  | D.Recursion (a, b, body) -> (match e with
    | C.Recursive e -> t === D.Function (a, b)
      && typed table (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, t), g))) e b body
    | _ -> false)
  | D.Let_binding (s, rhs, body) -> (match e, s with
    | C.Let (r, b), D.Forall (k, a) -> k === D.Z
      && typed table g r a rhs
      && typed table (D.Binding (s, g)) b t body
    | _ -> false))

let rec (local_lookup @ total) : (locals : D.context) @ immutable -> (globals : D.context) @ immutable ->
    (i : D.index) @ immutable -> {u : unit | R.global_index locals i === None} ->
    {u : unit | D.lookup (R.context locals globals) i === D.lookup locals i} @ ghost = fun locals globals i premise -> ghost_ (
  R.global_index_def locals i; R.context_def locals globals;
  D.lookup_def locals i; D.lookup_def (R.context locals globals) i;
  match locals, i with D.Binding (_, rest), D.S i -> local_lookup rest globals i () | _ -> ())

let rec (selected_lookup @ total) : (catalog : T.catalog) @ immutable -> (i : D.index) @ immutable ->
    (selected : T.selected) @ immutable -> {u : unit | T.selection catalog i === Some selected} ->
    {u : unit | D.lookup (T.context catalog) i === Some selected.T.definition.T.scheme} @ ghost =
  fun catalog i selected premise -> ghost_ (
    T.selection_def catalog i; T.context_def catalog; D.lookup_def (T.context catalog) i;
    match catalog, i with T.Declare (_, rest), D.S i -> selected_lookup rest i selected () | _ -> ())

let rec (preservation @ total) : (table : M.table) @ immutable -> (catalog : T.catalog) @ immutable ->
    (locals : D.context) @ immutable -> (code : C.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | M.bounded (M.size table) (C.links code) && D.typed D.Z (R.context locals (T.context catalog)) (C.erase code) ty d
      && R.records catalog locals (C.erase code) d (M.resolve table (C.links code))} ->
    {u : unit | typed table locals code ty d} @ ghost = fun table catalog locals code ty d premise -> ghost_ (
      C.erase_def code; C.links_def code; M.bounded_def (M.size table) (C.links code); M.resolve_def table (C.links code);
      D.typed_def D.Z (R.context locals (T.context catalog)) (C.erase code) ty d;
      R.records_def catalog locals (C.erase code) d (M.resolve table (C.links code));
      typed_def table locals code ty d;
      match code, d with
      | C.Local i, D.Variable _ -> local_lookup locals (T.context catalog) i ()
      | C.Global (i, j, id), D.Variable args ->
        M.lookup_present table id ();
        (match M.lookup table id with None -> () | Some entry ->
          let origin = entry.M.body.Hmc_specialized_body.origin in
          I.valid_def origin;
          R.global_lookup locals (T.context catalog) i j ();
          selected_lookup catalog j {T.definition = origin.I.definition; earlier = origin.I.earlier} ())
      | C.Lambda body, D.Abstraction (a, db) -> (match ty with D.Function (_, b) ->
        let next = D.Binding (D.Forall (D.Z, a), locals) in
        R.context_def next (T.context catalog); preservation table catalog next body b db () | _ -> ())
      | C.Recursive body, D.Recursion (a, b, db) ->
        let self = D.Binding (D.Forall (D.Z, D.Function (a, b)), locals) in
        let next = D.Binding (D.Forall (D.Z, a), self) in
        R.context_def self (T.context catalog); R.context_def next (T.context catalog);
        preservation table catalog next body b db ()
      | C.Apply (a, b), D.Application (arg, da, db) ->
        preservation table catalog locals a (D.Function (arg, ty)) da ();
        preservation table catalog locals b arg db ()
      | C.Cons (a, b), D.List_cons (arg, da, db) ->
        preservation table catalog locals a arg da (); preservation table catalog locals b ty db ()
      | C.Primitive (_, a, b), D.Word_primitive (da, db) ->
        preservation table catalog locals a D.Word64 da (); preservation table catalog locals b D.Word64 db ()
      | C.If (a, b, c), D.Conditional (da, db, dc) ->
        preservation table catalog locals a D.Boolean da (); preservation table catalog locals b ty db ();
        preservation table catalog locals c ty dc ()
      | C.CaseList (s, a, b), D.List_case (element, ds, da, db) ->
        let tail = D.Binding (D.Forall (D.Z, D.List_type element), locals) in
        let next = D.Binding (D.Forall (D.Z, element), tail) in
        R.context_def tail (T.context catalog); R.context_def next (T.context catalog);
        preservation table catalog locals s (D.List_type element) ds ();
        preservation table catalog locals a ty da (); preservation table catalog next b ty db ()
      | C.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
        let next = D.Binding (D.Forall (D.Z, arg), locals) in
        R.context_def next (T.context catalog); D.add_def D.Z D.Z;
        let identity = {Hm_interpreter_substitution.front = D.No_arguments; tail = D.Z} in
        Hmc_parameter_closed.closed_context identity D.Z (R.context locals (T.context catalog)) ();
        preservation table catalog locals a arg da (); preservation table catalog next b ty db ()
      | _ -> ())

let[@def] rec (definitions_typed @ total) (table : M.table @ immutable)
    (definitions : C.definitions @ immutable) = ghost_ (match definitions with
  | C.No_definitions -> true
  | C.Definition (d, rest) ->
    typed table D.Empty_context d.C.code (G.mono d.C.body.Hmc_specialized_body.origin.I.ty)
      d.C.body.Hmc_specialized_body.derivation
    && Hmc_ground_annotations.typing d.C.body.Hmc_specialized_body.derivation
    && definitions_typed table rest)

let rec (all_typed @ total) : (table : M.table) @ immutable -> (definitions : C.definitions) @ immutable ->
    {u : unit | C.origins definitions && M.valid (C.manifest definitions)
      && M.extends table (C.manifest definitions)} ->
    {u : unit | definitions_typed table definitions} @ ghost = fun table definitions premise -> ghost_ (
  C.origins_def definitions; C.manifest_def definitions; M.valid_def (C.manifest definitions);
  definitions_typed_def table definitions;
  match definitions with
  | C.No_definitions -> ()
  | C.Definition (d, rest) ->
    let previous = C.manifest rest in
    M.extends_def previous previous; M.extends_def (C.manifest definitions) previous;
    M.transitive table (C.manifest definitions) previous ();
    M.preserve table previous (C.links d.C.code) ();
    Hmc_specialized_body.valid_def d.C.body;
    R.context_def D.Empty_context (T.context d.C.body.Hmc_specialized_body.origin.I.earlier);
    preservation table d.C.body.Hmc_specialized_body.origin.I.earlier D.Empty_context d.C.code
      (G.mono d.C.body.Hmc_specialized_body.origin.I.ty) d.C.body.Hmc_specialized_body.derivation ();
    all_typed table rest ())

let (program_typed @ total) : (p : C.program) @ immutable ->
    {u : unit | definitions_typed (C.manifest p.C.definitions) p.C.definitions
      && typed (C.manifest p.C.definitions) D.Empty_context p.C.entry
        (D.Function (D.Word64, D.Word64)) p.C.source.T.derivation} @ ghost = fun p -> ghost_ (
  C.ready_def p; T.ready_def p.C.source;
  let table = C.manifest p.C.definitions in M.extends_def table table;
  all_typed table p.C.definitions ();
  R.context_def D.Empty_context (T.context p.C.source.T.globals);
  preservation table p.C.source.T.globals D.Empty_context p.C.entry
    (D.Function (D.Word64, D.Word64)) p.C.source.T.derivation ())
