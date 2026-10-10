module D = Hm_declarative
module C = Hmc_monomorphic
module K = Hmc_closure_ir
module M = Hmc_manifest

let rec (lookup_range @ total) : (table : K.table) @ immutable -> (id : D.index) @ immutable ->
    (entry : K.entry) @ immutable -> {u : unit | K.lookup table id === Some entry} ->
    {u : unit | D.present (K.size table) id} @ ghost = fun table id entry premise -> ghost_ (
  K.lookup_def table id; K.size_def table;
  match table with K.Empty -> () | K.Add (_, rest) ->
    if Hm_elaboration_check.index_equal id (K.size rest) then M.self_present (K.size rest)
    else (lookup_range rest id entry (); M.weaken (K.size rest) id ()))
let rec (lookup @ total) : (larger : K.table) @ immutable -> (smaller : K.table) @ immutable ->
    (id : D.index) @ immutable -> (entry : K.entry) @ immutable ->
    {u : unit | K.extends larger smaller && K.lookup smaller id === Some entry} ->
    {u : unit | K.lookup larger id === Some entry} @ ghost = fun larger smaller id entry premise -> ghost_ (
  K.extends_def larger smaller;
  if larger === smaller then () else match larger with K.Empty -> () | K.Add (_, rest) ->
    lookup rest smaller id entry (); lookup_range rest id entry ();
    M.irreflexive (K.size rest); K.lookup_def larger id;
    let _ = Hm_elaboration_check.index_equal id (K.size rest) in ())
let rec (transitive @ total) : (a : K.table) @ immutable -> (b : K.table) @ immutable ->
    (c : K.table) @ immutable -> {u : unit | K.extends a b && K.extends b c} ->
    {u : unit | K.extends a c} @ ghost = fun a b c premise -> ghost_ (
  K.extends_def a b; K.extends_def a c;
  if a === b then () else match a with K.Empty -> () | K.Add (_, rest) -> transitive rest b c ())

let rec (related @ total) : (larger : K.table) @ immutable -> (smaller : K.table) @ immutable ->
    (source : C.term) @ immutable -> (code : K.term) @ immutable ->
    {u : unit | K.extends larger smaller && K.related smaller source code} ->
    {u : unit | K.related larger source code} @ ghost = fun larger smaller source code premise -> ghost_ (
  K.related_def smaller source code; K.related_def larger source code;
  match source, code with
  | (C.Lambda _ | C.Recursive _), K.Closure id ->
    (match K.lookup smaller id with None -> () | Some entry -> lookup larger smaller id entry ())
  | C.Apply (a, b), K.Apply (x, y) | C.Cons (a, b), K.Cons (x, y)
  | C.Let (a, b), K.Let (x, y) | C.Primitive (_, a, b), K.Primitive (_, x, y) ->
    related larger smaller a x (); related larger smaller b y ()
  | C.If (a, b, c), K.If (x, y, z) | C.CaseList (a, b, c), K.CaseList (x, y, z) ->
    related larger smaller a x (); related larger smaller b y (); related larger smaller c z ()
  | _ -> ())

let rec (typing @ total) : (globals : M.table) @ immutable ->
    (larger : K.table) @ immutable -> (smaller : K.table) @ immutable ->
    (g : D.context) @ immutable -> (code : K.term) @ immutable -> (ty : D.mono) @ immutable ->
    (d : D.typing) @ immutable -> {u : unit | K.extends larger smaller && K.typed globals smaller g code ty d} ->
    {u : unit | K.typed globals larger g code ty d} @ ghost = fun globals larger smaller g code ty d premise -> ghost_ (
  K.typed_def globals smaller g code ty d; K.typed_def globals larger g code ty d;
  match code, d with
  | K.Closure id, (D.Abstraction _ | D.Recursion _) ->
    (match K.lookup smaller id with None -> () | Some entry -> lookup larger smaller id entry ())
  | K.Apply (a, b), D.Application (arg, da, db) ->
    typing globals larger smaller g a (D.Function (arg, ty)) da (); typing globals larger smaller g b arg db ()
  | K.Cons (a, b), D.List_cons (arg, da, db) ->
    typing globals larger smaller g a arg da (); typing globals larger smaller g b ty db ()
  | K.Primitive (_, a, b), D.Word_primitive (da, db) ->
    typing globals larger smaller g a D.Word64 da (); typing globals larger smaller g b D.Word64 db ()
  | K.If (a, b, c), D.Conditional (da, db, dc) ->
    typing globals larger smaller g a D.Boolean da (); typing globals larger smaller g b ty db ();
    typing globals larger smaller g c ty dc ()
  | K.CaseList (s, a, b), D.List_case (element, ds, da, db) ->
    typing globals larger smaller g s (D.List_type element) ds (); typing globals larger smaller g a ty da ();
    typing globals larger smaller (D.Binding (D.Forall (D.Z, element),
      D.Binding (D.Forall (D.Z, D.List_type element), g))) b ty db ()
  | K.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
    typing globals larger smaller g a arg da ();
    typing globals larger smaller (D.Binding (D.Forall (D.Z, arg), g)) b ty db ()
  | _ -> ())

let rec (lookup_valid @ total) : (globals : M.table) @ immutable -> (table : K.table) @ immutable ->
    (id : D.index) @ immutable -> (entry : K.entry) @ immutable ->
    {u : unit | K.valid globals table && K.lookup table id === Some entry} ->
    {u : unit | K.typed globals table (K.context entry) entry.K.body entry.K.result entry.K.derivation
      && K.related table entry.K.source entry.K.body && K.ground_context entry.K.captured
      && Hmc_ground_type.ground entry.K.argument && Hmc_ground_type.ground entry.K.result} @ ghost =
  fun globals table id entry premise -> ghost_ (
    K.valid_def globals table; K.lookup_def table id;
    match table with K.Empty -> () | K.Add (_, rest) ->
      if Hm_elaboration_check.index_equal id (K.size rest) then (
        K.extends_def rest rest; K.extends_def table rest;
        related table rest entry.K.source entry.K.body ();
        typing globals table rest (K.context entry) entry.K.body entry.K.result entry.K.derivation ()) else (
        lookup_valid globals rest id entry ();
        K.extends_def rest rest; K.extends_def table rest;
        related table rest entry.K.source entry.K.body ();
        typing globals table rest (K.context entry) entry.K.body entry.K.result entry.K.derivation ()))
