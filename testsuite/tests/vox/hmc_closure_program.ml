module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module H = Hmc_monomorphic_typing
module K = Hmc_closure_ir
module E = Hmc_closure_extension
module L = Hmc_closure_lower
module G = Hmc_ground_type
module A = Hmc_ground_annotations
module B = Hmc_specialized_body
module I = Hmc_instance
module T = Hmc_templates

type globals = No_globals | Global of D.index * globals [@@inductive]
let[@def] rec (size @ total) (globals : globals @ immutable) = match globals with
  | No_globals -> D.Z | Global (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (globals : globals @ immutable) (id : D.index @ immutable) = match globals with
  | No_globals -> None | Global (code, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then Some code else lookup rest id
let[@def] rec (mapped @ total) (interface : M.table @ immutable) (table : K.table @ immutable)
    (source : C.definitions @ immutable) (globals : globals @ immutable) = ghost_ (match source, globals with
  | C.No_definitions, No_globals -> true
  | C.Definition (d, rest), Global (id, tail) ->
    K.related table d.C.code (K.Closure id)
    && K.typed interface table D.Empty_context (K.Closure id) (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation
    && mapped interface table rest tail
  | _ -> false)
let rec (preserve @ total) : (interface : M.table) @ immutable -> (larger : K.table) @ immutable ->
    (smaller : K.table) @ immutable -> (source : C.definitions) @ immutable -> (globals : globals) @ immutable ->
    {u : unit | K.extends larger smaller && mapped interface smaller source globals} ->
    {u : unit | mapped interface larger source globals} @ ghost = fun interface larger smaller source globals premise -> ghost_ (
  mapped_def interface smaller source globals; mapped_def interface larger source globals;
  match source, globals with C.Definition (d, rest), Global (id, tail) ->
    E.related larger smaller d.C.code (K.Closure id) ();
    E.typing interface larger smaller D.Empty_context (K.Closure id) (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation ();
    preserve interface larger smaller rest tail () | _ -> ())

type result = {table : K.table; globals : globals}
let rec (lower @ total) : (interface : M.table) @ immutable -> (initial : K.table) @ immutable ->
    (source : C.definitions) @ immutable ->
    {u : unit | K.valid interface initial && C.origins source && H.definitions_typed interface source} ->
    {r : result | K.valid interface r.table && K.extends r.table initial && mapped interface r.table source r.globals} @ immutable =
  fun interface initial source premise ->
    ghost_ (C.origins_def source; H.definitions_typed_def interface source);
    match source with
    | C.No_definitions ->
      ghost_ (K.extends_def initial initial; mapped_def interface initial source No_globals);
      {table = initial; globals = No_globals}
    | C.Definition (d, rest) ->
      let previous = lower interface initial rest () in
      ghost_ (K.ground_context_def D.Empty_context; G.is_ground d.C.body.B.origin.I.ty);
      let entry = L.lower interface previous.table D.Empty_context d.C.code
        (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation () in
      ghost_ (E.transitive entry.L.table previous.table initial ();
        preserve interface entry.L.table previous.table rest previous.globals ();
        I.valid_def d.C.body.B.origin; T.definition_valid_def d.C.body.B.origin.I.earlier d.C.body.B.origin.I.definition;
        Hmc_admission.callable_def d.C.body.B.origin.I.definition.T.source; C.erase_def d.C.code;
        K.related_def entry.L.table d.C.code entry.L.code);
      match entry.L.code with
      | K.Closure id ->
        let globals = Global (id, previous.globals) in
        ghost_ (mapped_def interface entry.L.table source globals);
        {table = entry.L.table; globals}
      | _ -> unreachable_ ()

type payload = {origin : C.program; table : K.table; globals : globals; entry : D.index}
let[@def] (valid @ total) (p : payload @ immutable) = ghost_ (
  K.valid (C.manifest p.origin.C.definitions) p.table
  && mapped (C.manifest p.origin.C.definitions) p.table p.origin.C.definitions p.globals
  && K.related p.table p.origin.C.entry (K.Closure p.entry)
  && K.typed (C.manifest p.origin.C.definitions) p.table D.Empty_context (K.Closure p.entry)
    (D.Function (D.Word64, D.Word64)) p.origin.C.source.T.derivation)
type program = {p : payload | valid p}
let (build @ total) : (origin : C.program) @ immutable -> {p : program | p.origin === origin} @ immutable = fun origin ->
  let interface = C.manifest origin.C.definitions in
  ghost_ (C.ready_def origin; T.ready_def origin.C.source; H.program_typed origin; K.valid_def interface K.Empty);
  let definitions = lower interface K.Empty origin.C.definitions () in
  ghost_ (K.ground_context_def D.Empty_context;
    G.ground_def (D.Function (D.Word64, D.Word64)); G.ground_def D.Word64;
    A.typing_ground (T.context origin.C.source.T.globals) origin.C.source.T.entry
      (D.Function (D.Word64, D.Word64)) origin.C.source.T.derivation ());
  let entry = L.lower interface definitions.table D.Empty_context origin.C.entry
    (D.Function (D.Word64, D.Word64)) origin.C.source.T.derivation () in
  ghost_ (preserve interface entry.L.table definitions.table origin.C.definitions definitions.globals ();
    Hmc_admission.callable_def origin.C.source.T.entry; C.erase_def origin.C.entry;
    K.related_def entry.L.table origin.C.entry entry.L.code);
  match entry.L.code with
  | K.Closure id ->
    let out = {origin; table = entry.L.table; globals = definitions.globals; entry = id} in
    ghost_ (valid_def out);
    let out : program = refine_ out in out
  | _ -> unreachable_ ()

let rec (same_size @ total) : (interface : M.table) @ immutable -> (table : K.table) @ immutable ->
    (source : C.definitions) @ immutable -> (globals : globals) @ immutable ->
    {u : unit | mapped interface table source globals} ->
    {u : unit | M.size (C.manifest source) === size globals} @ ghost = fun interface table source globals premise -> ghost_ (
  mapped_def interface table source globals; C.manifest_def source; M.size_def (C.manifest source); size_def globals;
  match source, globals with C.Definition (_, rest), Global (_, tail) -> same_size interface table rest tail () | _ -> ())
let rec (lookup_origin @ total) : (interface : M.table) @ immutable -> (table : K.table) @ immutable ->
    (source : C.definitions) @ immutable -> (globals : globals) @ immutable ->
    (id : D.index) @ immutable -> (d : C.definition) @ immutable ->
    {u : unit | mapped interface table source globals && C.selection source id === Some d} ->
    {r : D.index | lookup globals id === Some r && K.related table d.C.code (K.Closure r)
      && K.typed interface table D.Empty_context (K.Closure r) (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation} @ immutable =
  fun interface table source globals id d premise ->
    ghost_ (mapped_def interface table source globals; C.selection_def source id; lookup_def globals id);
    match source, globals with
    | C.Definition (_, rest), Global (code, tail) ->
      ghost_ (same_size interface table rest tail ());
      if Hm_elaboration_check.index_equal id (size tail) then code else lookup_origin interface table rest tail id d ()
    | _ -> unreachable_ ()

let rec (lookup_absent @ total) : (interface : M.table) @ immutable -> (table : K.table) @ immutable ->
    (source : C.definitions) @ immutable -> (globals : globals) @ immutable -> (id : D.index) @ immutable ->
    {u : unit | mapped interface table source globals && C.selection source id === None} ->
    {u : unit | lookup globals id === None} @ ghost = fun interface table source globals id premise -> ghost_ (
  mapped_def interface table source globals; C.selection_def source id; lookup_def globals id;
  match source, globals with C.Definition (_, rest), Global (_, tail) ->
    same_size interface table rest tail ();
    if Hm_elaboration_check.index_equal id (size tail) then () else lookup_absent interface table rest tail id ()
  | _ -> ())
