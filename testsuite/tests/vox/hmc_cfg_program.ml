module D = Hm_declarative
module C = Hmc_monomorphic
module K = Hmc_closure_ir
module X = Hmc_closure_extension
module P = Hmc_closure_program
module M = Hmc_manifest
module G = Hmc_cfg_ir
module E = Hmc_cfg_extension
module O = Hmc_cfg_origin
module L = Hmc_cfg_lower

type function_entry = {start : D.index; return_label : D.index; trace : O.t}
type functions = No_functions | Function of function_entry * functions [@@inductive]
let[@def] rec (size @ total) (functions : functions @ immutable) = match functions with
  | No_functions -> D.Z | Function (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (functions : functions @ immutable) (id : D.index @ immutable) = match functions with
  | No_functions -> None | Function (entry, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then Some entry else lookup rest id
let[@def] (return_block @ total) (entry : K.entry @ immutable) =
  {G.signature = {G.locals = K.context entry; temporaries = G.Empty_temporaries; accumulator = Some entry.K.result}; instruction = G.Return}
let[@def] (function_valid @ total) (blocks : G.table @ immutable) (source : K.entry @ immutable)
    (entry : function_entry @ immutable) = ghost_ (
  entry.start === O.entry entry.trace && G.entry blocks entry.start (K.context source) G.Empty_temporaries
  && O.generated blocks source.K.body entry.return_label entry.trace
  && G.lookup blocks entry.return_label === Some (return_block source))
let[@def] rec (mapped @ total) (blocks : G.table @ immutable) (source : K.table @ immutable)
    (functions : functions @ immutable) = ghost_ (match source, functions with
  | K.Empty, No_functions -> true
  | K.Add (entry, rest), Function (compiled, tail) -> function_valid blocks entry compiled && mapped blocks rest tail
  | _ -> false)
let (preserve_function @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (source : K.entry) @ immutable -> (entry : function_entry) @ immutable ->
    {u : unit | G.extends larger smaller && function_valid smaller source entry} ->
    {u : unit | function_valid larger source entry} @ ghost = fun larger smaller source entry premise -> ghost_ (
  function_valid_def smaller source entry; function_valid_def larger source entry;
  E.entry larger smaller entry.start (K.context source) G.Empty_temporaries ();
  O.preserve larger smaller source.K.body entry.return_label entry.trace ();
  E.lookup larger smaller entry.return_label (return_block source) ())
let rec (preserve @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (source : K.table) @ immutable -> (functions : functions) @ immutable ->
    {u : unit | G.extends larger smaller && mapped smaller source functions} ->
    {u : unit | mapped larger source functions} @ ghost = fun larger smaller source functions premise -> ghost_ (
  mapped_def smaller source functions; mapped_def larger source functions;
  match source, functions with K.Add (entry, rest), Function (compiled, tail) ->
    preserve_function larger smaller entry compiled (); preserve larger smaller rest tail () | _ -> ())

type result = {blocks : G.table; functions : functions}
let rec (lower @ total) : (interface : M.table) @ immutable -> (closures : K.table) @ immutable ->
    (initial : G.table) @ immutable -> (source : K.table) @ immutable ->
    {u : unit | G.valid interface closures initial && K.valid interface source && K.extends closures source} ->
    {r : result | G.valid interface closures r.blocks && G.extends r.blocks initial && mapped r.blocks source r.functions} @ immutable =
  fun interface closures initial source premise ->
    ghost_ (K.valid_def interface source);
    match source with
    | K.Empty ->
      ghost_ (G.extends_def initial initial; mapped_def initial source No_functions);
      {blocks = initial; functions = No_functions}
    | K.Add (entry, rest) ->
      ghost_ (K.extends_def rest rest; K.extends_def source rest; X.transitive closures source rest ());
      let previous = lower interface closures initial rest () in
      let return_block = return_block entry in
      ghost_ (return_block_def entry; G.block_valid_def interface closures previous.blocks return_block);
      let returned = E.emit interface closures previous.blocks return_block () in
      ghost_ (X.typing interface closures rest (K.context entry) entry.K.body entry.K.result entry.K.derivation ();
        G.accepts_def returned.E.table returned.E.label (K.context entry) G.Empty_temporaries (Some entry.K.result));
      let body = L.lower interface closures returned.E.table (K.context entry) G.Empty_temporaries entry.K.body
        entry.K.result entry.K.derivation returned.E.label () in
      let compiled = {start = O.entry body.L.trace; return_label = returned.E.label; trace = body.L.trace} in
      let functions = Function (compiled, previous.functions) in
      ghost_ (E.transitive body.L.table returned.E.table previous.blocks (); E.transitive body.L.table previous.blocks initial ();
        preserve body.L.table previous.blocks rest previous.functions ();
        E.lookup body.L.table returned.E.table returned.E.label return_block ();
        function_valid_def body.L.table entry compiled; mapped_def body.L.table source functions);
      {blocks = body.L.table; functions}

type payload = {origin : P.program; blocks : G.table; functions : functions}
let[@def] (valid @ total) (p : payload @ immutable) = ghost_ (
  G.valid (C.manifest p.origin.P.origin.C.definitions) p.origin.P.table p.blocks
  && mapped p.blocks p.origin.P.table p.functions)
type program = {p : payload | valid p}
let (build @ total) : (origin : P.program) @ immutable -> {p : program | p.origin === origin} @ immutable = fun origin ->
  let interface = C.manifest origin.P.origin.C.definitions in
  ghost_ (P.valid_def origin; G.valid_def interface origin.P.table G.Empty; K.extends_def origin.P.table origin.P.table);
  let compiled = lower interface origin.P.table G.Empty origin.P.table () in
  let out = {origin; blocks = compiled.blocks; functions = compiled.functions} in
  ghost_ (valid_def out);
  let out : program = refine_ out in out

let rec (same_size @ total) : (blocks : G.table) @ immutable -> (source : K.table) @ immutable -> (functions : functions) @ immutable ->
    {u : unit | mapped blocks source functions} -> {u : unit | K.size source === size functions} @ ghost =
  fun blocks source functions premise -> ghost_ (
    mapped_def blocks source functions; K.size_def source; size_def functions;
    match source, functions with K.Add (_, rest), Function (_, tail) -> same_size blocks rest tail () | _ -> ())
let rec (lookup_origin @ total) : (blocks : G.table) @ immutable -> (source : K.table) @ immutable ->
    (functions : functions) @ immutable -> (id : D.index) @ immutable -> (entry : K.entry) @ immutable ->
    {u : unit | mapped blocks source functions && K.lookup source id === Some entry} ->
    {r : function_entry | lookup functions id === Some r && function_valid blocks entry r} @ immutable =
  fun blocks source functions id entry premise ->
    ghost_ (mapped_def blocks source functions; K.lookup_def source id; lookup_def functions id);
    match source, functions with
    | K.Add (_, rest), Function (compiled, tail) ->
      ghost_ (same_size blocks rest tail ());
      if Hm_elaboration_check.index_equal id (size tail) then compiled else lookup_origin blocks rest tail id entry ()
    | _ -> unreachable_ ()
