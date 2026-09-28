module D = Hm_declarative
module G = Hmc_cfg_ir
module M = Hmc_manifest

let rec (range @ total) : (table : G.table) @ immutable -> (id : D.index) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.lookup table id === Some block} -> {u : unit | D.present (G.size table) id} @ ghost = fun table id block premise -> ghost_ (
  G.lookup_def table id; G.size_def table;
  match table with G.Empty -> () | G.Add (_, rest) ->
    if Hm_elaboration_check.index_equal id (G.size rest) then M.self_present (G.size rest)
    else (range rest id block (); M.weaken (G.size rest) id ()))
let rec (lookup @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (id : D.index) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.extends larger smaller && G.lookup smaller id === Some block} ->
    {u : unit | G.lookup larger id === Some block} @ ghost = fun larger smaller id block premise -> ghost_ (
  G.extends_def larger smaller;
  if larger === smaller then () else match larger with G.Empty -> () | G.Add (_, rest) ->
    lookup rest smaller id block (); range rest id block (); M.irreflexive (G.size rest);
    G.lookup_def larger id; let _ = Hm_elaboration_check.index_equal id (G.size rest) in ())
let rec (transitive @ total) : (a : G.table) @ immutable -> (b : G.table) @ immutable -> (c : G.table) @ immutable ->
    {u : unit | G.extends a b && G.extends b c} -> {u : unit | G.extends a c} @ ghost = fun a b c premise -> ghost_ (
  G.extends_def a b; G.extends_def a c;
  if a === b then () else match a with G.Empty -> () | G.Add (_, rest) -> transitive rest b c ())
let (accepts @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (id : D.index) @ immutable -> (locals : D.context) @ immutable -> (temps : G.temporaries) @ immutable ->
    (acc : D.mono option) @ immutable -> {u : unit | G.extends larger smaller && G.accepts smaller id locals temps acc} ->
    {u : unit | G.accepts larger id locals temps acc} @ ghost = fun larger smaller id locals temps acc premise -> ghost_ (
  G.accepts_def smaller id locals temps acc; G.accepts_def larger id locals temps acc;
  match G.lookup smaller id with None -> () | Some block -> lookup larger smaller id block ())
let (entry @ total) : (larger : G.table) @ immutable -> (smaller : G.table) @ immutable ->
    (id : D.index) @ immutable -> (locals : D.context) @ immutable -> (temps : G.temporaries) @ immutable ->
    {u : unit | G.extends larger smaller && G.entry smaller id locals temps} ->
    {u : unit | G.entry larger id locals temps} @ ghost = fun larger smaller id locals temps premise -> ghost_ (
  G.entry_def smaller id locals temps; G.entry_def larger id locals temps;
  match G.lookup smaller id with None -> () | Some block -> lookup larger smaller id block ())
let (entry_accepts @ total) : (table : G.table) @ immutable -> (id : D.index) @ immutable ->
    (locals : D.context) @ immutable -> (temps : G.temporaries) @ immutable -> (acc : D.mono option) @ immutable ->
    {u : unit | G.entry table id locals temps} -> {u : unit | G.accepts table id locals temps acc} @ ghost =
  fun table id locals temps acc premise -> ghost_ (G.entry_def table id locals temps; G.accepts_def table id locals temps acc)

type emitted = {table : G.table; label : D.index}
let (emit @ total) : (interface : M.table) @ immutable -> (closures : Hmc_closure_ir.table) @ immutable ->
    (initial : G.table) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.valid interface closures initial && G.block_valid interface closures initial block} ->
    {r : emitted | G.valid interface closures r.table && G.extends r.table initial
      && G.lookup r.table r.label === Some block} @ immutable = fun interface closures initial block premise ->
  let table = G.Add (block, initial) in let label = G.size initial in
  ghost_ (G.valid_def interface closures table; G.extends_def initial initial; G.extends_def table initial;
    G.lookup_def table label; let _ = Hm_elaboration_check.index_equal label label in ());
  {table; label}

let (block @ total) : (interface : M.table) @ immutable -> (closures : Hmc_closure_ir.table) @ immutable ->
    (larger : G.table) @ immutable -> (smaller : G.table) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.extends larger smaller && G.block_valid interface closures smaller block} ->
    {u : unit | G.block_valid interface closures larger block} @ ghost = fun interface closures larger smaller block premise -> ghost_ (
  G.block_valid_def interface closures smaller block; G.block_valid_def interface closures larger block;
  let s = block.G.signature in
  match block.G.instruction with
  | G.Load (_, ty, _, next) -> accepts larger smaller next s.G.locals s.G.temporaries (Some ty) ()
  | G.Jump next -> accepts larger smaller next s.G.locals s.G.temporaries None ()
  | G.Save_environment next -> accepts larger smaller next s.G.locals (G.Environment (s.G.locals, s.G.temporaries)) None ()
  | G.Save_value next -> (match s.G.accumulator, s.G.temporaries with
    | Some ty, G.Environment (g, rest) -> accepts larger smaller next g (G.Value (g, ty, rest)) None () | _ -> ())
  | G.Bind next -> (match s.G.accumulator, s.G.temporaries with
    | Some ty, G.Environment (g, _) -> accepts larger smaller next (D.Binding (D.Forall (D.Z, ty), g)) s.G.temporaries None () | _ -> ())
  | G.Restore next -> (match s.G.accumulator, s.G.temporaries with
    | Some ty, G.Environment (g, rest) -> accepts larger smaller next g rest (Some ty) () | _ -> ())
  | G.Primitive (op, next) -> (match s.G.accumulator, s.G.temporaries with
    | Some D.Word64, G.Value (g, D.Word64, rest) -> accepts larger smaller next g rest (Some (D.operation_type op)) () | _ -> ())
  | G.Cons next -> (match s.G.accumulator, s.G.temporaries with
    | Some (D.List_type _), G.Value (g, _, rest) -> accepts larger smaller next g rest s.G.accumulator () | _ -> ())
  | G.Call next -> (match s.G.accumulator, s.G.temporaries with
    | Some _, G.Value (g, D.Function (_, result), rest) -> accepts larger smaller next g rest (Some result) () | _ -> ())
  | G.Branch (yes, no) -> accepts larger smaller yes s.G.locals s.G.temporaries None (); accepts larger smaller no s.G.locals s.G.temporaries None ()
  | G.List_branch (empty, full) -> (match s.G.accumulator with Some (D.List_type a) ->
    accepts larger smaller empty s.G.locals s.G.temporaries None ();
    accepts larger smaller full (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), s.G.locals)))
      (G.Environment (s.G.locals, s.G.temporaries)) None () | _ -> ())
  | G.Return -> ())
let rec (lookup_valid @ total) : (interface : M.table) @ immutable -> (closures : Hmc_closure_ir.table) @ immutable ->
    (table : G.table) @ immutable -> (id : D.index) @ immutable -> (selected : G.block) @ immutable ->
    {u : unit | G.valid interface closures table && G.lookup table id === Some selected} ->
    {u : unit | G.block_valid interface closures table selected} @ ghost = fun interface closures table id selected premise -> ghost_ (
  G.valid_def interface closures table; G.lookup_def table id;
  match table with G.Empty -> () | G.Add (_, rest) ->
    G.extends_def rest rest; G.extends_def table rest;
    if Hm_elaboration_check.index_equal id (G.size rest) then block interface closures table rest selected ()
    else (lookup_valid interface closures rest id selected (); block interface closures table rest selected ()))
