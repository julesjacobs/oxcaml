module D = Hm_declarative
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module T = Hmc_tail_sites

let[@def] rec (find @ total) (sites : T.sites @ immutable) (label : D.index @ immutable) = match sites with
  | T.Empty -> None | T.Site (id, exit, rest) -> if Hm_elaboration_check.index_equal id label then Some exit else find rest label
let rec (find_valid @ total) : (blocks : G.table) @ immutable -> (sites : T.sites) @ immutable -> (label : D.index) @ immutable ->
    (exit : T.exit) @ immutable -> {u : unit | T.valid blocks sites && find sites label === Some exit} ->
    {u : unit | O.instruction blocks label (G.Call (T.entry exit)) && T.exit_valid blocks exit} @ ghost =
  fun blocks sites label exit premise -> ghost_ (
    find_def sites label; T.valid_def blocks sites;
    match sites with T.Empty -> () | T.Site (id, _, rest) ->
      if Hm_elaboration_check.index_equal id label then () else find_valid blocks rest label exit ())

type instruction = Keep of G.instruction | Tail_call [@@inductive]
let[@def] (select @ total) (sites : T.sites @ immutable) (label : D.index @ immutable) (op : G.instruction @ immutable) =
  match find sites label with None -> Keep op | Some _ -> Tail_call

type table = Empty | Add of instruction * table [@@inductive]
let[@def] rec (size @ total) (table : table @ immutable) = match table with Empty -> D.Z | Add (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (table : table @ immutable) (label : D.index @ immutable) = match table with
  | Empty -> None | Add (op, rest) -> if Hm_elaboration_check.index_equal label (size rest) then Some op else lookup rest label
let[@def] rec (related @ total) (source : G.table @ immutable) (target : table @ immutable) (sites : T.sites @ immutable) = ghost_ (
  match source, target with
  | G.Empty, Empty -> true
  | G.Add (block, rest), Add (op, tail) -> op === select sites (G.size rest) block.G.instruction && related rest tail sites
  | _ -> false)
let rec (rewrite @ total) : (source : G.table) @ immutable -> (sites : T.sites) @ immutable ->
    {out : table | related source out sites} @ immutable = fun source sites ->
  match source with
  | G.Empty -> ghost_ (related_def source Empty sites); Empty
  | G.Add (block, rest) ->
    let tail = rewrite rest sites in
    let out = Add (select sites (G.size rest) block.G.instruction, tail) in
    ghost_ (related_def source out sites); out
let rec (same_size @ total) : (source : G.table) @ immutable -> (target : table) @ immutable -> (sites : T.sites) @ immutable ->
    {u : unit | related source target sites} -> {u : unit | G.size source === size target} @ ghost =
  fun source target sites premise -> ghost_ (
    related_def source target sites; G.size_def source; size_def target;
    match source, target with G.Add (_, rest), Add (_, tail) -> same_size rest tail sites () | _ -> ())
let rec (lookup_related @ total) : (source : G.table) @ immutable -> (target : table) @ immutable -> (sites : T.sites) @ immutable ->
    (label : D.index) @ immutable -> {u : unit | related source target sites} ->
    {u : unit | lookup target label === (match G.lookup source label with None -> None
      | Some block -> Some (select sites label block.G.instruction))} @ ghost =
  fun source target sites label premise -> ghost_ (
    related_def source target sites; G.lookup_def source label; lookup_def target label;
    match source, target with G.Add (_, rest), Add (_, tail) ->
      same_size rest tail sites ();
      if Hm_elaboration_check.index_equal label (G.size rest) then () else lookup_related rest tail sites label ()
    | _ -> ())

type payload = {origin : C.program; sites : T.sites; code : table}
let[@def] (valid @ total) (p : payload @ immutable) = ghost_ (T.valid p.origin.C.blocks p.sites && related p.origin.C.blocks p.code p.sites)
type program = {p : payload | valid p}
let (build @ total) : (origin : C.program) @ immutable -> {out : program | out.origin === origin} @ immutable = fun origin ->
  let sites = T.program origin in
  let code = rewrite origin.C.blocks sites in
  let out = {origin; sites; code} in ghost_ (valid_def out); let out : program = refine_ out in out
