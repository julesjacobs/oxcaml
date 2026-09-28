module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Index = Hmc_u32_index
module Machine = Hmc_heap_machine
module Block = Hmc_wasm_program_block
module R = Hmc_wasm_relayout
type table = Empty | Add of B.u32 * Block.fragment * table [@@inductive]
let[@def] rec (corresponds @ total) (globals : Machine.globals @ immutable) (source : G.table @ immutable)
    (rewritten : I.table @ immutable) (target : table @ immutable) (capacity : R.count) (max_pc : B.u32) = ghost_ (
  match source, rewritten, target with
  | G.Empty, I.Empty, Empty -> true
  | G.Add (block, rest), I.Add (instruction, code), Add (label, fragment, tail) ->
    Index.represents (G.size rest) label && label <= max_pc
    && Block.corresponds globals block.G.signature instruction capacity max_pc fragment
    && corresponds globals rest code tail capacity max_pc
  | _ -> false)
let[@def] rec (encodable @ total) (globals : Machine.globals @ immutable) (source : G.table @ immutable)
    (rewritten : I.table @ immutable) (capacity : R.count) (max_pc : B.u32) = ghost_ (
  match source, rewritten with
  | G.Empty, I.Empty -> true
  | G.Add (block, rest), I.Add (instruction, code) -> Index.fits (G.size rest) max_pc
    && Block.encodable globals block.G.signature instruction capacity max_pc
    && encodable globals rest code capacity max_pc
  | _ -> false)
let rec (lower @ total) : (globals : Machine.globals) @ immutable -> (source : G.table) @ immutable ->
    (rewritten : I.table) @ immutable -> (capacity_index : D.index) @ immutable -> (capacity : R.count) -> (max_pc : B.u32) ->
    {u : unit | Index.represents capacity_index capacity && capacity < 268435452} ->
    {out : table option | match out with None -> not (encodable globals source rewritten capacity max_pc) | Some target -> encodable globals source rewritten capacity max_pc && corresponds globals source rewritten target capacity max_pc} @ immutable =
  fun globals source rewritten capacity_index capacity max_pc premise ->
  ghost_ (encodable_def globals source rewritten capacity max_pc);
  match source, rewritten with
  | G.Empty, I.Empty -> ghost_ (corresponds_def globals source rewritten Empty capacity max_pc); Some Empty
  | G.Add (block, rest), I.Add (instruction, code) ->
    (match Index.encode max_pc (G.size rest), Block.lower globals block.G.signature instruction capacity_index capacity max_pc () with
    | Some label, Some fragment -> (match lower globals rest code capacity_index capacity max_pc () with
      | None -> None
      | Some tail -> let target = Add (label, fragment, tail) in
        ghost_ (corresponds_def globals source rewritten target capacity max_pc); Some target)
    | _ -> None)
  | _ -> None
let[@def] rec (lookup @ total) (target : table @ immutable) (number : B.u32) = match target with
  | Empty -> None | Add (label, fragment, rest) -> if number = label then Some fragment else lookup rest number
let rec (lookup_correct @ total) : (globals : Machine.globals) @ immutable -> (source : G.table) @ immutable ->
    (rewritten : I.table) @ immutable -> (sites : Hmc_tail_sites.sites) @ immutable -> (target : table) @ immutable ->
    (capacity : R.count) -> (max_pc : B.u32) -> (id : D.index) @ immutable -> (number : B.u32) ->
    {u : unit | corresponds globals source rewritten target capacity max_pc && I.related source rewritten sites && Index.represents id number} ->
    {u : unit | match G.lookup source id, I.lookup rewritten id, lookup target number with
      | None, None, None -> true
      | Some block, Some instruction, Some fragment -> Block.corresponds globals block.G.signature instruction capacity max_pc fragment
      | _ -> false} @ ghost = fun globals source rewritten sites target capacity max_pc id number premise -> ghost_ (
    corresponds_def globals source rewritten target capacity max_pc;
    I.related_def source rewritten sites;
    G.lookup_def source id; I.lookup_def rewritten id; lookup_def target number;
    match source, rewritten, target with
    | G.Add (_, rest), I.Add (_, code), Add (label, _, tail) ->
      I.same_size rest code sites ();
      let same = Hm_elaboration_check.index_equal id (G.size rest) in
      if number = label then Index.injective id (G.size rest) number ()
      else if same then Index.unique id number label ()
      else lookup_correct globals rest code sites tail capacity max_pc id number ()
    | _ -> ())
