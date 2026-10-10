module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Index = Hmc_u32_index
module Machine = Hmc_heap_machine
module Block = Hmc_wasm_structured_block
module Relayout = Hmc_wasm_relayout
type table = Empty | Add of W.limb * Block.fragment * table [@@inductive]
let[@def] rec (corresponds @ total) (globals : Machine.globals @ immutable) (source : G.table @ immutable) (target : table @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (match source, target with
  | G.Empty, Empty -> true
  | G.Add (block, rest), Add (label, fragment, tail) ->
    Index.represents (G.size rest) label && label <= max_pc
    && Block.corresponds globals block.G.signature block.G.instruction capacity max_pc fragment
    && corresponds globals rest tail capacity max_pc
  | _ -> false)
let rec (lower @ total) : (globals : Machine.globals) @ immutable -> (source : G.table) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : table option | match out with None -> true | Some target -> corresponds globals source target capacity max_pc} @ immutable =
  fun globals source capacity max_pc -> match source with
  | G.Empty -> ghost_ (corresponds_def globals source Empty capacity max_pc); Some Empty
  | G.Add (block, rest) ->
    (match Index.encode max_pc (G.size rest), Block.lower globals block.G.signature block.G.instruction capacity max_pc with
    | Some label, Some fragment -> (match lower globals rest capacity max_pc with
      | None -> None
      | Some tail -> let target = Add (label, fragment, tail) in ghost_ (corresponds_def globals source target capacity max_pc); Some target)
    | _ -> None)
let[@def] rec (lookup @ total) (target : table @ immutable) (number : W.limb) = match target with
  | Empty -> None | Add (label, fragment, rest) -> if number = label then Some fragment else lookup rest number
let rec (lookup_correct @ total) : (globals : Machine.globals) @ immutable -> (source : G.table) @ immutable -> (target : table) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) -> (id : D.index) @ immutable -> (number : W.limb) ->
    {u : unit | corresponds globals source target capacity max_pc && Index.represents id number} ->
    {u : unit | match G.lookup source id, lookup target number with
      | None, None -> true
      | Some block, Some fragment -> Block.corresponds globals block.G.signature block.G.instruction capacity max_pc fragment
      | _ -> false} @ ghost = fun globals source target capacity max_pc id number premise -> ghost_ (
    corresponds_def globals source target capacity max_pc; G.lookup_def source id; lookup_def target number;
    match source, target with
    | G.Add (_, rest), Add (label, _, tail) ->
      let same = Hm_elaboration_check.index_equal id (G.size rest) in
      if number = label then Index.injective id (G.size rest) number ()
      else if same then Index.unique id number label ()
      else lookup_correct globals rest tail capacity max_pc id number ()
    | _ -> ())
