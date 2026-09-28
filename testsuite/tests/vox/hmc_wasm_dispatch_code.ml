module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module T = Wasm_control
module Block = Hmc_wasm_structured_block
module Source = Hmc_wasm_structured_table
type table = Empty | Add of W.limb * T.code * table [@@inductive]
let[@def] rec (lookup @ total) (table : table @ immutable) (number : W.limb) = match table with
  | Empty -> None | Add (label, code, rest) -> if number = label then Some code else lookup rest number
let[@def] rec (corresponds @ total) (source : Source.table @ immutable) (target : table @ immutable)
    (locals : Block.locals @ immutable) (depth : B.u32) = ghost_ (match source, target with
  | Source.Empty, Empty -> true
  | Source.Add (number, fragment, rest), Add (label, code, tail) ->
    depth <= 4294967293 && number = label && code === Block.emit fragment locals (S.add32 depth 2)
    && corresponds rest tail locals (S.add32 depth 1)
  | _ -> false)
let rec (prepare @ total) : (source : Source.table) @ immutable -> (locals : Block.locals) @ immutable -> (depth : B.u32) ->
    {out : table option | match out with None -> true | Some target -> corresponds source target locals depth} @ immutable =
  fun source locals depth -> match source with
  | Source.Empty -> ghost_ (corresponds_def source Empty locals depth); Some Empty
  | Source.Add (number, fragment, rest) ->
    if depth > 4294967293 then None else
    (match prepare rest locals (S.add32 depth 1) with
    | None -> None
    | Some tail ->
      let target = Add (number, Block.emit fragment locals (S.add32 depth 2), tail) in
      ghost_ (corresponds_def source target locals depth); Some target)
let[@def] rec (exit_depth @ total) (source : Source.table @ immutable) (number : W.limb) (depth : B.u32) : B.u32 =
  match source with
  | Source.Empty -> depth
  | Source.Add (label, _, rest) -> if number = label then S.add32 depth 2 else exit_depth rest number (S.add32 depth 1)
let rec (lookup_correct @ total) : (source : Source.table) @ immutable -> (target : table) @ immutable ->
    (locals : Block.locals) @ immutable -> (depth : B.u32) -> (number : W.limb) ->
    {u : unit | corresponds source target locals depth} ->
    {u : unit | lookup target number === (match Source.lookup source number with
      | None -> None | Some fragment -> Some (Block.emit fragment locals (exit_depth source number depth)))} @ ghost =
  fun source target locals depth number premise -> ghost_ (
    corresponds_def source target locals depth; Source.lookup_def source number; lookup_def target number;
    exit_depth_def source number depth;
    match source, target with
    | Source.Add (label, _, rest), Add (_, _, tail) ->
      if number = label then () else lookup_correct rest tail locals (S.add32 depth 1) number ()
    | _ -> ())
