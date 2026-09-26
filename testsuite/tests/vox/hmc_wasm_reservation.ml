module D = Hm_declarative
module W = Hmc_word64
module Index = Hmc_u32_index
module Extent = Hmc_heap_extent
type count = {n : W.limb | n <= 268435455}
let rec (fits @ total) : (cells : D.index) @ immutable -> (number : count) -> (start : W.limb) -> (limit : W.limb) ->
    {u : unit | Index.represents cells number} ->
    {u : unit | Extent.fits cells start limit = (start + 16 * number <= limit)} @ ghost =
  fun cells number start limit premise -> ghost_ (
    Index.represents_def cells number; Extent.fits_def cells start limit;
    match cells with
    | D.Z -> ()
    | D.S rest -> if start <= limit && limit - start >= 16 then fits rest (number - 1) (start + 16) limit () else ())
let rec (span @ total) : (cells : D.index) @ immutable -> (number : count) -> (start : W.limb) -> (stop : W.limb) ->
    {u : unit | Index.represents cells number && Extent.span cells start stop} ->
    {u : unit | stop = start + 16 * number} @ ghost = fun cells number start stop premise -> ghost_ (
    Index.represents_def cells number; Extent.span_def cells start stop;
    match cells with D.Z -> () | D.S rest -> span rest (number - 1) (start + 16) stop ())
let (reserve @ total) : (cells : D.index) @ immutable -> (number : count) -> (start : W.limb) -> (limit : W.limb) ->
    {u : unit | Index.represents cells number} ->
    {out : W.limb option | out === Extent.reserve cells start limit
      && (match out with None -> start + 16 * number > limit | Some stop -> stop = start + 16 * number && stop <= limit)} @ immutable =
  fun cells number start limit premise ->
    let out = Extent.reserve cells start limit in
    ghost_ (fits cells number start limit (); match out with None -> () | Some stop -> span cells number start stop ()); out
