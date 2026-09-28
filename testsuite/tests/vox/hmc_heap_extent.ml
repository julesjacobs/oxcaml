module D = Hm_declarative
module W = Hmc_word64

let[@def] rec (fits @ total) (cells : D.index @ immutable) (start : W.limb) (limit : W.limb) =
  if start > limit then false else match cells with
  | D.Z -> true | D.S rest -> if limit - start < 16 then false else fits rest (start + 16) limit
let[@def] rec (span @ total) (cells : D.index @ immutable) (start : W.limb) (stop : W.limb) = match cells with
  | D.Z -> stop = start
  | D.S rest -> if start > 4294967279 then false else span rest (start + 16) stop
let rec (reserve @ total) : (cells : D.index) @ immutable -> (start : W.limb) -> (limit : W.limb) ->
    {out : W.limb option | match out with
      | None -> not (fits cells start limit)
      | Some stop -> fits cells start limit && span cells start stop && stop <= limit} @ immutable = fun cells start limit ->
  ghost_ (fits_def cells start limit; span_def cells start start);
  if start > limit then None else match cells with
  | D.Z -> Some start
  | D.S rest ->
    if limit - start < 16 then None else
    match reserve rest (start + 16) limit with
    | None -> None
    | Some stop -> ghost_ (span_def cells start stop); Some stop
let rec (ordered @ total) : (cells : D.index) @ immutable -> (start : W.limb) -> (stop : W.limb) ->
    {u : unit | span cells start stop} ->
    {u : unit | start <= stop && (match cells with D.Z -> true | D.S _ -> start + 16 <= stop)} @ ghost = fun cells start stop premise -> ghost_ (
  span_def cells start stop; match cells with D.Z -> () | D.S rest -> ordered rest (start + 16) stop ())
let rec (sufficient @ total) : (cells : D.index) @ immutable -> (start : W.limb) -> (stop : W.limb) -> (limit : W.limb) ->
    {u : unit | span cells start stop && stop <= limit} -> {u : unit | fits cells start limit} @ ghost =
  fun cells start stop limit premise -> ghost_ (
    span_def cells start stop; fits_def cells start limit; ordered cells start stop ();
    match cells with D.Z -> () | D.S rest -> sufficient rest (start + 16) stop limit ())
let rec (unique @ total) : (cells : D.index) @ immutable -> (start : W.limb) -> (left : W.limb) -> (right : W.limb) ->
    {u : unit | span cells start left && span cells start right} -> {u : unit | left = right} @ ghost =
  fun cells start left right premise -> ghost_ (
    span_def cells start left; span_def cells start right;
    match cells with D.Z -> () | D.S rest -> unique rest (start + 16) left right ())
