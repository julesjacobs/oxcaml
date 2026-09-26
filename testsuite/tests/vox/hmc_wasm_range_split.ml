module D = Hm_declarative
module Copy = Wasm_parallel_copy
module Lower = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
let[@def] (split @ total) (plan : Copy.plan @ immutable) (source : int) (destination : int) (length : D.index @ immutable) =
  ghost_ (Geometry.split plan source destination length)
let rec (correct @ total) : (plan : Copy.plan) @ immutable -> (source : int) -> (destination : int) ->
    (length : D.index) @ immutable -> (tail : Copy.plan) @ immutable ->
    {u : unit | Lower.range_is plan source destination length tail = (split plan source destination length === Some tail)} @ ghost =
  fun plan source destination length tail -> ghost_ (
    Lower.range_is_def plan source destination length tail; split_def plan source destination length; Geometry.split_def plan source destination length;
    match length, plan with
    | D.S remaining, Copy.Copy (_, _, Copy.Copy (_, _, rest)) -> correct rest (source + 1) (destination + 1) remaining tail; split_def rest (source + 1) (destination + 1) remaining
    | _ -> ())
