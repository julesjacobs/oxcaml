module D = Hm_declarative
module Index = Hmc_u32_index
module Geometry = Hmc_wasm_relayout_geometry
module Lower = Hmc_wasm_relayout
let (encode @ total) : (index : D.index) @ immutable -> (capacity : Lower.count) ->
    {u : unit | Index.fits index capacity} ->
    {out : Lower.count | out <= capacity && Index.represents index out && Geometry.size index = out} =
  fun index capacity premise ->
    match Index.encode capacity index with
    | None -> unreachable_ ()
    | Some number -> ghost_ (Geometry.size_represents index number ()); number
