module C = Wasm_code
module B = Wasm_u32
module Dispatch = Hmc_wasm_program_dispatch
module Fuel = Wasm_control_compose
let (dispatch @ total) : (body : C.count) @ immutable -> (status : B.u32) ->
    {fuel : C.count | fuel === Dispatch.cost body status && not (fuel === C.Zero)} @ immutable =
  fun body status ->
    ghost_ (Dispatch.cost_def body status; Dispatch.two_def ();
      Fuel.add_def (Dispatch.two ()) (Fuel.add body (if status = 0 then Dispatch.three () else Dispatch.six ())));
    Dispatch.cost body status
