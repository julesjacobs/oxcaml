open Effect

type _ eff += B : int eff

let local_module_shadow () : int{ _ > 0 } =
  let module Stdlib = struct
    module Effect = struct
      let perform (_ : int eff) = 0
    end
  end
  in
  try Stdlib.Effect.perform B with
  | effect B, _continuation -> 1
