open Effect

type _ eff += A : int eff

module Stdlib = struct
  module Effect = struct
    let perform (_ : int eff) = 0
  end
end

let module_shadow () : int{ _ > 0 } =
  try Stdlib.Effect.perform A with
  | effect A, _continuation -> 1
