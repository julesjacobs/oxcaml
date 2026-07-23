open Effect

type _ eff += A : int eff

module Stdlib = struct
  module Effect = struct
    let perform (_ : int eff) = 0
  end
end

let match_shadow () : int{ _ > 0 } =
  let result =
    match Stdlib.Effect.perform A with
    | value -> value
    | effect A, _continuation -> 1
  in
  result
