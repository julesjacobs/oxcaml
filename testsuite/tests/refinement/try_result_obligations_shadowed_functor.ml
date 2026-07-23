open Effect

type _ eff += C : int eff

module Make_stdlib () = struct
  module Effect = struct
    let perform (_ : int eff) = 0
  end
end

let functor_module_shadow () : int{ _ > 0 } =
  let module Stdlib = Make_stdlib () in
  try Stdlib.Effect.perform C with
  | effect C, _continuation -> 1
