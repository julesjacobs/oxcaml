module Real_effect = Effect
open Real_effect

type _ t += A : unit t

module Stdlib = struct
  module Effect = struct
    module Deep = struct
      let continue _continuation value = value
    end
  end
end

let continue_shadow () : int{ _ > 0 } =
  let result =
    try
      perform A;
      1
    with
    | effect A, continuation ->
      Stdlib.Effect.Deep.continue continuation 0
  in
  result
