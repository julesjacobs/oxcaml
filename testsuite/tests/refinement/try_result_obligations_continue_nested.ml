open Effect
open Effect.Deep

type _ eff += A : unit eff
type _ eff += B : int eff

let nested_continue () : int{ _ = 1 } =
  let result =
    try
      try
        perform A;
        1
      with
      | effect B, _continuation -> 2
    with
    | effect A, continuation -> continue continuation ()
  in
  result
