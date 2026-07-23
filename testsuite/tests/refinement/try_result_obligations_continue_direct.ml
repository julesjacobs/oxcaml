open Effect
open Effect.Deep

type _ eff += A : unit eff

let direct_continue (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  let result =
    try
      perform A;
      x
    with
    | effect A, continuation -> continue continuation ()
  in
  result
