open Effect
open Effect.Deep

type _ eff += A : unit eff

let continue_then_code () : int{ _ = 1 } =
  let result =
    try
      perform A;
      1
    with
    | effect A, continuation ->
      let _resumed = continue continuation () in
      0
  in
  result
