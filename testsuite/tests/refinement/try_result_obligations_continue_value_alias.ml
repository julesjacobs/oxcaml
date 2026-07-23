open Effect

type _ eff += A : unit eff

let value_alias (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  let resume = Effect.Deep.continue in
  let result =
    try
      perform A;
      x
    with
    | effect A, continuation -> resume continuation ()
  in
  result
