open Effect

type _ eff += A : unit eff

module Deep = Effect.Deep

let module_alias (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  let result =
    try
      perform A;
      x
    with
    | effect A, continuation -> Deep.continue continuation ()
  in
  result
