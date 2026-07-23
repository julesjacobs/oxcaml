open Effect

type _ eff += A : unit eff

let local_module_alias (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  let module Deep = Effect.Deep in
  let result =
    try
      perform A;
      x
    with
    | effect A, continuation -> Deep.continue continuation ()
  in
  result
