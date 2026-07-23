open Stdlib
open Stdlib.Effect

type _ eff += A : int eff

let custom_stdlib () : int{ _ = 1 } =
  let result =
    try
      Stdlib.Effect.perform A;
      1
    with
    | effect A, continuation ->
      Stdlib.Effect.Deep.continue continuation 0
  in
  result
