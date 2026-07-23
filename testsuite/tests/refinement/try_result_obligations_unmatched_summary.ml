open Effect
open Effect.Deep

type _ eff += A : unit eff
type _ eff += B : int eff

let resume_a thunk =
  match thunk () with
  | value -> value
  | effect A, continuation -> continue continuation ()

let unmatched_summary () : int{ _ > 0 } =
  let result =
    try
      perform A;
      0
    with
    | effect B, _continuation -> 1
  in
  result

let observe_unmatched_summary () = resume_a unmatched_summary
