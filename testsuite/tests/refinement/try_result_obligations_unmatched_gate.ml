open Effect
open Effect.Deep

type _ eff += A : unit eff
type _ eff += B : int eff

let resume_a thunk =
  match thunk () with
  | value -> value
  | effect A, continuation -> continue continuation ()

let unmatched_gate () : int{ _ > 0 } =
  try
    perform A;
    0
  with
  | effect B, _continuation -> 1

let observe_unmatched_gate () = resume_a unmatched_gate
