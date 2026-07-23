open Effect
open Effect.Deep

type _ eff += A : int eff
type _ eff += B : int eff

let resume_a thunk =
  match thunk () with
  | value -> value
  | effect A, continuation -> continue continuation 0

let unmatched_leaf condition : int{ _ > 0 } =
  try
    if condition then perform A else 1
  with
  | effect B, _continuation -> 2

let observe_unmatched_leaf condition =
  resume_a (fun () -> unmatched_leaf condition)
