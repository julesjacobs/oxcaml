open Effect
open Effect.Deep

type _ eff += A : int eff
type _ eff += B = A

let takes_three (value : int{ _ = 3 }) = value

let rebound_effect_summary (perform_a : bool) =
  match (if perform_a then perform A else 3) with
  | value ->
    let _ = takes_three value in
    value
  | effect B, continuation ->
    ignore (continue continuation 7);
    3
  | effect A, _continuation -> 3
