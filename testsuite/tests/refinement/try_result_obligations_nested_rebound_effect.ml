open Effect

type _ eff += A : unit eff
type _ eff += B = A

let nested_rebound_effect () : int{ _ = 0 } =
  try
    try
      perform A;
      let rebound_resumed_body_result = 1 in
      rebound_resumed_body_result
    with
    | effect B, continuation ->
      ignore continuation;
      0
    | effect A, _continuation -> 0
  with
  | effect A, _continuation -> 0
