open Effect

type _ eff += Payload : int -> unit eff

let nested_refutable_effect () : int{ _ = 0 } =
  try
    try
      perform (Payload 0);
      let refutable_resumed_body_result = 1 in
      refutable_resumed_body_result
    with
    | effect Payload 0, continuation ->
      ignore continuation;
      0
    | effect Payload _, _continuation -> 0
  with
  | effect Payload _, _continuation -> 0
