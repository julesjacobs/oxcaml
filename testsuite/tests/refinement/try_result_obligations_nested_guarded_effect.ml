open Effect

type _ eff += A : unit eff

let nested_guarded_effect (condition : bool) : int{ _ = 0 } =
  try
    try
      perform A;
      let guarded_resumed_body_result = 1 in
      guarded_resumed_body_result
    with
    | effect A, continuation when condition ->
      ignore continuation;
      0
    | effect A, _continuation -> 0
  with
  | effect A, _continuation -> 0
