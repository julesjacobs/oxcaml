open Effect

type _ eff += A : unit eff

let nested_same_effect () : int{ _ = 0 } =
  try
    try
      perform A;
      let resumed_body_result = 1 in
      resumed_body_result
    with
    | effect A, continuation ->
      ignore continuation;
      let inner_handler_result = 0 in
      inner_handler_result
  with
  | effect A, _continuation ->
    let outer_handler_result = 0 in
    outer_handler_result
