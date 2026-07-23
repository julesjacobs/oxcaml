open Effect

type _ eff += A : int eff
type _ eff += B : int eff

let nested_try () : int{ _ > 0 } =
  try
    try perform A with
    | effect B, _continuation ->
      let inner_try_handler = 2 in
      inner_try_handler
  with
  | effect A, _continuation ->
    let outer_try_handler = 1 in
    outer_try_handler

let nested_match () : int{ _ > 0 } =
  try
    match perform A with
    | _value ->
      let match_value_result = 3 in
      match_value_result
    | effect B, _continuation ->
      let inner_match_handler = 2 in
      inner_match_handler
  with
  | effect A, _continuation ->
    let outer_match_handler = 1 in
    outer_match_handler
