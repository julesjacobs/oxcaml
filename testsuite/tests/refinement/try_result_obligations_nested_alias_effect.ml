open Effect

module M = struct
  type _ eff += A : unit eff
end

module N = M

let nested_alias_effect () : int{ _ = 0 } =
  try
    try
      perform M.A;
      let alias_resumed_body_result = 1 in
      alias_resumed_body_result
    with
    | effect N.A, continuation ->
      ignore continuation;
      0
    | effect M.A, _continuation -> 0
  with
  | effect M.A, _continuation -> 0
