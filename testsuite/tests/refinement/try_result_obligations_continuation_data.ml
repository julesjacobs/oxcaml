open Effect

type _ eff += A : unit eff

let continuation_as_data () : int{ _ = 0 } =
  let result =
    try
      perform A;
      1
    with
    | effect A, continuation ->
      ignore continuation;
      0
  in
  result
