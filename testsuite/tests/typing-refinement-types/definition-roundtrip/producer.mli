val next : int -> int @@ total
val next_def : (x : int) -> {u : unit | next x = x + 2} @@ total
val choose : bool -> int -> int -> int @@ total
val choose_def :
  (b : bool) -> (x : int) -> (y : int) ->
  {u : unit | choose b x y = if b then x + 1 else y - 1} @@ total
