val next : int -> int @@ total
val next_def : (x : int) -> {u : unit | next x === x + 2} @@ total
val choose : bool -> int -> int -> int @@ total
val choose_def :
  (b : bool) -> (x : int) -> (y : int) ->
  {u : unit | choose b x y === if b then x + 1 else y - 1} @@ total

type 'a box = Box of 'a

val box : 'a @ immutable -> 'a box @ immutable @@ total
val box_def : (x : 'a) -> {u : unit | box x === Box x} @@ total

val dependent : (x : int) -> {v : int | v = x} -> int @@ total
val dependent_def :
  (x : int) -> (y : {v : int | v = x}) ->
  {u : unit | dependent x y === x} @@ total

val witnessed : int -> int @@ total
val witnessed_def : (x : int) -> {u : unit | witnessed x === x} @@ total

type ghost_identity = {x : int | ghost_ (x + 1) === x + 1}
