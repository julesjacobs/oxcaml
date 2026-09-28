val add : int -> int -> int @@ total
val equal : int -> int -> bool @@ total

type addition =
  (left : int) ->
  (right : int) ->
  { result : int | equal result (add left right) }
