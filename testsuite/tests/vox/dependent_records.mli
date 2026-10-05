type interval = { lower : int; upper : {u : int | lower <= u} }
val make : (lower : int) -> (upper : {u : int | lower <= u}) -> interval @@ total
val upper : (r : interval) -> {u : int | r.lower <= u} @@ total

type minimum = {
  value : int;
  optimality : (other : {n : int | 0 <= n}) ->
    {u : unit | value <= other} @@ ghost total;
}
val minimum : unit -> minimum @@ total
