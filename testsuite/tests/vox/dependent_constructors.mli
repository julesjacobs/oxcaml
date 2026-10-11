type _ bounded =
  | Bounded : {
      lower : int;
      value : {v : int | lower <= v};
    } -> int bounded
  | Empty : unit bounded

val make : (lower : int) -> (value : {v : int | lower <= v}) -> int bounded @@ total

type minimum =
  | Minimum : {
      value : int;
      optimality : (other : {n : int | 0 <= n}) ->
        {u : unit | value <= other} @@ ghost total;
    } -> minimum

val minimum : unit -> minimum @@ total

type evidence =
  | First of { proof : unit @@ ghost }
  | Payload of int
  | Second of { proof : unit @@ ghost }

val first : unit -> evidence @@ total
val second : unit -> evidence @@ total
val copy : evidence -> evidence @@ total
