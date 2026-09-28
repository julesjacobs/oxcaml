val signed : Bigint.t -> Bigint.t @@ total
val signed_def : (value : Bigint.t) ->
  {u : unit | signed value ===
    Bigint.sub
      (Bigint.modulo (Bigint.add value 4611686018427387904Z) 9223372036854775808Z)
      4611686018427387904Z} @@ total
val range : (value : int) ->
  {u : unit | -4611686018427387904Z <= Bigint.of_int value
    && Bigint.of_int value <= 4611686018427387903Z} @ ghost @@ total
val add : (left : int) -> (right : int) ->
  {u : unit | Bigint.of_int (left + right) =
    signed (Bigint.add (Bigint.of_int left) (Bigint.of_int right))} @ ghost @@ total
val sub : (left : int) -> (right : int) ->
  {u : unit | Bigint.of_int (left - right) =
    signed (Bigint.sub (Bigint.of_int left) (Bigint.of_int right))} @ ghost @@ total
val less_equal : (left : int) -> (right : int) ->
  {u : unit | (left <= right) = (Bigint.of_int left <= Bigint.of_int right)}
  @ ghost @@ total
