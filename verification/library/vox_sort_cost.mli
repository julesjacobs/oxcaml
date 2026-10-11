(** Comparison budget for merge sort. For positive [size], [height size] is
    the least nonnegative exponent whose power of two covers [size]. *)
val height : Bigint.t -> Bigint.t @@ total
val height_def : (size : Bigint.t) ->
  {u : unit | height size ===
    (if size <= 1Z then 0Z
     else Bigint.add 1Z (height (Bigint.div (Bigint.add size 1Z) 2Z)))} @@ total

val budget : Bigint.t -> Bigint.t @@ total
val budget_def : (size : Bigint.t) ->
  {u : unit | budget size === Bigint.mul size (height size)} @@ total

val power : Bigint.t -> Bigint.t @@ total
val power_def : (depth : Bigint.t) ->
  {u : unit | power depth ===
    (if depth <= 0Z then 1Z
     else Bigint.mul 2Z (power (Bigint.sub depth 1Z)))} @@ total

val height_bound : (size : Bigint.t) ->
  {u : unit | 0Z <= height size && size <= power (height size)} @@ total
val height_minimal : (size : Bigint.t) ->
  {u : unit | if size > 1Z then
    0Z < height size && power (Bigint.sub (height size) 1Z) < size
    else height size = 0Z} @@ total

(** Proof support for splitting a machine-integer credit token. *)
val bounded_int : (bound : int) ->
  (amount : {n : Bigint.t | 0Z <= n && n <= Bigint.of_int bound}) ->
  {n : int | let amount = amount in
    Bigint.of_int n = amount && 0 <= n && n <= bound} @ ghost @@ total
