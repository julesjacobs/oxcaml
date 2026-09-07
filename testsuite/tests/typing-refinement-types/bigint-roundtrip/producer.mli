type number = Bigint.t
type positive = {n : number | n > 0Z}
val next : number -> number @@ total
val next_def : (x : number) ->
  {u : unit | next x === Bigint.add x 1Z} @@ total
val huge : unit -> positive @@ total
