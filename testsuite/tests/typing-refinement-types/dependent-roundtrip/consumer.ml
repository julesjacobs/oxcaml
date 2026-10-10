type local_addition =
  (x : int) ->
  (y : int) ->
  { z : int | Producer.equal z (Producer.add x y) }

let coerce (functions : Producer.addition list) : local_addition list =
  functions
