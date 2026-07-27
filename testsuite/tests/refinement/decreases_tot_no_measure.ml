(* The same body without a measure: recursion alone leaves it partial. *)
let (expects_total @ total) (f @ total) = f

let rec countdown (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else countdown (n - 1)

let total_use () = expects_total countdown
