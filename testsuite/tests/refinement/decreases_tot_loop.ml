(* A measure buys termination of the recursion and nothing else: a loop in
   the body leaves the function partial exactly as it would without one. *)
let (expects_total @ total) (f @ total) = f

let[@vox.decreases n] rec countdown (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else (while false do () done; countdown (n - 1))

let total_use () = expects_total countdown
