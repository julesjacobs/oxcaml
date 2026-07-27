(* Likewise for a partial operation: integer division keeps it partial. *)
let (expects_total @ total) (f @ total) = f

let[@vox.decreases n] rec halves (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else (n / 2) + halves (n - 1)

let total_use () = expects_total halves
