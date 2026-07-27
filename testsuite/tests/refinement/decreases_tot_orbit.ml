(* The measure has to be read at the arguments the call actually passes.
   This orbit -- [(3, 0) -> (5, 1) -> (3, 0)] -- never ends, and it is
   accepted as total by any reading that lets one position's argument be
   rewritten by another position's. *)
let (expects_total @ total) (f @ total) = f

let[@vox.decreases a] rec orbit (a : int) (b : int) : int =
  if (a = 3 && b = 0) || (a = 5 && b = 1)
  then
    orbit
      (if a = 3 then (if b = 0 then 5 else 0) else (if b = 1 then 3 else 1))
      (if b = 0 then 1 else 0)
  else 0

let total_use () = expects_total orbit
