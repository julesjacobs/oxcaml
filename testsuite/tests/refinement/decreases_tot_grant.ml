(* A measure is what lets an integer recursion be total: structural recursion
   does not reach it, so without one the binding would be partial. *)
let (expects_total @ total) (f @ total) = f

let[@vox.decreases n] rec countdown (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else countdown (n - 1)

let total_use () = expects_total countdown
