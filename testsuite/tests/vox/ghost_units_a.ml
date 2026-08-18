(* A ghost parameter used from another unit. *)
let f (x : int @ ghost) = 0
let use () = f (ghost_ 1)
