(* An erased parameter used from another unit. *)
let f (x : int @ erased) = 0
let use () = f (erased_ 1)
