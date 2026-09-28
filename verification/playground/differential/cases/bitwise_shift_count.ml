(* Rejected: a shift count outside [0, 63] gives an unspecified result. *)
let same (n : {n : int | n = 64}) : {b : bool | b} = (1 lsl n) = (1 lsl 64)
