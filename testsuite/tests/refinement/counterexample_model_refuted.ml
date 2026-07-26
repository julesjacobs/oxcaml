(* No positive integer is its own difference, so this obligation is refuted
   and the assignment that refutes it is what the counterexample reports. *)
let refuted (x : int{ _ > 0 }) = (x - x : int{ _ > 0 })
