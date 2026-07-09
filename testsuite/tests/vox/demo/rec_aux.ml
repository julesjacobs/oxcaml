(* Auxiliary module for lean_records.ml: a record refinement with a
   field projection crosses the module boundary (the predicate's record
   path is remapped by Subst on import). *)

type wid = { w : int }

let one : wid{ _.w = 1 } = { w = 1 }
