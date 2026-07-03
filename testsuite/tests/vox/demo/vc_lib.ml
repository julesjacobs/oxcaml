(* Auxiliary module for lean_verify.ml: refined values and dependent
   functions cross the module boundary through the inferred signature. *)

let pos : {v:int | v > 0} = refine_ 3

let add : (a : int) -> (b : int) -> {c:int | c = a + b} =
  fun a b -> assume_ (a + b)
