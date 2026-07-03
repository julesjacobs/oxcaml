(* Auxiliary module for stamp_collide.ml: kept token-for-token
   identical in SHAPE to collide_b.ml so that the two units' dependent
   binders are minted at the same stamps by their (separate) compiler
   runs -- .cmi-marshalled Scoped stamps collide by construction. *)

let dep : (x : int) -> {v:int | v = x} = fun x -> assume_ x

let dep2 : (p : int) -> (q : int) -> {v:int | v = p + q} =
  fun p q -> assume_ (p + q)
