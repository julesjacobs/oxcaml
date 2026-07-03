(* Auxiliary module for stamp_collide.ml; see collide_a.ml.  Same
   shape, different binder names and a DIFFERENT second predicate. *)

let dep : (y : int) -> {v:int | v = y} = fun y -> assume_ y

let dep2 : (r : int) -> (s : int) -> {v:int | v = r * s} =
  fun r s -> assume_ (r * s)
