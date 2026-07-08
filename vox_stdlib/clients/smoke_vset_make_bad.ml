(* NEGATIVE: a sign-flipped comparator claiming ORD's ordered contract is
   DISPROVED at its own definition -- Vset_make's instantiation obligation
   is real and enforced across the .cmi, same as Vmap_make's. *)

open Vset_make

module BadOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then 1 else if x = y then 0 else (-1)
end

module S = Make (BadOrd)
