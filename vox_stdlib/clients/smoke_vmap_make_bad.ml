(* NEGATIVE headline: a sign-flipped comparator that CLAIMS the ORD ordered
   contract is DISPROVED at its own definition -- the instantiation
   obligation is real and enforced ACROSS the .cmi.  [Make (BadOrd)] cannot
   be built on a comparator whose contract does not hold.  Expected: NOT the
   green path; compile FAILS with a disproof of BadOrd.compare's contract. *)

open Vmap_make

module BadOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then 1 else if x = y then 0 else (-1)  (* sign flipped *)
end

module M = Make (BadOrd)
