(* smoke_viarray: forces each Viarray op contract to fire, plus the one
   "law" Viarray relies on -- the built-in theory's length-nonneg axiom.
   Verified against Viarray's own artifact (cmi + no VoxSig block). *)

(* [length]'s result refinement (_ = Iarray.length a) combined with the
   built-in nonneg axiom discharges 0 <= _.  Removing the axiom (or the
   contract) breaks this goal. *)
let length_nonneg : (a : int iarray) -> int{ 0 <= _ } =
  fun a -> Viarray.length a

(* [get]'s result refinement (_ = a.(i)) under the bounds precondition. *)
let get_eq : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
             -> int{ _ = a.(i) } =
  fun a i -> Viarray.get a i

(* [unsafe_get]: the bounds precondition is discharged statically (same
   contract as [get], no result refinement) -- exercising it forces the
   precondition VC. *)
let unsafe_head : (a : int iarray) -> (h : unit{ 0 < Iarray.length a })
                  -> int =
  fun a _ -> Viarray.unsafe_get a 0

(* [length] feeds a caller-side bounds proof: n = Iarray.length a, so
   n - 1 < Iarray.length a discharges [get]'s precondition when n > 0. *)
let last : (a : int iarray) -> (h : unit{ 0 < Iarray.length a })
           -> int{ _ = a.(Iarray.length a - 1) } =
  fun a _ ->
    let n = Viarray.length a in
    Viarray.get a (n - 1)
