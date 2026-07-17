(* Two different signatures that merely share the sibling value name "cap" --
   the honest-code accidental-collision case. *)
module P : sig
  val cap : int
  val f : int{ _ = cap } -> int
end
module Q : sig
  val cap : int
  val g : unit -> int{ _ = cap }
  val extra : bool
end
