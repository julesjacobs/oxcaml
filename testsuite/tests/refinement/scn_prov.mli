module P : sig
  val basea : int
  val f : int{ _ = basea } -> int
end
module Q : sig
  val baseb : int
  val g : unit -> int{ _ = baseb }
end
