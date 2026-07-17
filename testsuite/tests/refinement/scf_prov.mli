module Make (X : sig val cap : int end) : sig
  val cap : int
  val f : int{ _ = cap } -> int
  val g : unit -> int{ _ = cap }
end
