module type SIG = sig
  val cap : int
  val f : int{ _ = cap } -> int
  val g : unit -> int{ _ = cap }
end
val m1 : (module SIG)
val m2 : (module SIG)
