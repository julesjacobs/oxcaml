module type SIG = sig
  val cap : int
  val f : int{ _ = cap } -> int
  val g : unit -> int{ _ = cap }
end
module R1 : SIG
module R2 : SIG
