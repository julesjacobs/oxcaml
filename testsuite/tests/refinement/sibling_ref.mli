val base : int
val g : int{ _ = base }

module type T = sig
  val cap : int
  val v : int{ _ = cap }
end

module Make : functor (X : sig val cap : int end) -> T
