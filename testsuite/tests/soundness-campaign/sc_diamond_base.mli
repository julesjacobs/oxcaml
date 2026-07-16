(* Base interface for the cross-.cmi diamond persistence probe. Carries a
   refined type alias, a sibling-reference refined value (predicate names a
   same-signature sibling [base]), and a functor whose result predicate names
   its own parameter — all of which must survive independent .cmi round-trips
   through two re-export paths and be recognized as EQUAL where they meet. *)

type pos = int{ _ > 0 }

val base : int
val g : int{ _ = base }

module type T = sig
  val cap : int
  val v : int{ _ = cap }
end

module Make : functor (X : sig val cap : int end) -> T
