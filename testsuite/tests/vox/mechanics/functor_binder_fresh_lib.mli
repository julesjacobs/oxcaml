module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end
module type S = sig
  type elt
  val leq : (a : elt) -> (b : elt) -> bool{ _ = true -> not (b < a) }
end
module Make : functor (O : ORD) -> S with type elt = O.t
