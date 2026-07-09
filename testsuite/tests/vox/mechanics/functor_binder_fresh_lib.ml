module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end
module type S = sig
  type elt
  val leq : (a : elt) -> (b : elt) -> bool{ _ = true -> not (b < a) }
end
module Make (O : ORD) = struct
  type elt = O.t
  let leq : (a : elt) -> (b : elt) -> bool{ _ = true -> not (b < a) } =
    fun a b -> let c = O.compare a b in c <= 0
end
