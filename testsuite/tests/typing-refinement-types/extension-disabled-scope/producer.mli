module type Parameter = sig
  val flag : bool
end

module Make (X : Parameter) : sig
  type t = { x : int | X.flag }
  val value : t
end
