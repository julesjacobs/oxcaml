module Global : sig
  val x : int
end

val value : int{ _ = Global.x }

val outer : int

module Nested : sig
  val inherited : int{ _ = outer }
  val base : int
  val sibling : int{ _ = base }
end
