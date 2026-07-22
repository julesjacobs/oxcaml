module Nested : sig
  val p : bool
  type 'a law = 'a{ let _ = _ in p = true }
  val proof : unit law
end

module Make (X : sig end) : sig
  val p : bool
  type 'a law = 'a{ let _ = _ in p = true }
  val proof : unit law
end

