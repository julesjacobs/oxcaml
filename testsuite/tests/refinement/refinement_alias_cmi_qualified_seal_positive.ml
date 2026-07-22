module Left = Refinement_alias_cmi_left
module Right = Refinement_alias_cmi_right

module Accepted : sig
  val p : bool
  module M : sig val p : bool end
  val law : unit{ M.p = true }
end = struct
  let p = Left.p
  module M = Right
  let law : unit{ M.p = true } = M.proof
end
