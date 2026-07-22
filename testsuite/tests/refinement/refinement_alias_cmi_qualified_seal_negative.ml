module Left = Refinement_alias_cmi_left
module Right = Refinement_alias_cmi_right

(* Keep both an unqualified [p] and a qualified [M.p] in the seal.  If an
   unresolved qualified reference were reduced to [Path.last], this would
   incorrectly identify the two provider facts. *)
module Rejected : sig
  val p : bool
  module M : sig val p : bool end
  val law : unit{ M.p = true }
end = struct
  let p = Left.p
  module M = Right
  let law : unit{ p = true } = Left.proof
end

