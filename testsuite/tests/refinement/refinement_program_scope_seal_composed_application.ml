module Input = struct
  let value = 1
end

module Wrap (M : sig val value : int end) = struct
  let value = M.value
end

module F (M : sig val value : int end) = struct
  let out : int{ _ = M.value } = M.value
end

module C = F (Wrap (Input))

(* The implementation predicate contains [Wrap(Input).value], whose path has
   an application prefix.  It is a concrete projection, not an unprojected
   declaration, and inspecting it must not abort seal checking. *)
module Rejected : sig
  val out : int{ _ = C.out }
end = C
