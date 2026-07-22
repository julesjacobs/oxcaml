module F (M : sig val v : int end) = struct
  let f = M.v
end

module X = struct
  let v = 1
end

module Y = struct
  let v = 2
end

module A = F (X)
module B = F (Y)

(* Distinct applications retain the declaration UID of [F.f], but their
   values are not interchangeable at a seal. *)
module Rejected : sig
  val g : int{ _ = A.f }
end = struct
  let g : int{ _ = B.f } = B.f
end
