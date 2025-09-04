module A : sig 
  type t = A | B
  val v : t
end = struct
  type t = A | B
  let v = A
end

module B : sig
  type t = A.t
end = struct
  type t = A.t
end