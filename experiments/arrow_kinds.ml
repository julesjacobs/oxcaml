module type S = sig
  type t : value
end

module M : S = struct
  type t = int -> int
end

