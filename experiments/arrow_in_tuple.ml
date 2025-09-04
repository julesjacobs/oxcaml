module type S = sig
  type t : value with (int -> int)
end

module M : S = struct
  type t = (int -> int) * int ref
end

