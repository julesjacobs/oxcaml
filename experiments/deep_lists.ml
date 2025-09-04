module type S = sig
  type t : value with (int list list list)
end

module M : S = struct
  type t = int list list list
end

