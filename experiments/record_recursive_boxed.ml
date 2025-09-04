module type S = sig
  type t : value with t list
end

module M : S = struct
  type t = { tl : t list }
end

