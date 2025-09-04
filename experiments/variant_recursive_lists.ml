module type S = sig
  type t : value with (int * t list)
end

module M : S = struct
  type t =
    | A of int
    | B of t list
end

