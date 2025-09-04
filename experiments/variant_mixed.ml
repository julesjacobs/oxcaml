module type S1 = sig
  type t : value with int ref
end

module M1 : S1 = struct
  type t =
    | A of int ref
    | B of int
end

module type S2 = sig
  type t : immediate
end

module M2 : S2 = struct
  type t = C | D
end

