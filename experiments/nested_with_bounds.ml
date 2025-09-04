module type S = sig
  type t : value with (int * (int * int ref))
end

module M : S = struct
  type t = int * (int * int ref)
end

