module type S = sig
  type t : value with (int * int ref)
end

module M : S = struct
  type t = int * int ref
end

