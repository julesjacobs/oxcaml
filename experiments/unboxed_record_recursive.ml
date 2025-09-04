module type S = sig
  type t : value with t list
end

module M : S = struct
  (* Unboxed product record recursive through a list *)
  type t = #{ tl : t list }
end

