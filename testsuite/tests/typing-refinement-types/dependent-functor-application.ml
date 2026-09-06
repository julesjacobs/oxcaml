(* TEST
 flags = "-extension refinement_types";
 expect;
*)

module Direct_application : sig end = struct
  module type Map = sig
    type key
    type !+'a t
    val mem : key -> 'a t -> bool
    val mem : key @ immutable -> 'a t @ immutable -> bool @@ total

    module Refined : sig
      val find :
        ('a : value mod separable).
        (map : 'a t) -> {key : key | mem key map} -> 'a
        @@ total
    end
  end

  module Use (Maker : sig
    module Make (Order : sig type t end) : Map with type key = Order.t
  end) = struct
    module M = Maker.Make (struct type t = int end)

    let find :
      (map : int M.t) -> {key : int | M.mem key map} -> int =
      fun map key -> M.Refined.find map key
  end
end;;
[%%expect{|
module Direct_application : sig end
|}]
