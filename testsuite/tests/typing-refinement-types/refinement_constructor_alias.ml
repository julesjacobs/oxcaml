(* TEST
 flags = "-extension refinement_types";
 expect;
*)

module _ = struct
  module Source = struct
    type t = C
    let id (x : {x : t | match x with C -> true}) = x
  end

  module Alias : sig
    type t = Source.t = C
    val id :
      {x : t | match x with C -> true} ->
      {x : t | match x with C -> true}
  end = Source

  module Record_source = struct
    type t = { field : int }
    let id (x : {x : t | x.field = 0}) = x
  end

  module Record_alias : sig
    type t = Record_source.t = { field : int }
    val id : {x : t | x.field = 0} -> {x : t | x.field = 0}
  end = Record_source

  module Poly_source = struct
    type 'a t = C
    let id (x : {x : 'a t | match x with C -> true}) = x
  end

  module Poly_alias : sig
    type 'a t = 'a Poly_source.t = C
    val id :
      {x : 'a t | match x with C -> true} ->
      {x : 'a t | match x with C -> true}
  end = Poly_source
end;;
[%%expect{|
|}]

module _ = struct
  module First = struct type t = C end
  module Second = struct type t = C end

  let mismatch
      (x : {u : unit | match First.C with First.C -> true})
      : {u : unit | match Second.C with Second.C -> true} =
    x
end;;
[%%expect{|
Line 8, characters 4-5:
8 |     x
        ^
Error: The value "x" has type "{u : unit | match First.C with | First.C -> true}"
       but an expression was expected of type
         "{u : unit | match Second.C with | Second.C -> true}"
|}]

module _ = struct
  module First = struct type t = { field : int } end
  module Second = struct type t = { field : int } end

  let mismatch
      (x : {u : unit | ({First.field = 0}).First.field = 0})
      : {u : unit | ({Second.field = 0}).Second.field = 0} =
    x
end;;
[%%expect{|
Line 8, characters 4-5:
8 |     x
        ^
Error: The value "x" has type "{u : unit | { First.field = 0 }.First.field = 0}"
       but an expression was expected of type
         "{u : unit | { Second.field = 0 }.Second.field = 0}"
|}]

let escape_record_owner () =
  let module Local = struct type t = { field : int } end in
  let unit = () in
  (refine_ unit : {u : unit | ({Local.field = 0}).Local.field = 0})
;;
[%%expect{|
Line 4, characters 2-67:
4 |   (refine_ unit : {u : unit | ({Local.field = 0}).Local.field = 0})
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "Local"
|}]
