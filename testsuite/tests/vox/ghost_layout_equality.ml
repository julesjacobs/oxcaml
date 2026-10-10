(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Proof : sig
  type t : void
  val reflexive : (x : t) @ ghost -> {u : unit | ghost_ (x === x)} @ ghost
    @@ total
end = struct
  type t = { number : int @@ ghost }
  let (reflexive @ total) (x : t @ ghost) :
      {u : unit | ghost_ (x === x)} @ ghost = ghost_ (refine_ ())
end;;
[%%expect{|
module Proof :
  sig
    type t : void
    val reflexive : (x : t) @ ghost -> {u : unit | ghost_ (x === x)} @ ghost
      @@ total
  end
|}]

module Rejected = struct
  type t = { number : int @@ ghost }
  let equal_at_runtime (x : t) (y : t) =
    let value = true in
    (assume_ value : {b : bool | b = (x === y)})
end;;
[%%expect{|
Line 5, characters 38-39:
5 |     (assume_ value : {b : bool | b = (x === y)})
                                          ^
Error: This expression has type "t" but an expression was expected of type
         "('a : value)"
       The layout of t is void
         because of the definition of t at line 2, characters 2-36.
       But the layout of t must be a value layout
         because unknown (please alert the Jane Street
                         compilers team with this message: logical equality operand).
|}]
