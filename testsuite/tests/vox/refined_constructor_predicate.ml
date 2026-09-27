(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* In a refinement predicate a refined constructor argument or record field
   must already have its refined type; the error says how to get one. *)

type event = Find of {depth : int | depth >= 0} | Other
type box = { depth : {depth : int | depth >= 0} }
[%%expect{|
type event = Find of {depth : int | depth >= 0} | Other
type box = { depth : {depth : int | depth >= 0}; }
|}]

module Rejected = struct
  let[@def] (d @ total) (x : int) : int = if x >= 0 then x else 0
  let (f @ total) (x : int) : {e : event | e === Find (d x)} = Find (d x)
end
[%%expect{|
Line 3, characters 54-59:
3 |   let (f @ total) (x : int) : {e : event | e === Find (d x)} = Find (d x)
                                                          ^^^^^
Error: This expression has type "int" but an expression was expected of type
         "{depth : int | depth >= 0}"
Hint: in a refinement predicate,
this argument must already have its refined type.
A helper function whose result type is refined works,
for example "let f x : {y : int | y >= 0} = ...".
|}]

module Rejected_field = struct
  let[@def] (d @ total) (x : int) : int = if x >= 0 then x else 0
  let (f @ total) (x : int) : {b : box | b === { depth = d x }} =
    { depth = d x }
end
[%%expect{|
Line 3, characters 57-60:
3 |   let (f @ total) (x : int) : {b : box | b === { depth = d x }} =
                                                             ^^^
Error: This expression has type "int" but an expression was expected of type
         "{depth : int | depth >= 0}"
Hint: in a refinement predicate,
this argument must already have its refined type.
A helper function whose result type is refined works,
for example "let f x : {y : int | y >= 0} = ...".
|}]

module Accepted = struct
  let[@def] (d @ total) (x : int) : {depth : int | depth >= 0} =
    if x >= 0 then x else 0
  let (f @ total) (x : int) : {e : event | e === Find (d x)} = Find (d x)
end
[%%expect{|
module Accepted :
  sig
    val d : int -> {depth : int | depth >= 0}
    val d_def :
      (x : int) -> {u : unit | (d x) === (if x >= 0 then x else 0 : int)}
    val f : (x : int) -> {e : event | e === (Find (d x))}
  end
|}]
