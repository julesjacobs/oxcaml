(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Behaviour that must not change with stages 1-4.  Everything accepted here
   is accepted today without any inclusion obligation, and must stay so (no
   solver query for syntactically equal refinements, DESIGN.md 2.1 (Eq)).
   Everything rejected here stays rejected with the same message.  Each
   block's output is today's output, which is also the intended output. *)

(* Alpha-equivalent refinements: accepted, no obligation. *)
module Alpha : sig val f : (x : int) -> {r : int | r >= x} end = struct
  let f (y : int) : {s : int | s >= y} = y
end;;
[%%expect{|
module Alpha : sig val f : (x : int) -> {r : int | r >= x} end
|}]

(* A polymorphic implementation instantiated at a refined type on both
   sides. *)
module Pos_id : sig val pos_id : {x : int | x > 0} -> {x : int | x > 0} end =
struct
  let pos_id x = x
end;;
[%%expect{|
module Pos_id : sig val pos_id : {x : int | x > 0} -> {x : int | x > 0} end
|}]

(* A dependent functor argument with equal refinements (the pattern of
   typing-refinement-types/dependent-functor-application.ml). *)
module type Pred = sig val holds : int -> bool @@ total end
module Use (P : Pred) = struct
  let accept (x : {x : int | P.holds x}) : {y : int | P.holds y} = x
end
module Positive = struct let (holds @ total) (x : int) = x > 0 end
module Applied = Use (Positive);;
[%%expect{|
module type Pred = sig val holds : int -> bool @@ total end
module Use :
  functor (P : Pred) ->
    sig val accept : {x : int | P.holds x} -> {y : int | P.holds y} end
module Positive : sig val holds : int -> bool end
module Applied :
  sig
    val accept : {x : int | Positive.holds x} -> {y : int | Positive.holds y}
  end
|}]

(* The refine_ re-export idiom keeps working (it eta-expands and checks the
   contract; stage 4's (g :> T) is the allocation-free alternative). *)
module M = struct
  let (g @ total) (x : int) : {r : int | r > 0} = if x > 0 then x else 1
end
let reexport : int -> {r : int | r >= 0} = refine_ M.g;;
[%%expect{|
module M : sig val g : int -> {r : int | r > 0} end
val reexport : int -> {r : int | r >= 0} = <fun>
|}]

(* The implicit adapter where the expected type is pushed down (a tuple
   component, experiment e12). *)
let pair : (int -> {r : int | r >= 0}) * int = (M.g, 1);;
[%%expect{|
val pair : (int -> {r : int | r >= 0}) * int = (<fun>, 1)
|}]

(* A non-dependent refined function through List.map instantiates 'b with
   the refined type (experiment e4). *)
let mapped xs = List.map M.g xs;;
[%%expect{|
val mapped : int list -> {r : int | r > 0} list = <fun>
|}]

(* Ghost modes are compared as modes (ghost_subsumption.ml). *)
let ghost_coerce (p : int @ ghost -> int -> int) = (p :> int -> int -> int);;
[%%expect{|
val ghost_coerce : (int @ ghost -> int -> int) -> int -> int -> int = <fun>
|}]

(* Module type equality with equal refinements. *)
module type S1 = sig val f : int -> {r : int | r > 0} end
module Z : sig module type S = S1 end = struct
  module type S = sig val f : int -> {r : int | r > 0} end
end;;
[%%expect{|
module type S1 = sig val f : int -> {r : int | r > 0} end
module Z : sig module type S = S1 end
|}]

(* Type declarations stay syntactic: a manifest refinement must be equal. *)
module Y : sig type w = {u : int | u > 0} end = struct
  type w = {u : int | u >= 1}
end;;
[%%expect{|
Lines 1-3, characters 48-3:
1 | ................................................struct
2 |   type w = {u : int | u >= 1}
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type w = {u : int | u >= 1} end
       is not included in
         sig type w = {u : int | u > 0} end
       Type declarations do not match:
         type w = {u : int | u >= 1}
       is not included in
         type w = {u : int | u > 0}
       The type "{u : int | u >= 1}" is not equal to the type "{u : int | u > 0}"
|}]

(* Unification stays rigid (stage 5 is out of scope): the identity tests of
   type-formers.ml keep their errors. *)
type positive = {x : int | x > 0}
type nonnegative = {x : int | x >= 0}
let different : positive list = ([] : nonnegative list);;
[%%expect{|
type positive = {x : int | x > 0}
type nonnegative = {x : int | x >= 0}
Line 3, characters 32-55:
3 | let different : positive list = ([] : nonnegative list);;
                                    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "nonnegative list"
       but an expression was expected of type "positive list"
       Type "nonnegative" = "{x : int | x >= 0}" is not compatible with type
         "positive" = "{x : int | x > 0}"
|}]

(* Externals included into values keep their primitive coercion. *)
module Ext : sig val add : int -> int -> int end = struct
  external add : int -> int -> int = "%addint"
end;;
[%%expect{|
module Ext : sig val add : int -> int -> int end
|}]
