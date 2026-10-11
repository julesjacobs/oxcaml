(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* The cyclic-DATA variant of the T9 knot: instead of hiding a function that
   consumes the matched type, hide the type's own recursion behind an abstract
   type and build a cyclic value with [let rec], then loop by matching it in
   total code. It is closed by two rules:

   - a recursive type is logical, so total code may match it, only if it is
     [@@inductive]; and
   - [value_rec_check] treats constructors of [@@inductive] types as
     dereferences, so a cyclic value of an inductive type cannot be built.

   The two rules are mutually exclusive for a would-be looping consumer, so the
   route is not live. A [mod logical] annotation on a recursive, non-inductive
   type is refused where the type is defined. *)

(* [u] abstract, secretly [= t]; [t = Roll of u]. The equality is visible in
   the implementation, so a cyclic value could be built there -- but total code
   still cannot match [t], because [t] is recursive and not [@@inductive]. *)
module Cyclic_abstract = struct
  type u = t
  and t = Roll of u
  let rec x = Roll x
  let rec (loop @ total) (y : t) : {v : unit | false} =
    match y with Roll u -> loop u
  let _ = x
end;;
[%%expect{|
Line 6, characters 17-23:
6 |     match y with Roll u -> loop u
                     ^^^^^^
Error: The match on a value whose type is not logical (t is recursive but not [@@inductive]) is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 5-6, characters 25-33
         which is expected to be "total".
|}]

(* A directly recursive type cannot claim to be logical without [@@inductive]. *)
module Total_matchable_recursive = struct
  type t : value mod logical = Roll of t
  let (peek @ total) (y : t) : int = match y with Roll _ -> 0
end;;
[%%expect{|
Line 2, characters 2-40:
2 |   type t : value mod logical = Roll of t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod logical
         because of the annotation on the declaration of the type t.
       It is not logical: t is recursive but not [@@inductive].
|}]

(* The same cyclic route through a functor parameter's abstract type: total
   code inside the functor may not match [X.t]. *)
module Cyclic_functor (X : sig
  type u
  type t = Roll of u
  val of_u : u -> t @@ total
end) = struct
  let rec (loop @ total) (y : X.t) : {v : unit | false} =
    match y with X.Roll u -> loop (X.of_u u)
end;;
[%%expect{|
Line 7, characters 17-25:
7 |     match y with X.Roll u -> loop (X.of_u u)
                     ^^^^^^^^
Error: The match on a value whose type is not logical (X.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 6-7, characters 25-44
         which is expected to be "total".
|}]

(* A genuinely inductive type: total structural recursion is allowed, and
   [value_rec_check] forbids building a cyclic value, so no loop exists. *)
module Inductive_no_cycle = struct
  type t = Leaf | Node of t [@@inductive]
  let rec (count @ total) (y : t) : int = match y with Leaf -> 0 | Node z -> 1 + count z
  let good = Node (Node Leaf)
end;;
[%%expect{|
module Inductive_no_cycle :
  sig
    type t = Leaf | Node of t
    [@@inductive]
    val count : t -> int
    val good : t
  end
|}]

module Inductive_cyclic_rejected = struct
  type t = Leaf | Node of t [@@inductive]
  let rec bad = Node bad
end;;
[%%expect{|
Line 3, characters 16-24:
3 |   let rec bad = Node bad
                    ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
