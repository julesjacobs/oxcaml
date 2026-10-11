(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Siblings of the abstract-consumer knot (T9, "open route"): the same shape
   -- an abstract type equal to a function that consumes the matched type,
   applied by an exported total function -- reached through a functor
   parameter, a first-class module, a private type, an [include], a recursive
   module and a polymorphic variant behind abstraction. Total code may match
   such a type only when the hidden component's kind says it is logical. See
   totality_hidden_types.ml (the negative route) and
   totality_matchable_attribute.ml (the kind). *)

(* A functor parameter's abstract type, reached POSITIVELY in [Roll of u] and
   consumed by the parameter's total [app]. Matching [X.t] ties the knot. *)
module Functor_consumer (X : sig
  type u
  type t = Roll of u
  val app : u -> t -> int @@ total
end) = struct
  let (use @ total) (x : X.t) = match x with X.Roll _ -> 0
end;;
[%%expect{|
Line 6, characters 45-53:
6 |   let (use @ total) (x : X.t) = match x with X.Roll _ -> 0
                                                 ^^^^^^^^
Error: The match on a value whose type is not logical (X.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 6, characters 20-58
         which is expected to be "total".
|}]

(* With [mod logical] on the parameter's abstract type, the client may match
   the container: the functor's user must prove the kind at the argument. *)
module Functor_consumer_ok (X : sig
  type u : value mod logical
  type t = Roll of u
end) = struct
  let (use @ total) (x : X.t) = match x with X.Roll _ -> 0
end;;
[%%expect{|
module Functor_consumer_ok :
  functor (X : sig type u : value mod logical type t = Roll of u end) ->
    sig val use : X.t -> int end
|}]

(* A first-class module hiding the same knot: unpacking brings [u] into scope
   and [app] can consume it, so total code may not unpack it. *)
module type Knot_sig = sig
  type u
  type t = Roll of u
  val app : u -> t -> int @@ total
end
module First_class_consumer = struct
  let (use @ total) (module M : Knot_sig) = let _ = M.app in ()
end;;
[%%expect{|
module type Knot_sig =
  sig type u type t = Roll of u val app : u -> t -> int @@ total end
Line 7, characters 28-29:
7 |   let (use @ total) (module M : Knot_sig) = let _ = M.app in ()
                                ^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 7, characters 20-63
         which is expected to be "total".
|}]

(* A private type over the abstract consumer: matching deconstructs it, which
   is enough to extract the hidden function, so it is rejected too. *)
module Private_consumer = struct
  module M : sig
    type u
    type t = private Roll of u
    val app : u -> t -> int @@ total
    val mk : u -> t @@ total
  end = struct
    type u = t -> int
    and t = Roll of u
    let app f x = f x
    let mk f = Roll f
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 13, characters 45-53:
13 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                  ^^^^^^^^
Error: The match on a value whose type is not logical (M.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 13, characters 20-58
         which is expected to be "total".
|}]

(* [include] brings the abstract consumer's declarations into a new module;
   the knot is unchanged, so matching the included [t] is rejected. *)
module Knot_impl : Knot_sig = struct
  type u = t -> int
  and t = Roll of u
  let app f x = f x
end
module Include_consumer = struct
  include Knot_impl
  let (use @ total) (x : t) = match x with Roll _ -> 0
end;;
[%%expect{|
module Knot_impl : Knot_sig
Line 8, characters 43-49:
8 |   let (use @ total) (x : t) = match x with Roll _ -> 0
                                               ^^^^^^
Error: The match on a value whose type is not logical (Knot_impl.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 8, characters 20-54
         which is expected to be "total".
|}]

(* A recursive module whose signature hides the abstract type [u]. The module
   itself defines (it exports no total consumer, so the recursive-module
   totality guard does not fire), but total code still may not match [A.t],
   because [A.u] is hidden and could be a consumer of [A.t]. *)
module rec A : sig
  type u
  type t = Roll of u
  val mk : (A.t -> int) -> u
end = struct
  type u = A.t -> int
  type t = Roll of u
  let mk f = f
end;;
[%%expect{|
module rec A : sig type u type t = Roll of u val mk : (A.t -> int) -> u end
|}]

module Recmod_consumer = struct
  let (use @ total) (x : A.t) = match x with A.Roll _ -> 0
end;;
[%%expect{|
Line 2, characters 45-53:
2 |   let (use @ total) (x : A.t) = match x with A.Roll _ -> 0
                                                 ^^^^^^^^
Error: The match on a value whose type is not logical (A.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 20-58
         which is expected to be "total".
|}]

(* A polymorphic variant behind abstraction. Total code cannot match a
   structural (polymorphic-variant) value directly, and it cannot match a
   container holding an abstract [u] whose manifest is a poly variant either:
   [u] is not declared logical, so matching [t] is rejected, even
   though this particular [u] is benign. *)
module Poly_variant_consumer = struct
  module M : sig
    type u
    type t = Roll of u
    val a : u
  end = struct
    type u = [ `A | `B ]
    type t = Roll of u
    let a = `A
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 11, characters 45-53:
11 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                  ^^^^^^^^
Error: The match on a value whose type is not logical (M.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 11, characters 20-58
         which is expected to be "total".
|}]

(* Forging [mod logical] on a poly-variant consumer is refused at the
   definition site: the manifest reaches the type negatively through the
   arrow inside the variant. *)
module Poly_variant_forge = struct
  type t : value mod logical = Roll of [ `F of (t -> int) ]
end;;
[%%expect{|
Line 2, characters 2-59:
2 |   type t : value mod logical = Roll of [ `F of (t -> int) ]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value non_float mod immutable
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod logical
         because of the annotation on the declaration of the type t.
       It is not logical: t is recursive but not [@@inductive].
|}]

(* The kind assumed on a functor parameter is discharged at application:
   applying [Functor_consumer_ok] to a module whose [u] hides a negative
   recursion is rejected, because the argument's [u] is not logical. *)
module Bad_arg = struct
  type u = t -> int
  and t = Roll of u
end;;
[%%expect{|
module Bad_arg : sig type u = t -> int and t = Roll of u end
|}]

module Applied_bad = Functor_consumer_ok (Bad_arg);;
[%%expect{|
Line 1, characters 21-50:
1 | module Applied_bad = Functor_consumer_ok (Bad_arg);;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type u = t -> int and t = Bad_arg.t = Roll of u end
     is not included in sig type u : value mod logical type t = Roll of u end
     Type declarations do not match:
       type u = t -> int
     is not included in
       type u : value mod logical
     The kind of the first is value non_float mod aliased immutable
       because it's a function type.
     But the kind of the first must be a subkind of value mod logical
       because of the definition of u at line 2, characters 2-28.
     The first is not logical: Bad_arg.t is recursive but not [@@inductive].
|}]

(* Legitimate total code over an abstract type is still accepted: a functional
   map whose kind says it is logical when its payload is (an inductive tree) may
   be a component of a matched container. *)
module Legit_total = struct
  module M : sig
    type 'a map : value mod logical with 'a
    type t = Wrap of int map
    val empty : int map
  end = struct
    type 'a map = Leaf | Node of 'a map * int * 'a * 'a map [@@inductive]
    type t = Wrap of int map
    let empty = Leaf
  end
  let (use @ total) (x : M.t) = match x with M.Wrap _ -> 0
end;;
[%%expect{|
module Legit_total :
  sig
    module M :
      sig
        type 'a map : value mod logical with 'a
        type t = Wrap of int map
        val empty : int map
      end
    val use : M.t -> int
  end
|}]
