(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
 }
*)

(* A type parameter that occurs only in a refinement predicate is invariant
   (but not injective).  It used to be phantom, so [:>] turned a law proved
   at [int] into the same law at [bool]. *)

module type Q = sig
  type ('a : immutable_data) t : immutable_data
  val empty : ('a : immutable_data). 'a t @@ total immutable
  val is_empty : 'a t -> bool @@ total
  val law_int : unit -> {u : unit | is_empty (empty : int t)} @@ total
  val law_all :
    ('a : immutable_data). unit -> {u : unit | is_empty (empty : 'a t)} @@ total
end;;
[%%expect{|
module type Q =
  sig
    type ('a : immutable_data) t : immutable_data
    val empty : ('a : immutable_data). 'a t @@ total immutable
    val is_empty : ('a : immutable_data). 'a t -> bool @@ total
    val law_int : unit -> {u : unit | is_empty (empty : int t)} @@ total
    val law_all : unit -> {u : unit | is_empty (empty : 'a t)} @@ total
  end
|}]

(* The fact at [bool] does not follow from the law at [int]. *)
module Direct (Q : Q) = struct
  type ('a : immutable_data) law =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let check (x : int law) : {b : bool | b} =
    match x with Law _ -> Q.is_empty (Q.empty : bool Q.t)
end;;
[%%expect{|
Line 5, characters 26-57:
5 |     match x with Law _ -> Q.is_empty (Q.empty : bool Q.t)
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 40-41:
4 |   let check (x : int law) : {b : bool | b} =
                                            ^
  The refinement is stated here.
|}]

(* Before the fix this was accepted, and [check] verified. *)
module Coerce (Q : Q) = struct
  type ('a : immutable_data) law =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let check (x : int law) : {b : bool | b} =
    match (x :> bool law) with Law _ -> Q.is_empty (Q.empty : bool Q.t)
end;;
[%%expect{|
Line 5, characters 10-25:
5 |     match (x :> bool law) with Law _ -> Q.is_empty (Q.empty : bool Q.t)
              ^^^^^^^^^^^^^^^
Error: Type "int law" is not a subtype of "bool law"
|}]

(* The same through a record field, an unboxed constructor and a kind
   annotation. *)
module Record (Q : Q) = struct
  type ('a : immutable_data) law =
    { n : int; p : {u : unit | Q.is_empty (Q.empty : 'a Q.t)} }
  let cast (x : int law) = (x :> bool law)
end;;
[%%expect{|
Line 4, characters 27-42:
4 |   let cast (x : int law) = (x :> bool law)
                               ^^^^^^^^^^^^^^^
Error: Type "int law" is not a subtype of "bool law"
|}]

module Unboxed (Q : Q) = struct
  type ('a : immutable_data) law : immediate =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)} [@@unboxed]
  let cast (x : int law) = (x :> bool law)
end;;
[%%expect{|
Line 4, characters 27-42:
4 |   let cast (x : int law) = (x :> bool law)
                               ^^^^^^^^^^^^^^^
Error: Type "int law" is not a subtype of "bool law"
|}]

(* A variance annotation is checked against predicate occurrences, both on
   the definition and when an abstract type is declared covariant. *)
module Annotated (Q : Q) = struct
  type (+'a : immutable_data) law =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
end;;
[%%expect{|
Lines 2-3, characters 2-53:
2 | ..type (+'a : immutable_data) law =
3 |     Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is invariant.
|}]

module Sealed (Q : Q) = struct
  module M : sig
    type (-'a : immutable_data) law
  end = struct
    type ('a : immutable_data) law = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  end
end;;
[%%expect{|
Lines 4-6, characters 8-5:
4 | ........struct
5 |     type ('a : immutable_data) law = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
6 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immutable_data) law =
               {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
         end
       is not included in
         sig type (-'a : immutable_data) law end
       Type declarations do not match:
         type ('a : immutable_data) law =
             {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
       is not included in
         type (-'a : immutable_data) law
       Their variances do not agree.
|}]

(* The same for a variable reached through a constrained parameter. *)
module Constrained (Q : Q) = struct
  type +'a law = Law of {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
    constraint 'a = 'b list
end;;
[%%expect{|
Lines 2-3, characters 2-27:
2 | ..type +'a law = Law of {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
3 |     constraint 'a = 'b list
Error: In the definition
         "type +'a law = Law of {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
           constraint 'a = 'b list"
       the type variable "'b" has a variance that
       is not reflected by its occurrence in type parameters.
       It was expected to be covariant, but it is invariant.
|}]

(* Predicates are compared without their types, so predicate occurrences
   must not make a parameter injective. *)
module Injective (Q : Q) = struct
  type (!'a : immutable_data) law =
    {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
end;;
[%%expect{|
Lines 2-3, characters 2-46:
2 | ..type (!'a : immutable_data) law =
3 |     {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is invariant.
|}]

(* An invariant parameter is not generalized by the relaxed value
   restriction. *)
module Weak (Q : Q) = struct
  type ('a : immutable_data) law =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let make () : 'a law = Law (Q.law_all ())
  let l = make ()
  let use : int law = l
end;;
[%%expect{|
module Weak :
  functor (Q : Q) ->
    sig
      type ('a : immutable_data) law =
          Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
      val make : ('a : immutable_data). unit -> 'a law
      val l : int law
      val use : int law
    end
|}]

(* The same through an abbreviation: [put] and [get] share one weak
   variable instead of being polymorphic, which would let one cell store a
   law at [int] and return it at [bool].  (An abbreviation still unifies
   [int w] with [bool w], because predicates are compared without their
   types; that is a separate hole.) *)
module Cell (Q : Q) = struct
  type ('a : immutable_data) w = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let cell () =
    let r : 'a w option ref = {contents = None} in
    (fun (x : 'a w) -> r := Some x), (fun () : 'a w option -> !r)
  let put, get = cell ()
end;;
[%%expect{|
module Cell :
  functor (Q : Q) ->
    sig
      type ('a : immutable_data) w =
          {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
      val cell :
        ('a : immutable_data). unit -> ('a w -> unit) * (unit -> 'a w option)
      val put : '_weak1 w -> unit
      val get : unit -> '_weak1 w option
    end
|}]

(* Coercing a law to itself, and covariant parameters next to a
   predicate-only one, are unaffected. *)
module Unaffected (Q : Q) = struct
  type (+'b, 'a : immutable_data) law =
    Law of 'b * {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let same (x : ([`A], int) law) = (x :> ([`A | `B], int) law)
end;;
[%%expect{|
module Unaffected :
  functor (Q : Q) ->
    sig
      type ('b, 'a : immutable_data) law =
          Law of 'b * {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
      val same : ([ `A ], int) law -> ([ `A | `B ], int) law
    end
|}]
