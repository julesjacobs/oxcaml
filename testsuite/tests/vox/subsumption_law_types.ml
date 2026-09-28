(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Stage 2: predicates are compared together with their types
   (polymorphism-definitions.md item 2, DESIGN.md 2.7, 3.2).  Today a law
   proved at int Q.t is accepted as the law at bool Q.t by unification,
   signature inclusion, type-declaration equality, and (found while writing
   this test, DESIGN.md 8 W1) a refined result position, where
   introduce_refinement cancels the obligation with the type-blind
   Ctype.is_equal.  After stage 2 each is rejected: by the verifier where the
   predicates are then compared semantically (each side at its own sorts,
   which cannot be related), and by the type checker where the relation stays
   syntactic.  Each block is the output after stages 1-4. *)

(* The quantifier of law_all is not printed: its variable occurs only in
   the predicate (polymorphism-definitions.md item 1).  Currently: the same. *)
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

(* [stage 2] Unification.  After stage 2 the types are no longer equal:
   unification relates the predicates' node types and rejects the law at
   int where the law at bool is expected.  (The design predicted a failed
   proof through the implicit adapter; the type error comes first.)
   Currently: accepted, with no obligation ("module Unify : functor (Q : Q) ->
   sig val use_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
   end"). *)
module Unify (Q : Q) = struct
  let use_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)} =
    Q.law_int
end;;
[%%expect{|
Line 3, characters 4-13:
3 |     Q.law_int
        ^^^^^^^^^
Error: The value "Q.law_int" has type
         "unit -> {u : unit | Q.is_empty (Q.empty : int Q.t)}"
       but an expression was expected of type
         "unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}"
       Type "int" is not compatible with type "bool"
|}]

(* [stage 2] A refined result position (W1).  introduce_refinement no longer
   cancels the introduction, so the verifier checks it.
   Currently: accepted without any solver query (checked with
   -dsmt-resources); "let v = Q.law_int () in v" is checked and fails. *)
module Result (Q : Q) = struct
  let use_bool () : {u : unit | Q.is_empty (Q.empty : bool Q.t)} =
    Q.law_int ()
end;;
[%%expect{|
Line 3, characters 4-16:
3 |     Q.law_int ()
        ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 32-63:
2 |   let use_bool () : {u : unit | Q.is_empty (Q.empty : bool Q.t)} =
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* [stage 2 + 3] Signature inclusion.  The predicates are alpha-equal but
   their types differ, so moregen defers them and the verifier fails.
   Currently: accepted ("module Moregen : functor (Q : Q) -> sig val law_bool
   : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)} end"). *)
module Moregen (Q : Q) : sig
  val law_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
end = struct
  let law_bool = Q.law_int
end;;
[%%expect{|
Line 4, characters 6-14:
4 |   let law_bool = Q.law_int
          ^^^^^^^^
Error: The value "law_bool" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 2, characters 37-68:
2 |   val law_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* [stage 2] Type declarations stay syntactic, now including predicate
   types.
   Currently: accepted ("module Eqtype : functor (Q : Q) -> sig type w =
   {u : unit | Q.is_empty (Q.empty : bool Q.t)} end"). *)
module Eqtype (Q : Q) : sig
  type w = {u : unit | Q.is_empty (Q.empty : bool Q.t)}
end = struct
  type w = {u : unit | Q.is_empty (Q.empty : int Q.t)}
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type w = {u : unit | Q.is_empty (Q.empty : int Q.t)}
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type w = {u : unit | Q.is_empty (Q.empty : int Q.t)} end
       is not included in
         sig type w = {u : unit | Q.is_empty (Q.empty : bool Q.t)} end
       Type declarations do not match:
         type w = {u : unit | Q.is_empty (Q.empty : int Q.t)}
       is not included in
         type w = {u : unit | Q.is_empty (Q.empty : bool Q.t)}
       The type "{u : unit | Q.is_empty (Q.empty : int Q.t)}"
       is not equal to the type "{u : unit | Q.is_empty (Q.empty : bool Q.t)}"
       Type "int" is not equal to type "bool"
|}]

(* [unchanged] The same law at the same type is still accepted, without a
   proof obligation.  Currently: accepted. *)
module Same (Q : Q) : sig
  val law : unit -> {u : unit | Q.is_empty (Q.empty : int Q.t)}
end = struct
  let law = Q.law_int
end;;
[%%expect{|
module Same :
  functor (Q : Q) ->
    sig val law : unit -> {u : unit | Q.is_empty (Q.empty : int Q.t)} end
|}]

(* [stage 2] A polymorphic law whose type variable occurs only in the
   predicate: moregen now instantiates it through the paired predicate types
   ('a := bool), so the predicates are equal and no proof is needed.
   Currently: accepted too, but only because predicate types are ignored. *)
module Instantiate (Q : Q) : sig
  val law_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
end = struct
  let law_bool = Q.law_all
end;;
[%%expect{|
module Instantiate :
  functor (Q : Q) ->
    sig
      val law_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
    end
|}]

(* [stage 2] The same through unification: an ascription pins the lemma's
   instance.  Currently: accepted (type-blindly). *)
module Pinned (Q : Q) = struct
  let use_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)} =
    Q.law_all
end;;
[%%expect{|
module Pinned :
  functor (Q : Q) ->
    sig
      val use_bool : unit -> {u : unit | Q.is_empty (Q.empty : bool Q.t)}
    end
|}]

(* W8: a type parameter that occurs only in a refinement predicate is
   invariant (typedecl_variance.ml, fixed on trunk separately), so :> cannot
   convert a law at int into a law at bool. *)
module Phantom_law (Q : Q) = struct
  type ('a : immutable_data) law =
    Law of {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
  let cast (x : int law) = (x :> bool law)
end;;
[%%expect{|
Line 4, characters 27-42:
4 |   let cast (x : int law) = (x :> bool law)
                               ^^^^^^^^^^^^^^^
Error: Type "int law" is not a subtype of "bool law"
|}]
