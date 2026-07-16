(* TEST
 expect;
*)

(* SOUNDNESS CAMPAIGN — Family 2 (refinement rigidity bypass).

   The rigidity invariant: a bare value cannot silently acquire a refined type;
   a refined type meeting a bare type is a clash at every depth. The acceptance
   corpus (refinement-acceptance/rigid_unification.ml) covers direct nested
   unification. Here we attack the equality channels that do NOT go through
   Ctype.unify's refined-vs-bare guard: module sealing, private-type coercion,
   GADT type equalities, functor type aliasing, and abstract-type identity.

   A bare value emerging with a refined type WITHOUT a rejection or an
   obligation is a soundness finding (once VC discharge lands, that value would
   be trusted to satisfy the predicate with no proof). *)

(* C1: bare implementation behind a REFINED interface (plan's open question;
   vox1 rejects outright). A silent accept hands out int{_>0} for any int. *)
module M1 : sig
  type t = int{ _ > 0 }
end = struct
  type t = int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int end
       is not included in
         sig type t = int{ (app[Stdlib!.>] _ 0) } end
       Type declarations do not match:
         type t = int
       is not included in
         type t = int{ (app[Stdlib!.>] _ 0) }
       The type "int" is not equal to the type "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* C2: the reverse — refined implementation behind a BARE interface. This is
   the covariant direction the plan says MAY be dropped at a seal; recording
   the outcome for the record. *)
module M2 : sig
  type t = int
end = struct
  type t = int{ _ > 0 }
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int{ _ > 0 }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int{ (app[Stdlib!.>] _ 0) } end
       is not included in
         sig type t = int end
       Type declarations do not match:
         type t = int{ (app[Stdlib!.>] _ 0) }
       is not included in
         type t = int
       The type "int{ (app[Stdlib!.>] _ 0) }" is not equal to the type "int"
|}]

(* C3: bare value coerced into a refined private-abbreviation type. *)
module M3 : sig
  type t = private int{ _ > 0 }
  val make : int -> t
end = struct
  type t = int
  let make x = x
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = int
6 |   let make x = x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int val make : 'a -> 'a end
       is not included in
         sig
           type t = private int{ (app[Stdlib!.>] _ 0) }
           val make : int -> t
         end
       Type declarations do not match:
         type t = int
       is not included in
         type t = private int{ (app[Stdlib!.>] _ 0) }
       The type "int" is not equal to the type "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* C4: GADT propositional equality between bare and refined. If [Refl] can be
   built at type [(int, int{_>0}) eq], the refinement is coercible for free. *)
type (_, _) eq = Refl : ('a, 'a) eq
let bad_eq : (int, int{ _ > 0 }) eq = Refl
[%%expect {|
type (_, _) eq = Refl : ('a, 'a) eq
Line 2, characters 38-42:
2 | let bad_eq : (int, int{ _ > 0 }) eq = Refl
                                          ^^^^
Error: The constructor "Refl" has type "(int, int) eq"
       but an expression was expected of type
         "(int, int{ (app[Stdlib!.>] _ 0) }) eq"
       Type "int" is not compatible with type "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* C5: functor that aliases a refined param type to a bare type. *)
module type REF = sig type t = int{ _ > 0 } end
module F (X : sig type t = int end) : REF with type t = X.t = struct
  type t = X.t
end
[%%expect {|
module type REF = sig type t = int{ (app[Stdlib!.>] _ 0) } end
Line 2, characters 38-59:
2 | module F (X : sig type t = int end) : REF with type t = X.t = struct
                                          ^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = X.t
       is not included in
         type t = int{ (app[Stdlib!.>] _ 0) }
       The type "X.t" = "int" is not equal to the type
         "int{ (app[Stdlib!.>] _ 0) }"
|}]
