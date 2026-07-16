(* TEST
 expect;
*)

(* Soundness guard for the sibling-reference boundary fix: pairing sibling
   value identifiers must ALIGN equal predicates across a signature boundary,
   never accept unequal ones.  Both inclusions below must still be REJECTED. *)

(* Same paired sibling, different predicates: [_ = base] vs [_ = base + 1]. *)
module type A = sig
  val base : int
  val g : int{ _ = base }
end

module type B = sig
  val base : int
  val g : int{ _ = base + 1 }
end

module Different (X : A) : B = X
[%%expect {|
module type A =
  sig val base : int val g : int{ (app[Stdlib!.=] _ global[base/289]) } end
module type B =
  sig
    val base : int
    val g : int{ (app[Stdlib!.=] _ (app[Stdlib!.+] global[base/298] 1)) }
  end
Line 11, characters 31-32:
11 | module Different (X : A) : B = X
                                    ^
Error: Signature mismatch:
       Modules do not match:
         sig
           val base : int
           val g : int{ (app[Stdlib!.=] _ global[base/289]) }
         end
       is not included in
         B
       Values do not match:
         val g : int{ (app[Stdlib!.=] _ global[base/289]) }
       is not included in
         val g :
           int{ (app[Stdlib!.=] _ (app[Stdlib!.+] global[base/289] 1)) }
       The type "int{ (app[Stdlib!.=] _ global[base/289]) }"
       is not compatible with the type
         "int{ (app[Stdlib!.=] _ (app[Stdlib!.+] global[base/289] 1)) }"
|}]

(* Bare implementation behind a refined interface must not be accepted. *)
module type C = sig
  val base : int
  val g : int
end

module type Refined = sig
  val base : int
  val g : int{ _ = base }
end

module Bare (X : C) : Refined = X
[%%expect {|
module type C = sig val base : int val g : int end
module type Refined =
  sig val base : int val g : int{ (app[Stdlib!.=] _ global[base/314]) } end
Line 11, characters 32-33:
11 | module Bare (X : C) : Refined = X
                                     ^
Error: Signature mismatch:
       Modules do not match:
         sig val base : int val g : int end
       is not included in
         Refined
       Values do not match:
         val g : int
       is not included in
         val g : int{ (app[Stdlib!.=] _ global[base/309]) }
       The type "int" is not compatible with the type
         "int{ (app[Stdlib!.=] _ global[base/309]) }"
|}]
