(* TEST
 expect;
*)

(* SOUNDNESS CAMPAIGN — Family 4 (persistence / substitution under adversarial
   in-memory module graphs).

   Cross-compilation-unit .cmi round-trips are exercised by the refinement/
   persistence_*.ml tests. Here we stress the same substitution, binder-stamp
   freshening, and alpha-equality machinery WITHIN a single unit via functor
   towers, shadowed names, and same-name-different-stamp collisions — the paths
   that rewrite predicate reference-heads and freshen binder stamps. A
   refinement that survives these transforms must remain structurally intact
   (equal predicates stay equal; a shadowed/renamed reference must not silently
   equate two different refinements, and must not lose its predicate).

   RESULT (no finding): the substitution / alpha-equality / reference-head
   machinery holds. P1 preserves the functor-result refinement (impl mismatch
   rejected). P2 correctly treats two alpha-equal predicates as EQUAL. P3 keeps
   two same-named nested [t]s with distinct predicates DISTINCT (clash). P4
   keeps S1's and S2's predicates distinct across the functor: on the
   integrated tree the [_ < 0] annotation on an [_ > 0] value is now the
   emitted VC obligation, which fails not-proved (upgraded from the first
   sweep's pending-VC accept) — again confirming no predicate collapse. No
   distinct predicates were merged and no predicate was lost. *)

(* P1: functor whose result signature carries a refinement; instantiate twice
   and confirm the refinement survives instantiation (substitution over the
   value/type graph must carry the predicate). *)
module F (X : sig type t = int end) : sig
  type t = X.t
  val pos : t{ _ > 0 }
end = struct
  type t = X.t
  let pos = 1
end
module A = F (struct type t = int end)
module B = F (struct type t = int end)
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = X.t
6 |   let pos = 1
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t val pos : int end
       is not included in
         sig type t = X.t val pos : t{ (app[Stdlib!.>] _ 0) } end
       Values do not match:
         val pos : int
       is not included in
         val pos : t{ (app[Stdlib!.>] _ 0) }
       The type "int" is not compatible with the type "t{ (app[Stdlib!.>] _ 0) }"
|}]

(* P2: shadowed binder names inside two predicates that are otherwise identical
   — alpha-equality must treat them as equal (structural), not diverge on the
   shadow stamp. *)
type s1 = int{ (fun x -> x = _) 1 }
type s2 = int{ (fun x -> x = _) 1 }
let coerce (v : s1) = (v : s2)
[%%expect {|
type s1 = int{ ((fun x -> (app[Stdlib!.=] x _)) 1) }
type s2 = int{ ((fun x -> (app[Stdlib!.=] x _)) 1) }
val coerce : s1 -> s2 = <fun>
|}]

(* P3: same source name, different stamps: two [t]s in nested modules, each
   refined; the outer must not confuse the inner's reference head. *)
module Outer = struct
  type t = int{ _ > 0 }
  module Inner = struct
    type t = int{ _ < 0 }
  end
end
let bad (x : Outer.t) = (x : Outer.Inner.t)
[%%expect {|
module Outer :
  sig
    type t = int{ (app[Stdlib!.>] _ 0) }
    module Inner : sig type t = int{ (app[Stdlib!.<] _ 0) } end
  end
Line 7, characters 25-26:
7 | let bad (x : Outer.t) = (x : Outer.Inner.t)
                             ^
Error: The value "x" has type "Outer.t" = "int{ (app[Stdlib!.>] _ 0) }"
       but an expression was expected of type
         "Outer.Inner.t" = "int{ (app[Stdlib!.<] _ 0) }"
|}]

(* P4: two signatures with same-named refined field, distinct predicates; the
   [_ < 0] annotation on the [_ > 0] value now emits a VC that fails not-proved
   (no silent unification). *)
module type S1 = sig val v : int{ _ > 0 } end
module type S2 = sig val v : int{ _ < 0 } end
module M (X : S1) (Y : S2) = struct
  let same = (X.v : int{ _ < 0 })
end
[%%expect {|
module type S1 = sig val v : int{ (app[Stdlib!.>] _ 0) } end
module type S2 = sig val v : int{ (app[Stdlib!.<] _ 0) } end
Line 4, characters 13-33:
4 |   let same = (X.v : int{ _ < 0 })
                 ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]
