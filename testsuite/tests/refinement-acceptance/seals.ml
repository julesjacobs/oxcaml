(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: module seals                                *)
(*                                                                *)
(* plan.html "Equality and unification" + "How checking works" +  *)
(* roadmap "Seals": logical implication between two different      *)
(* refinements exists in exactly one place -- module sealing,      *)
(* where signature inclusion records a DIRECTED implication to     *)
(* prove instead of requiring equal predicates. A refined          *)
(* implementation behind a bare interface is an allowed COVARIANT  *)
(* drop; a bare implementation behind a refined interface is never *)
(* a silent accept (it is an open question whether it becomes a    *)
(* definition-tied obligation or a hard reject -- vox1 rejects).   *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=seal_conforming final=ACCEPT today=REJECT stable=no unlocks=seals
   Refined interface over a CONFORMING implementation: the impl
   [f x = x * x] genuinely satisfies [f _ >= 0].
   FINAL: accepts (seal implication [x * x >= 0] discharged).
   TODAY: rejected with a signature mismatch -- the bare impl result
   [int] rigidly clashes with the refined interface result. *)
module Seal_conforming : sig
  val f : int -> int{ _ >= 0 }
end = struct
  let f (x : int) = x * x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : int) = x * x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int{ (app[Stdlib!.>=] _ 0) } end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int{ (app[Stdlib!.>=] _ 0) }
       The type "int -> int" is not compatible with the type
         "int -> int{ (app[Stdlib!.>=] _ 0) }"
       Type "int" is not compatible with type "int{ (app[Stdlib!.>=] _ 0) }"
|}]

(* @acc id=seal_nonconforming final=REJECT today=REJECT stable=no unlocks=seals
   Refined interface over a NON-conforming implementation: [f x =
   x - 1000] can be negative.
   FINAL: rejected with a seal-VERIFICATION error (implication
   [x - 1000 >= 0] not provable).
   TODAY: rejected with a signature mismatch -- same outcome, the
   message tightens to a seal VC failure. *)
module Seal_nonconforming : sig
  val f : int -> int{ _ >= 0 }
end = struct
  let f (x : int) = x - 1000
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : int) = x - 1000
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int{ (app[Stdlib!.>=] _ 0) } end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int{ (app[Stdlib!.>=] _ 0) }
       The type "int -> int" is not compatible with the type
         "int -> int{ (app[Stdlib!.>=] _ 0) }"
       Type "int" is not compatible with type "int{ (app[Stdlib!.>=] _ 0) }"
|}]

(* @acc id=seal_covariant_drop final=ACCEPT today=REJECT stable=no unlocks=integration+seals
   Covariant drop: a refined-result implementation behind a BARE
   interface. The interface asks only for [int], the impl provides
   [int{ _ >= 0 }] -- weakening in the covariant (result) direction,
   which is sound and accepted.
   FINAL: accepts. TODAY: rejected -- and note it fails inside the
   impl body first (the result annotation on a bare value is itself
   fail-closed today), before the seal is even reached. *)
module Seal_covariant_drop : sig
  val f : int -> int
end = struct
  let f (x : int) = (x * x : int{ _ >= 0 })
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : int) = (x * x : int{ _ >= 0 })
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int{ (app[Stdlib!.>=] _ 0) } end
       is not included in
         sig val f : int -> int end
       Values do not match:
         val f : int -> int{ (app[Stdlib!.>=] _ 0) }
       is not included in
         val f : int -> int
       The type "int -> int{ (app[Stdlib!.>=] _ 0) }"
       is not compatible with the type "int -> int"
       Type "int{ (app[Stdlib!.>=] _ 0) }" is not compatible with type "int"
|}]

(* @acc id=seal_launder_unsound final=REJECT today=ACCEPT stable=no unlocks=seals
   KNOWN GAP, deferred to the Seals stage: an UNCONSTRAINED implementation
   ([Obj.magic 0] : the value-restricted [Tvar] result) behind a refined
   interface laundered the refinement through signature inclusion, with no
   obligation queued.  A concrete bare implementation is already rigidly
   rejected (see seal_conforming); only this [Tvar]-through-seal case slips.
   Signature-boundary obligations are the Seals stage -- the verification pass
   over the structure has no visibility into the ascribing signature -- so this
   is an ANCHOR recording the current unsound ACCEPT.  It flips to REJECT when
   Seals queues the directed implication VC. *)
module Seal_launder : sig
  val x : int{ _ = 1 }
end = struct
  let x = Obj.magic 0
end
[%%expect {|
module Seal_launder : sig val x : int{ (app[Stdlib!.=] _ 1) } end
|}]
