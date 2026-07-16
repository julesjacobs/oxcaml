(* TEST
 expect;
*)

(* SOUNDNESS CAMPAIGN — Family 7 (seal channel: directed implication at
   signature boundaries).

   A module seal [struct ... end : SIG] may strengthen a value's refinement
   only if the implementation's refinement IMPLIES the interface's. Two
   guarantees: (a) a BARE implementation behind a refined interface is a
   structural mismatch, rejected WITHOUT invoking Lean (fail-closed — a bare
   value never acquires a refinement for free, even a tautological one); (b)
   when both sides are refined, the implication is DIRECTED — impl => iface,
   discharged by Lean; the wrong direction fails not-proved. *)

(* SC1: bare val behind a refined interface — structural reject, no Lean. *)
module Sc1 : sig
  val x : int{ _ > 0 }
end = struct
  let x = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : int end
       is not included in
         sig val x : int{ (app[Stdlib!.>] _ 0) } end
       Values do not match:
         val x : int
       is not included in
         val x : int{ (app[Stdlib!.>] _ 0) }
       The type "int" is not compatible with the type
         "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* SC2: even a TAUTOLOGICAL interface predicate is not granted to a bare impl —
   still a structural mismatch (fail-closed). *)
module Sc2 : sig
  val x : int{ _ = _ }
end = struct
  let x = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : int end
       is not included in
         sig val x : int{ (app[Stdlib!.=] _ _) } end
       Values do not match:
         val x : int
       is not included in
         val x : int{ (app[Stdlib!.=] _ _) }
       The type "int" is not compatible with the type
         "int{ (app[Stdlib!.=] _ _) }"
|}]

(* SC3: refined impl implies refined interface (5 = 5 => 5 > 0) — Lean
   discharges the directed implication; accepts. *)
module Sc3 : sig
  val x : int{ _ > 0 }
end = struct
  let x = (5 : int{ _ = 5 })
end
[%%expect {|
module Sc3 : sig val x : int{ (app[Stdlib!.>] _ 0) } end
|}]

(* SC4: WRONG direction — [_ > 0] does not imply [_ = 5]; fails at the seal. *)
module Sc4 : sig
  val x : int{ _ = 5 }
end = struct
  let x = (7 : int{ _ > 0 })
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = (7 : int{ _ > 0 })
5 | end
Error: Refinement verification failed at module seal for value "x" (not-proved)
Line 2, characters 2-22:
2 |   val x : int{ _ = 5 }
      ^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value x
Line 4, characters 6-7:
4 |   let x = (7 : int{ _ > 0 })
          ^
  Implementation declaration for value x
|}]
