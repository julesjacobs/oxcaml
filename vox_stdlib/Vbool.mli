(* Vbool -- boolean operations and a bool-as-DATA demonstration. The first
   stdlib module authored against Variant V (bool VALUES sort at Lean [Bool],
   condition/predicate positions stay [Prop], with the hybrid bridge at the
   boundary; see docs/plans/2026-07-07-vox-bool-representation-study.md).

   Two kinds of surface:

   - The boolean ALGEBRA (not/and/or/xor/equal) is refinement-NATIVE: [&&], [||],
     [not], [<>], [=] over bool binders already model as the boolish Prop
     connectives, so each op ships an EXACT spec ([_ = not b], [_ = (a && b)],
     ...) with no Lean def and no law -- like Vint's succ/pred/even/odd. bnot is
     deliberately implemented by a bool-scrutinee [match] (Vbool.ml), exercising
     Variant V's wart-(b) positive-arm fact (the [true] arm now refines to
     [b = true]); the older Prop scrutinee gave that arm no hypothesis.

   - [to_int] is the bool-as-DATA case: its value flows into [vb_toint : Bool ->
     Int], a model function that CASES on a Bool-sorted value (Variant V's
     wart-(a) fix -- a Prop bool could not be cased). It ships true/false
     characterization laws so a client discharges either branch, and the V bridge
     lets both an [if]- and a [match]-bodied caller verify.

   V-acceptance findings (first real consumer) are in notes/vbool.md; the
   case-able-bool-FIELD datatype exercise lives in clients/smoke_Vbool.ml. *)

[%%vox.lean {lean|
-- to_int: bool as DATA. vb_toint cases on a Bool value -- the thing a Prop bool
-- could not do (wart (a)). Kept public-but-NOT-exposed so the laws are
-- load-bearing; the true/false pair is what lets grind close either branch of a
-- caller (the V hybrid bridges the caller's Prop condition to the Bool value).
public def vb_toint (b : Bool) : Int := if b then 1 else 0
@[grind] public theorem vb_toint_true (b : Bool) (h : b = true) : vb_toint b = 1 := by
  grind [vb_toint]
grind_pattern vb_toint_true => vb_toint b
@[grind] public theorem vb_toint_false (b : Bool) (h : b = false) : vb_toint b = 0 := by
  grind [vb_toint]
grind_pattern vb_toint_false => vb_toint b
@[grind] public theorem vb_toint_cases (b : Bool) : vb_toint b = 0 ∨ vb_toint b = 1 := by
  grind [vb_toint]
grind_pattern vb_toint_cases => vb_toint b
|lean}]

(* Boolean algebra: native boolish specs (no Lean def). bnot is implemented via a
   bool [match] in the .ml (wart-(b) exercise); the rest are native operators. *)
val bnot : (b : bool) -> bool{ _ = not b }
val band : (a : bool) -> (b : bool) -> bool{ _ = (a && b) }
val bor : (a : bool) -> (b : bool) -> bool{ _ = (a || b) }
val bxor : (a : bool) -> (b : bool) -> bool{ _ = (a <> b) }
val bequal : (a : bool) -> (b : bool) -> bool{ _ = (a = b) }

(* bool <-> int bridges. to_int cases on the Bool value in the model; of_int
   returns the native nonzero test. *)
val to_int : (b : bool) -> int{ _ = vb_toint b }
val of_int : (n : int) -> bool{ _ = (n <> 0) }
