(* Vint -- reflected integer ops (min / max / abs). Every LAW is a [theorem] PROVED from a
   computable Lean [def] (never an [axiom]), so the assumed-axiom ledger is empty. That is
   NOT the same as "zero trust": the trust surface is the inspectable [@@vox.reflect]
   body<->def correspondence, which the solver does NOT check -- a divergent body (e.g.
   imin = a+b) would verify and then prove runtime-false facts. The shipped bodies were
   AUDITED character-for- character against their defs; that audit, not a machine check,
   is what makes the module honest. This is the graduation of demo/reflectbits:
   min/max/abs, vi_* names, the full bound/comm/idem algebra, plus the min/max CASES laws
   (vi_max a b is a or b) that let a client discharge clamp-style COMBINED bounds (lo <=
   clamp <= hi) by case analysis -- the conditional "vi_max a b <= c given a<=c, b<=c"
   form cannot fire (its c is a free arithmetic variable no grind pattern can bind; see
   notes/vint.md).

   The model defs are [public] (clients may name [vi_min] etc. in refinements) but
   deliberately NOT [expose]d: at a client, grind reasons about the ops through the
   shipped LAWS, not by unfolding the defs. That is what keeps the laws load-bearing
   (dead-law check, blueprint 6.7) rather than derivable. *)

[%%vox.lean
  {lean|
public def vi_min (x y : Int) : Int := if x <= y then x else y
public def vi_max (x y : Int) : Int := if x <= y then y else x
public def vi_abs (x : Int) : Int := if x < 0 then -x else x

@[grind] public theorem vi_min_comm (x y : Int) : vi_min x y = vi_min y x := by
  grind [vi_min]
@[grind] public theorem vi_min_idem (x : Int) : vi_min x x = x := by grind [vi_min]
@[grind] public theorem vi_min_le_left (x y : Int) : vi_min x y <= x := by
  grind [vi_min]
grind_pattern vi_min_le_left => vi_min x y
@[grind] public theorem vi_min_le_right (x y : Int) : vi_min x y <= y := by
  grind [vi_min]
grind_pattern vi_min_le_right => vi_min x y
@[grind] public theorem vi_min_cases (a b : Int) : vi_min a b = a ∨ vi_min a b = b := by
  grind [vi_min]
grind_pattern vi_min_cases => vi_min a b

@[grind] public theorem vi_max_comm (x y : Int) : vi_max x y = vi_max y x := by
  grind [vi_max]
@[grind] public theorem vi_max_idem (x : Int) : vi_max x x = x := by grind [vi_max]
@[grind] public theorem vi_max_ge_left (x y : Int) : x <= vi_max x y := by
  grind [vi_max]
grind_pattern vi_max_ge_left => vi_max x y
@[grind] public theorem vi_max_ge_right (x y : Int) : y <= vi_max x y := by
  grind [vi_max]
grind_pattern vi_max_ge_right => vi_max x y
@[grind] public theorem vi_max_cases (a b : Int) : vi_max a b = a ∨ vi_max a b = b := by
  grind [vi_max]
grind_pattern vi_max_cases => vi_max a b

@[grind] public theorem vi_abs_nonneg (x : Int) : 0 <= vi_abs x := by grind [vi_abs]
grind_pattern vi_abs_nonneg => vi_abs x
|lean}]

val imin : int -> int -> int [@@vox.reflect "vi_min"]
val imax : int -> int -> int [@@vox.reflect "vi_max"]
val iabs : int -> int [@@vox.reflect "vi_abs"]
