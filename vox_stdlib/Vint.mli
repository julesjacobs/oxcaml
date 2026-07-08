(* Vint -- reflected integer ops (min / max / abs / sign / pow / clamp / min3 / max3 /
   div / mod, plus native succ / pred / even / odd). Every LAW is a [theorem] PROVED from a
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

-- ===== sign =====
-- Like the min/max algebra, vi_sign is public-but-not-exposed: clients name it
-- in refinements but reason through the shipped laws (cases + the three
-- characterization facts, each a trigger on vi_sign x with the sign hyp bound by
-- that trigger -- contrast the un-fireable free-var bound of notes/vint.md).
public def vi_sign (x : Int) : Int := if x < 0 then -1 else if x > 0 then 1 else 0
@[grind] public theorem vi_sign_cases (x : Int) :
    vi_sign x = -1 ∨ vi_sign x = 0 ∨ vi_sign x = 1 := by grind [vi_sign]
grind_pattern vi_sign_cases => vi_sign x
@[grind] public theorem vi_sign_pos (x : Int) (h : 0 < x) : vi_sign x = 1 := by
  grind [vi_sign]
grind_pattern vi_sign_pos => vi_sign x
@[grind] public theorem vi_sign_neg (x : Int) (h : x < 0) : vi_sign x = -1 := by
  grind [vi_sign]
grind_pattern vi_sign_neg => vi_sign x
@[grind] public theorem vi_sign_zero (x : Int) (h : x = 0) : vi_sign x = 0 := by
  grind [vi_sign]
grind_pattern vi_sign_zero => vi_sign x

-- ===== pow (recursion + termination) =====
-- vi_pow recurses on the exponent, driven to termination by n.toNat (the branch
-- guard n <= 0 makes the toNat measure strictly decrease). vi_pow_nonneg needs
-- the auto-generated vi_pow.induct; vi_pow_zero/_succ are the reduction algebra.
-- CAVEAT (L6, notes/vint.md): vi_pow_nonneg is model-true under vox's unbounded
-- Int but a machine-int ipow can overflow to a negative -- same status as vi_abs.
public def vi_pow (b : Int) (n : Int) : Int :=
  if n <= 0 then 1 else b * vi_pow b (n - 1)
termination_by n.toNat
decreasing_by omega
@[grind] public theorem vi_pow_zero (b : Int) : vi_pow b 0 = 1 := by rw [vi_pow]; simp
grind_pattern vi_pow_zero => vi_pow b 0
@[grind] public theorem vi_pow_succ (b : Int) (n : Int) (hn : 0 <= n) :
    vi_pow b (n + 1) = b * vi_pow b n := by
  rw [vi_pow]; rw [if_neg (by omega)]; congr 2; omega
grind_pattern vi_pow_succ => vi_pow b (n + 1)
@[grind] public theorem vi_pow_nonneg (b : Int) (hb : 0 <= b) (n : Int) :
    0 <= vi_pow b n := by
  induction n using vi_pow.induct with
  | case1 n h => rw [vi_pow]; simp [h]
  | case2 n h ih => rw [vi_pow]; simp only [if_neg h]; exact Int.mul_nonneg hb ih
grind_pattern vi_pow_nonneg => vi_pow b n

-- ===== clamp (direct, not min/max composition -- see smoke for the compose form) =====
public def vi_clamp (lo hi x : Int) : Int :=
  if x < lo then lo else if x > hi then hi else x
@[grind] public theorem vi_clamp_lb (lo hi x : Int) (h : lo <= hi) :
    lo <= vi_clamp lo hi x := by grind [vi_clamp]
grind_pattern vi_clamp_lb => vi_clamp lo hi x
@[grind] public theorem vi_clamp_ub (lo hi x : Int) (h : lo <= hi) :
    vi_clamp lo hi x <= hi := by grind [vi_clamp]
grind_pattern vi_clamp_ub => vi_clamp lo hi x
@[grind] public theorem vi_clamp_id (lo hi x : Int) (h1 : lo <= x) (h2 : x <= hi) :
    vi_clamp lo hi x = x := by grind [vi_clamp]
grind_pattern vi_clamp_id => vi_clamp lo hi x

-- ===== min3 / max3 (ternary, composed from vi_min/vi_max) =====
-- All three bound laws fire: every variable (a,b,c) is bound by the trigger
-- vi_min3 a b c, so the free-arithmetic-var wall (notes/vint.md) does not apply.
public def vi_min3 (a b c : Int) : Int := vi_min a (vi_min b c)
public def vi_max3 (a b c : Int) : Int := vi_max a (vi_max b c)
@[grind] public theorem vi_min3_le_left (a b c : Int) : vi_min3 a b c <= a := by
  grind [vi_min3, vi_min]
grind_pattern vi_min3_le_left => vi_min3 a b c
@[grind] public theorem vi_min3_le_mid (a b c : Int) : vi_min3 a b c <= b := by
  grind [vi_min3, vi_min]
grind_pattern vi_min3_le_mid => vi_min3 a b c
@[grind] public theorem vi_min3_le_right (a b c : Int) : vi_min3 a b c <= c := by
  grind [vi_min3, vi_min]
grind_pattern vi_min3_le_right => vi_min3 a b c
@[grind] public theorem vi_min3_cases (a b c : Int) :
    vi_min3 a b c = a ∨ vi_min3 a b c = b ∨ vi_min3 a b c = c := by
  grind [vi_min3, vi_min]
grind_pattern vi_min3_cases => vi_min3 a b c
@[grind] public theorem vi_max3_ge_left (a b c : Int) : a <= vi_max3 a b c := by
  grind [vi_max3, vi_max]
grind_pattern vi_max3_ge_left => vi_max3 a b c
@[grind] public theorem vi_max3_ge_mid (a b c : Int) : b <= vi_max3 a b c := by
  grind [vi_max3, vi_max]
grind_pattern vi_max3_ge_mid => vi_max3 a b c
@[grind] public theorem vi_max3_ge_right (a b c : Int) : c <= vi_max3 a b c := by
  grind [vi_max3, vi_max]
grind_pattern vi_max3_ge_right => vi_max3 a b c
@[grind] public theorem vi_max3_cases (a b c : Int) :
    vi_max3 a b c = a ∨ vi_max3 a b c = b ∨ vi_max3 a b c = c := by
  grind [vi_max3, vi_max]
grind_pattern vi_max3_cases => vi_max3 a b c

-- ===== div / mod (reflected ops carrying the division algorithm) =====
-- OCaml [/] and [mod] render to Int.tdiv / Int.tmod (T-division, toward zero:
-- (-1) mod 2 = -1, faithful to OCaml -- probe-confirmed, so NO Euclidean
-- soundness gap). grind does not carry the division algorithm on its own (a bare
-- [0 <= x mod d] is NOT PROVED). We ship the three facts as laws on vi_div/vi_mod
-- rather than on the raw Int.tdiv/Int.tmod because a grind law keyed on a CORE
-- symbol does NOT survive the VoxSig import (probe-confirmed: fires in-unit,
-- inert cross-unit -- notes/vint.md), whereas a law on our own def fires like
-- vi_min's. Kept public-but-NOT-exposed so the laws stay load-bearing; a client
-- gets the division facts by calling idiv/imod (not the native [x mod d], which
-- stays fact-free by design). Bounds guarded on 0 <= x because tmod carries the
-- dividend's sign.
public def vi_mod (x d : Int) : Int := Int.tmod x d
public def vi_div (x d : Int) : Int := Int.tdiv x d
@[grind] public theorem vi_mod_nonneg (x d : Int) (hx : 0 <= x) (hd : 0 < d) :
    0 <= vi_mod x d := by unfold vi_mod; exact Int.tmod_nonneg (a := x) (b := d) hx
grind_pattern vi_mod_nonneg => vi_mod x d
@[grind] public theorem vi_mod_lt (x d : Int) (hd : 0 < d) :
    vi_mod x d < d := by unfold vi_mod; exact Int.tmod_lt_of_pos x hd
grind_pattern vi_mod_lt => vi_mod x d
@[grind] public theorem vi_divmod (x d : Int) :
    d * vi_div x d + vi_mod x d = x := by
  unfold vi_div vi_mod; exact Int.mul_tdiv_add_tmod x d
grind_pattern vi_divmod => vi_mod x d
|lean}]

val imin : int -> int -> int [@@vox.reflect "vi_min"]
val imax : int -> int -> int [@@vox.reflect "vi_max"]
val iabs : int -> int [@@vox.reflect "vi_abs"]
val isign : int -> int [@@vox.reflect "vi_sign"]
val ipow : int -> int -> int [@@vox.reflect "vi_pow"]
val iclamp : int -> int -> int -> int [@@vox.reflect "vi_clamp"]
val imin3 : int -> int -> int -> int [@@vox.reflect "vi_min3"]
val imax3 : int -> int -> int -> int [@@vox.reflect "vi_max3"]
val idiv : int -> int -> int [@@vox.reflect "vi_div"]
val imod : int -> int -> int [@@vox.reflect "vi_mod"]

(* succ / pred / even / odd carry their spec inline (native +/-/mod), no Lean
   def: [+ 1], [- 1], [mod] are refinement-native, so these are transparent
   conveniences with no algebra to keep live. *)
val succ : (x : int) -> int{ _ = x + 1 }
val pred : (x : int) -> int{ _ = x - 1 }
val ieven : (x : int) -> bool{ _ = (x mod 2 = 0) }
val iodd : (x : int) -> bool{ _ = (x mod 2 <> 0) }
