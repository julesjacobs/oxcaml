(* Implementation of ptrie_packed.mli: the compiler's Patricia
   representation (middle_end/flambda2/algorithms/patricia_tree.ml),
   packed [prefix_and_bit] included, checked against its interface's
   model.

   The bridges below are the ENTIRE trust boundary, and each one is
   the compiler's own line: [unpack] is [x land (-x)] (a NATIVE PAIR,
   specified by the [lbit] theorems), [pack] is [lor], [mask] is
   [i land -(b lsl 1)], [zero_bit] is [i land b = 0].  The one
   departure: the compiler finds the highest differing bit with a
   [clz] builtin the stdlib lacks, so [branching_bit] carries the
   same contract over a doubling loop.  Every consequence -- that
   unpack inverts pack, the prefix algebra, the invariant -- is
   PROVED in the interface; [join], [mem] and [insert] are verified
   arm by arm against the model with nothing else assumed. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * t * t

[%%vox.lean {lean|
-- Bit-level theory, BIG-endian: [hmask i b] keeps the bits of [i]
-- strictly ABOVE the power-of-two bit [b]; [zbit i b] says bit [b] of
-- [i] is clear.  Euclidean / and % are floor semantics for positive
-- divisors, i.e. exactly infinite two's complement.
@[expose] def hmask (i b : Int) : Int := i - i % (2*b)

@[expose] def zbit (i b : Int) : Prop := i % (2*b) < b

instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

inductive isbit : Int -> Prop where
  | one : isbit 1
  | dbl : {b : Int} -> isbit b -> isbit (2*b)

theorem isbit_pos {b : Int} (h : isbit b) : 1 ≤ b := by
  induction h <;> omega
grind_pattern isbit_pos => isbit b

-- Highest differing bit of two distinct NONNEGATIVE integers (junk 1
-- when equal or when either is negative: two integers of opposite
-- sign differ at every sufficiently high bit, so no highest one
-- exists on ideal integers).
@[expose] def hbit (p0 p1 : Int) : Int :=
  if p0 < 0 ∨ p1 < 0 ∨ p0/2 = p1/2 then 1
  else 2 * hbit (p0/2) (p1/2)
termination_by (p0.natAbs + p1.natAbs)
decreasing_by omega

-- Lowest set bit of a POSITIVE integer (junk 1 otherwise): the
-- arithmetic meaning of the compiler's [x land (-x)].
@[expose] def lbit (x : Int) : Int :=
  if x ≤ 0 then 1
  else if x % 2 = 1 then 1
  else 2 * lbit (x/2)
termination_by x.natAbs
decreasing_by omega

-- The workhorse: peel one bit off a modulus.
theorem emod_unique {a b q r : Int} (h0 : 0 ≤ r) (h1 : r < b)
    (h : a = b*q + r) : a % b = r := by
  subst h
  rw [Int.add_comm, Int.add_mul_emod_self_left]
  exact Int.emod_eq_of_lt h0 h1

theorem emod_double (p m : Int) (hm : 0 < m) :
    p % (2*m) = 2 * ((p/2) % m) + p % 2 := by
  have h2 := Int.mul_ediv_add_emod p 2
  have h3 := Int.mul_ediv_add_emod (p/2) m
  have h4 := Int.emod_nonneg (p/2) (show m ≠ 0 by omega)
  have h5 := Int.emod_lt_of_pos (p/2) hm
  have h6 := Int.emod_nonneg p (show (2:Int) ≠ 0 by omega)
  have h7 := Int.emod_lt_of_pos p (show (0:Int) < 2 by omega)
  apply emod_unique (q := (p/2)/m) (by omega) (by omega)
  calc p = 2 * (m * ((p/2)/m) + (p/2) % m) + p % 2 := by rw [h3]; omega
    _ = (2*m) * ((p/2)/m) + (2 * ((p/2) % m) + p % 2) := by
          rw [Int.mul_add, Int.mul_assoc]; omega

theorem hbit_isbit (p0 p1 : Int) : isbit (hbit p0 p1) := by
  fun_induction hbit p0 p1 with
  | case1 => exact isbit.one
  | case2 _ _ _ ih => exact isbit.dbl ih
grind_pattern hbit_isbit => hbit p0 p1

theorem hbit_pos (p0 p1 : Int) : 1 ≤ hbit p0 p1 :=
  isbit_pos (hbit_isbit p0 p1)

-- Two distinct nonnegative integers disagree AT their highest
-- differing bit ...
theorem hbit_diff (p0 p1 : Int) :
    0 ≤ p0 → 0 ≤ p1 → p0 ≠ p1 →
    (zbit p0 (hbit p0 p1) ↔ ¬ zbit p1 (hbit p0 p1)) := by
  fun_induction hbit p0 p1 with
  | case1 p0 p1 h =>
    intro h0 h1 hne
    have hq : p0/2 = p1/2 := by omega
    have e0 := Int.mul_ediv_add_emod p0 2
    have e1 := Int.mul_ediv_add_emod p1 2
    have m0 := Int.emod_nonneg p0 (show (2:Int) ≠ 0 by omega)
    have m1 := Int.emod_lt_of_pos p0 (show (0:Int) < 2 by omega)
    have m2 := Int.emod_nonneg p1 (show (2:Int) ≠ 0 by omega)
    have m3 := Int.emod_lt_of_pos p1 (show (0:Int) < 2 by omega)
    unfold zbit
    omega
  | case2 p0 p1 h ih =>
    intro h0 h1 hne
    have hb := hbit_pos (p0/2) (p1/2)
    have e0 := emod_double p0 (2 * hbit (p0/2) (p1/2)) (by omega)
    have e1 := emod_double p1 (2 * hbit (p0/2) (p1/2)) (by omega)
    have hq : p0/2 ≠ p1/2 := by omega
    have ihh := ih (by omega) (by omega) hq
    unfold zbit at *
    omega

-- ... and agree strictly above it.
theorem hbit_agree (p0 p1 : Int) :
    0 ≤ p0 → 0 ≤ p1 →
    hmask p0 (hbit p0 p1) = hmask p1 (hbit p0 p1) := by
  fun_induction hbit p0 p1 with
  | case1 p0 p1 h =>
    intro h0 h1
    have hq : p0/2 = p1/2 := by omega
    have e0 := Int.mul_ediv_add_emod p0 2
    have e1 := Int.mul_ediv_add_emod p1 2
    have m0 := Int.emod_nonneg p0 (show (2:Int) ≠ 0 by omega)
    have m1 := Int.emod_lt_of_pos p0 (show (0:Int) < 2 by omega)
    have m2 := Int.emod_nonneg p1 (show (2:Int) ≠ 0 by omega)
    have m3 := Int.emod_lt_of_pos p1 (show (0:Int) < 2 by omega)
    unfold hmask
    omega
  | case2 p0 p1 h ih =>
    intro h0 h1
    have hb := hbit_pos (p0/2) (p1/2)
    have e0 := emod_double p0 (2 * hbit (p0/2) (p1/2)) (by omega)
    have e1 := emod_double p1 (2 * hbit (p0/2) (p1/2)) (by omega)
    have f0 := Int.mul_ediv_add_emod p0 2
    have f1 := Int.mul_ediv_add_emod p1 2
    have ihh := ih (by omega) (by omega)
    unfold hmask at *
    omega

-- Nonnegative integers that differ ABOVE bit [b] have their highest
-- differing bit above [b].
theorem hbit_gt (p0 p1 : Int) :
    0 ≤ p0 → 0 ≤ p1 →
    ∀ b, isbit b → hmask p0 b ≠ hmask p1 b → b < hbit p0 p1 := by
  fun_induction hbit p0 p1 with
  | case1 p0 p1 h =>
    intro h0 h1 b hb hm
    have hq : p0/2 = p1/2 := by omega
    exfalso
    apply hm
    have hbp := isbit_pos hb
    have e0 := emod_double p0 b hbp
    have e1 := emod_double p1 b hbp
    have f0 := Int.mul_ediv_add_emod p0 2
    have f1 := Int.mul_ediv_add_emod p1 2
    have hg : p0/2 % b = p1/2 % b := by rw [hq]
    unfold hmask
    omega
  | case2 p0 p1 h ih =>
    intro h0 h1 b hb hm
    have hq : p0/2 ≠ p1/2 := by omega
    have hc := hbit_pos (p0/2) (p1/2)
    cases hb with
    | one =>
      have := hbit_pos (p0/2) (p1/2)
      omega
    | @dbl c hc' =>
      have hcpos := isbit_pos hc'
      have e0 := emod_double p0 (2*c) (by omega)
      have e1 := emod_double p1 (2*c) (by omega)
      have f0 := Int.mul_ediv_add_emod p0 2
      have f1 := Int.mul_ediv_add_emod p1 2
      have hm' : hmask (p0/2) c ≠ hmask (p1/2) c := by
        unfold hmask at *
        omega
      have := ih (by omega) (by omega) c hc' hm'
      omega

theorem isbit_dvd {b : Int} (hb : isbit b) :
    ∀ a, isbit a → a ≤ b → a ∣ b := by
  induction hb with
  | one =>
    intro a ha hle
    have := isbit_pos ha
    have : a = 1 := by omega
    subst this; exact Int.one_dvd 1
  | @dbl c hc ih =>
    intro a ha hle
    cases ha with
    | one => exact Int.one_dvd _
    | @dbl a' ha' =>
      have := ih a' ha' (by omega)
      rcases this with ⟨k, hk⟩
      exact ⟨k, by rw [hk, Int.mul_assoc]⟩

theorem isbit_lt_dvd {b : Int} (hb : isbit b) :
    ∀ a, isbit a → a < b → 2*a ∣ b := by
  induction hb with
  | one =>
    intro a ha hlt
    have := isbit_pos ha
    exact absurd hlt (by omega)
  | @dbl c hc ih =>
    intro a ha hlt
    cases ha with
    | one => exact ⟨c, by omega⟩
    | @dbl a' ha' =>
      have := ih a' ha' (by omega)
      rcases this with ⟨k, hk⟩
      exact ⟨k, by simp [hk, Int.mul_assoc]⟩

-- Two multiples of [d] strictly apart are at least [d] apart: the
-- step that keeps every "multiple below a multiple" argument linear.
theorem dvd_lt_le {d x y : Int} (hd : 0 < d) (hx : d ∣ x) (hy : d ∣ y)
    (h : x < y) : x + d ≤ y := by
  rcases hx with ⟨qx, hqx⟩
  rcases hy with ⟨qy, hqy⟩
  subst hqx; subst hqy
  have hq : qx < qy := by
    rcases Int.lt_or_le qx qy with h' | h'
    · exact h'
    · exfalso
      have hmul := Int.mul_le_mul_of_nonneg_left h' (Int.le_of_lt hd)
      omega
  have : d * (qx + 1) ≤ d * qy :=
    Int.mul_le_mul_of_nonneg_left (by omega) (by omega)
  rw [Int.mul_add, Int.mul_one] at this
  omega

-- Telescoping upward: masking above [b], then above a HIGHER bit
-- [b'], is masking above [b'].
theorem hmask_tele {b b' : Int} (i : Int) (hb : 0 < b) (hb' : 0 < b')
    (hd : (2*b) ∣ (2*b')) : hmask (hmask i b) b' = hmask i b' := by
  have hMM := Int.emod_emod_of_dvd i hd
  have h1 := Int.mul_ediv_add_emod i (2*b')
  have h2 := Int.mul_ediv_add_emod (i % (2*b')) (2*b)
  have h3 := Int.emod_nonneg (i % (2*b')) (show (2*b) ≠ 0 by omega)
  have h4 := Int.emod_nonneg i (show (2*b') ≠ 0 by omega)
  have h5 := Int.emod_lt_of_pos i (show (0:Int) < 2*b' by omega)
  have h6 : 0 ≤ (i % (2*b')) / (2*b) := Int.ediv_nonneg h4 (by omega)
  have h7 : 0 ≤ 2*b * (i % (2*b') / (2*b)) :=
    Int.mul_nonneg (by omega) h6
  have h8 := Int.emod_nonneg i (show (2*b) ≠ 0 by omega)
  have key : (i - i % (2*b)) % (2*b') = i % (2*b') - i % (2*b) := by
    apply emod_unique (q := i / (2*b')) (by omega) (by omega)
    omega
  unfold hmask
  omega

-- Keys that match a prefix above [b] follow the PREFIX's bits at any
-- higher bit [B] with 2b | B: the whole subtree rides one side of a
-- higher branch.
theorem zbit_match {b B : Int} (j p : Int) (hb : 0 < b) (hB : 0 < B)
    (hd : (2*b) ∣ B) (hp : p % (2*b) = 0) (hj : hmask j b = p) :
    (zbit j B ↔ zbit p B) := by
  have hd2 : (2*b) ∣ (2*B) := by
    rcases hd with ⟨k, hk⟩
    exact ⟨2*k, by rw [hk, Int.mul_left_comm]⟩
  have hjj := Int.mul_ediv_add_emod j (2*b)
  have hj0 := Int.emod_nonneg j (show (2*b) ≠ 0 by omega)
  have hj1 := Int.emod_lt_of_pos j (show (0:Int) < 2*b by omega)
  have hpm : p % (2*B) % (2*b) = 0 := by
    rw [Int.emod_emod_of_dvd p hd2, hp]
  have hpp := Int.mul_ediv_add_emod p (2*B)
  have hp0 := Int.emod_nonneg p (show (2*B) ≠ 0 by omega)
  have hp1 := Int.emod_lt_of_pos p (show (0:Int) < 2*B by omega)
  have hdvd1 : (2*b) ∣ (p % (2*B)) :=
    ⟨p % (2*B) / (2*b), by
      have := Int.mul_ediv_add_emod (p % (2*B)) (2*b); omega⟩
  have hgap : p % (2*B) + 2*b ≤ 2*B :=
    dvd_lt_le (by omega) hdvd1 hd2 hp1
  have key : j % (2*B) = p % (2*B) + j % (2*b) := by
    apply emod_unique (q := p / (2*B)) (by omega) (by omega)
    unfold hmask at hj
    omega
  constructor
  · intro hz
    unfold zbit at *
    by_cases hcase : p % (2*B) < B
    · exact hcase
    · omega
  · intro hz
    unfold zbit at *
    have hdvdB : (2*b) ∣ B := hd
    have hgapB : p % (2*B) + 2*b ≤ B :=
      dvd_lt_le (by omega) hdvd1 hdvdB hz
    omega

-- Helpers about hmask on nonnegative integers.
theorem hmask_nonneg (i b : Int) (hi : 0 ≤ i) (_hb : 0 < b) :
    0 ≤ hmask i b := by
  have h1 := Int.mul_ediv_add_emod i (2*b)
  have h2 := Int.ediv_nonneg hi (show (0:Int) ≤ 2*b by omega)
  have h3 : 0 ≤ 2*b * (i / (2*b)) := Int.mul_nonneg (by omega) h2
  unfold hmask
  omega
grind_pattern hmask_nonneg => hmask i b

theorem hmask_mod (i b : Int) (hb : 0 < b) : hmask i b % (2*b) = 0 := by
  have h1 := Int.mul_ediv_add_emod i (2*b)
  unfold hmask
  apply emod_unique (q := i / (2*b)) (by omega) (by omega)
  omega
grind_pattern hmask_mod => hmask i b

-- A prefix with no bits at or below [b] is its own mask.
theorem hmask_self (p b : Int) (hb : 0 < b) (hp : p % (2*b) = 0) :
    hmask p b = p := by
  unfold hmask
  omega
grind_pattern hmask_self => hmask p b

-- [pack]'s disjointness precondition, as a named Prop the surface
-- contract can state.
@[grind, expose] def packmod (p b : Int) : Prop := p % (2*b) = 0

-- [lbit] of anything is a bit (the junk value 1 included) ...
theorem lbit_isbit (x : Int) : isbit (lbit x) := by
  fun_induction lbit x with
  | case1 => exact isbit.one
  | case2 => exact isbit.one
  | case3 x h1 h2 ih => exact isbit.dbl ih
grind_pattern lbit_isbit => lbit x

-- ... a positive integer splits as prefix-above + lowest-bit ...
theorem lbit_split (x : Int) :
    0 < x → (x - lbit x) % (2 * lbit x) = 0 ∧ 0 ≤ x - lbit x := by
  fun_induction lbit x with
  | case1 x h =>
    intro hx
    omega
  | case2 x h1 h2 =>
    intro hx
    omega
  | case3 x h1 h2 ih =>
    intro hx
    have hx2 : (0:Int) < x/2 := by omega
    obtain ⟨ih1, ih2⟩ := ih hx2
    have hc := isbit_pos (lbit_isbit (x/2))
    have e := emod_double (x - 2 * lbit (x/2)) (2 * lbit (x/2)) (by omega)
    have hxe : (x - 2 * lbit (x/2)) / 2 = x / 2 - lbit (x/2) := by omega
    rw [hxe] at e
    constructor
    · omega
    · omega
grind_pattern lbit_split => lbit x

-- ... and [lbit] is EXACTLY the decomposition [pack] built: the
-- proved meaning of the compiler's [x land (-x)] unpack.
theorem lbit_pack (b : Int) (hb : isbit b) :
    ∀ p, p % (2*b) = 0 → 0 ≤ p → lbit (p + b) = b := by
  induction hb with
  | one =>
    intro p hp h0
    unfold lbit
    rw [if_neg (by omega), if_pos (by omega)]
  | @dbl c hc ih =>
    intro p hp h0
    have hcpos := isbit_pos hc
    have h2 : (2:Int) ∣ 2*(2*c) := ⟨2*c, rfl⟩
    have hp2 : p % 2 = 0 := by
      have hmm := Int.emod_emod_of_dvd p h2
      rw [hp] at hmm
      omega
    have e := emod_double p (2*c) (by omega)
    have hsplit : (p/2) % (2*c) = 0 := by omega
    have hdiv : (p + 2*c)/2 = p/2 + c := by omega
    have hrec := ih (p/2) hsplit (by omega)
    unfold lbit
    rw [if_neg (by omega), if_neg (by omega), hdiv, hrec]
grind_pattern lbit_pack => lbit (p + b)

-- Tree model: naive membership over the WHOLE tree, and the Patricia
-- invariant over the PACKED node int [x]: [lbit x] is the branching
-- bit and [x - lbit x] the prefix (bits strictly above it) -- their
-- shape facts ([isbit], disjointness, nonnegativity) are the [lbit]
-- theorems above, so the invariant itself only demands [0 < x].
-- Keys are NONNEGATIVE: on ideal integers a negative and a
-- nonnegative key differ at every sufficiently high bit, so no
-- highest differing bit exists (the compiler's finite word makes the
-- sign bit an ordinary bit; width is outside the model, as always).
@[grind, expose] def mem : Int -> Vox_Ptrie_packed_t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ t0 t1 => mem i t0 ∨ mem i t1

@[grind, expose] def allmatch : Vox_Ptrie_packed_t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => hmask j b = p
  | .Branch _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind, expose] def allzero : Vox_Ptrie_packed_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind, expose] def allone : Vox_Ptrie_packed_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind, expose] def allpos : Vox_Ptrie_packed_t -> Prop
  | .Empty => True
  | .Leaf j => 0 ≤ j
  | .Branch _ t0 t1 => allpos t0 ∧ allpos t1

@[grind, expose] def trie : Vox_Ptrie_packed_t -> Prop
  | .Empty => True
  | .Leaf j => 0 ≤ j
  | .Branch x t0 t1 =>
      0 < x ∧
      allmatch t0 (x - lbit x) (lbit x) ∧
      allmatch t1 (x - lbit x) (lbit x) ∧
      allzero t0 (lbit x) ∧ allone t1 (lbit x) ∧
      trie t0 ∧ trie t1

@[grind, expose] def join (p0 : Int) (t0 : Vox_Ptrie_packed_t)
    (p1 : Int) (t1 : Vox_Ptrie_packed_t) : Vox_Ptrie_packed_t :=
  if zbit p0 (hbit p0 p1)
  then .Branch (hmask p0 (hbit p0 p1) + hbit p0 p1) t0 t1
  else .Branch (hmask p0 (hbit p0 p1) + hbit p0 p1) t1 t0

@[grind, expose] def insert (i : Int) : Vox_Ptrie_packed_t -> Vox_Ptrie_packed_t
  | .Empty => .Leaf i
  | .Leaf j => if i = j then .Leaf i else join i (.Leaf i) j (.Leaf j)
  | .Branch x t0 t1 =>
      if hmask i (lbit x) = x - lbit x then
        if zbit i (lbit x) then .Branch x (insert i t0) t1
        else .Branch x t0 (insert i t1)
      else join i (.Leaf i) (x - lbit x) (.Branch x t0 t1)

-- The invariant makes one-path search complete.
theorem not_mem_mismatch (i p b : Int) (t : Vox_Ptrie_packed_t)
    (h : allmatch t p b) (hm : hmask i b ≠ p) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_mismatch => mem i t, allmatch t p b

theorem not_mem_zero (i b : Int) (t : Vox_Ptrie_packed_t)
    (h : allzero t b) (hz : ¬ zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_zero => mem i t, allzero t b

theorem not_mem_one (i b : Int) (t : Vox_Ptrie_packed_t)
    (h : allone t b) (hz : zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_one => mem i t, allone t b

-- Insertion adds exactly one key ...
theorem mem_insert (x y : Int) (t : Vox_Ptrie_packed_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t) := by
  induction t <;> grind
grind_pattern mem_insert => mem y (insert x t)

theorem allmatch_insert (x p b : Int) (t : Vox_Ptrie_packed_t)
    (h : allmatch t p b) (hx : hmask x b = p) :
    allmatch (insert x t) p b := by
  induction t <;> grind
grind_pattern allmatch_insert => allmatch (insert x t) p b

theorem allzero_insert (x b : Int) (t : Vox_Ptrie_packed_t)
    (h : allzero t b) (hx : zbit x b) : allzero (insert x t) b := by
  induction t <;> grind
grind_pattern allzero_insert => allzero (insert x t) b

theorem allone_insert (x b : Int) (t : Vox_Ptrie_packed_t)
    (h : allone t b) (hx : ¬ zbit x b) : allone (insert x t) b := by
  induction t <;> grind
grind_pattern allone_insert => allone (insert x t) b

-- Weakening a subtree's prefix to a HIGHER bit.
theorem allmatch_weaken {b B : Int} (p : Int) (t : Vox_Ptrie_packed_t)
    (h : allmatch t p b) (hb : 0 < b) (hB : 0 < B)
    (hd : (2*b) ∣ (2*B)) : allmatch t (hmask p B) B := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := hmask_tele j hb hB hd
    grind [allmatch]
  | Branch x t0 t1 ih0 ih1 => grind [allmatch]

theorem allzero_of_allmatch {b B : Int} (p : Int) (t : Vox_Ptrie_packed_t)
    (h : allmatch t p b) (hb : 0 < b) (hB : 0 < B) (hd : (2*b) ∣ B)
    (hp : p % (2*b) = 0) (hz : zbit p B) : allzero t B := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := zbit_match j p hb hB hd hp
    grind [allmatch, allzero]
  | Branch x t0 t1 ih0 ih1 => grind [allmatch, allzero]

theorem allone_of_allmatch {b B : Int} (p : Int) (t : Vox_Ptrie_packed_t)
    (h : allmatch t p b) (hb : 0 < b) (hB : 0 < B) (hd : (2*b) ∣ B)
    (hp : p % (2*b) = 0) (hz : ¬ zbit p B) : allone t B := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := zbit_match j p hb hB hd hp
    grind [allmatch, allone]
  | Branch x t0 t1 ih0 ih1 => grind [allmatch, allone]

-- Joining a fresh leaf with a whole subtree keeps the invariant.
theorem trie_join (x p : Int) (t : Vox_Ptrie_packed_t)
    (ht : trie t) (hx : 0 ≤ x) (hp : 0 ≤ p) (hxnep : x ≠ p)
    (hall : allmatch t (hmask p (hbit x p)) (hbit x p))
    (hz : zbit p (hbit x p) → allzero t (hbit x p))
    (ho : ¬ zbit p (hbit x p) → allone t (hbit x p)) :
    trie (join x (.Leaf x) p t) := by
  have hbb := hbit_isbit x p
  have hbpos := isbit_pos hbb
  have hdiff := hbit_diff x p hx hp hxnep
  have hagree := hbit_agree x p hx hp
  have hnn := hmask_nonneg x (hbit x p) hx (by omega)
  have hmm := hmask_mod x (hbit x p) (by omega)
  have hpk := lbit_pack (hbit x p) hbb (hmask x (hbit x p)) hmm hnn
  have htl := hmask_tele x (b := hbit x p) (b' := hbit x p) (by omega)
    (by omega) (Int.dvd_refl _)
  grind

-- ... and insertion of a nonnegative key preserves it.
theorem trie_insert (x : Int) (t : Vox_Ptrie_packed_t)
    (h : trie t) (hx : 0 ≤ x) : trie (insert x t) := by
  induction t with
  | Empty => grind
  | Leaf j =>
    by_cases hxj : x = j
    · grind
    · have hj : (0:Int) ≤ j := h
      have := trie_join x j (.Leaf j) (by trivial) hx hj hxj
        (by grind) (by grind) (by grind)
      grind
  | Branch w t0 t1 ih0 ih1 =>
    have hw : (0:Int) < w := h.1
    have hbb := lbit_isbit w
    have hbpos := isbit_pos hbb
    obtain ⟨hsp, hpn⟩ := lbit_split w hw
    by_cases hmp : hmask x (lbit w) = w - lbit w
    · by_cases hzx : zbit x (lbit w)
      · have h1 : allmatch (insert x t0) (w - lbit w) (lbit w) :=
          allmatch_insert x (w - lbit w) (lbit w) t0 (by grind) hmp
        have h2 : allzero (insert x t0) (lbit w) :=
          allzero_insert x (lbit w) t0 (by grind) hzx
        grind
      · have h1 : allmatch (insert x t1) (w - lbit w) (lbit w) :=
          allmatch_insert x (w - lbit w) (lbit w) t1 (by grind) hmp
        have h2 : allone (insert x t1) (lbit w) :=
          allone_insert x (lbit w) t1 (by grind) hzx
        grind
    · -- the miss: x parts from this node's prefix ABOVE its bit, so
      -- the whole node rides on one side of a new, higher branch
      have hps : hmask (w - lbit w) (lbit w) = w - lbit w :=
        hmask_self _ _ (by omega) hsp
      have hxnep : x ≠ w - lbit w := by
        intro he; rw [he] at hmp; exact hmp hps
      have hBB := hbit_isbit x (w - lbit w)
      have hBpos := isbit_pos hBB
      have hgt : lbit w < hbit x (w - lbit w) :=
        hbit_gt x (w - lbit w) hx hpn (lbit w) hbb (by rw [hps]; exact hmp)
      have hdvd : (2 * lbit w) ∣ hbit x (w - lbit w) :=
        isbit_lt_dvd hBB (lbit w) hbb hgt
      have hdvd2 : (2 * lbit w) ∣ (2 * hbit x (w - lbit w)) := by
        rcases hdvd with ⟨k, hk⟩
        exact ⟨2*k, by rw [hk, Int.mul_left_comm]⟩
      have hall : allmatch (.Branch w t0 t1)
          (hmask (w - lbit w) (hbit x (w - lbit w)))
          (hbit x (w - lbit w)) := by
        have h0 : allmatch t0 (w - lbit w) (lbit w) := by grind
        have h1 : allmatch t1 (w - lbit w) (lbit w) := by grind
        have := allmatch_weaken (w - lbit w) t0 h0 (by omega) (by omega) hdvd2
        have := allmatch_weaken (w - lbit w) t1 h1 (by omega) (by omega) hdvd2
        grind [allmatch]
      have hz : zbit (w - lbit w) (hbit x (w - lbit w)) →
          allzero (.Branch w t0 t1) (hbit x (w - lbit w)) := by
        intro hzp
        have h0 : allmatch t0 (w - lbit w) (lbit w) := by grind
        have h1 : allmatch t1 (w - lbit w) (lbit w) := by grind
        have := allzero_of_allmatch (w - lbit w) t0 h0 (by omega) (by omega)
          hdvd hsp hzp
        have := allzero_of_allmatch (w - lbit w) t1 h1 (by omega) (by omega)
          hdvd hsp hzp
        grind [allzero]
      have ho : ¬ zbit (w - lbit w) (hbit x (w - lbit w)) →
          allone (.Branch w t0 t1) (hbit x (w - lbit w)) := by
        intro hzp
        have h0 : allmatch t0 (w - lbit w) (lbit w) := by grind
        have h1 : allmatch t1 (w - lbit w) (lbit w) := by grind
        have := allone_of_allmatch (w - lbit w) t0 h0 (by omega) (by omega)
          hdvd hsp hzp
        have := allone_of_allmatch (w - lbit w) t1 h1 (by omega) (by omega)
          hdvd hsp hzp
        grind [allone]
      have := trie_join x (w - lbit w) (.Branch w t0 t1) h hx hpn hxnep
        hall hz ho
      grind
grind_pattern trie_insert => trie (insert x t)
|lean}]

type set = t{ trie _ }

(* The compiler's unpack, verbatim -- bit and prefix from one packed
   int; the contract is the [lbit] characterization. *)
let unpack (x : int{ 0 < _ })
  : (int * int){ fst _ = x - lbit x && snd _ = lbit x } =
  assume_unchecked_ (let bit = x land (-x) in (x lxor bit, bit))

(* The compiler's pack, verbatim: disjointness makes [lor] addition. *)
let pack (p : int{ 0 <= _ }) (b : int{ isbit _ && packmod p _ })
  : int{ _ = p + b } =
  assume_unchecked_ (p lor b)

let zero_bit (i : int) (b : int{ isbit _ }) : bool{ _ = zbit i b } =
  assume_unchecked_ (i land b = 0)

(* Keep only the bits strictly higher than [b] (the compiler's
   [mask]). *)
let mask (i : int) (b : int{ isbit _ }) : int{ _ = hmask i b } =
  assume_unchecked_ (i land (-(b lsl 1)))

(* Highest bit at which two distinct nonnegative prefixes differ; the
   compiler computes this from [p0 lxor p1] with a [clz] builtin, the
   toy by doubling -- the contract is the same. *)
let branching_bit (p0 : int{ 0 <= _ }) (p1 : int{ 0 <= _ && _ <> p0 })
  : int{ _ = hbit p0 p1 } =
  assume_unchecked_
    (let x = p0 lxor p1 in
     let rec top b = if x land (-(b lsl 1)) = 0 then b else top (b lsl 1) in
     top 1)

(* Split two subtrees with distinct prefixes at their branching bit,
   zero side left; the packed node int is [pack]ed exactly as the
   model packs it. *)
let join (p0 : int{ 0 <= _ }) (t0 : t) (p1 : int{ 0 <= _ && _ <> p0 })
  (t1 : t) : t{ _ = join p0 t0 p1 t1 } =
  let b = branching_bit p0 p1 in
  let p = mask p0 b in
  let x = pack p b in
  let z = zero_bit p0 b in
  if z then Branch (x, t0, t1) else Branch (x, t1, t0)

let empty : set{ _ = Empty } = Empty

(* One path decides membership in the whole tree, exactly as in the
   little-endian toy -- but here the node's prefix and bit are
   RECOVERED from the packed int, and the recovery is proved. *)
let rec mem (i : int) (s : set) : bool{ _ = mem i s } =
  match s with
  | Empty -> false
  | Leaf j -> i = j
  | Branch (x, t0, t1) ->
    let (p, b) = unpack x in
    let m = mask i b in
    if m <> p then false
    else begin
      let z = zero_bit i b in
      if z then mem i t0 else mem i t1
    end

let rec insert (i : int{ 0 <= _ }) (s : set)
  : set{ _ = insert i s && mem i _ } =
  match s with
  | Empty -> Leaf i
  | Leaf j ->
    if i = j then s
    else begin
      let l = Leaf i in
      join i l j s
    end
  | Branch (x, t0, t1) ->
    let (p, b) = unpack x in
    let m = mask i b in
    if m = p then begin
      let z = zero_bit i b in
      if z then begin
        let t0' = insert i t0 in
        Branch (x, t0', t1)
      end
      else begin
        let t1' = insert i t1 in
        Branch (x, t0, t1')
      end
    end
    else begin
      let l = Leaf i in
      join i l p s
    end
