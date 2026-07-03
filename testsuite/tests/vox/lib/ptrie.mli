(* A Patricia trie -- the integer-set structure the compiler itself
   runs on (middle_end/flambda2/algorithms/patricia_tree.ml), as the
   classic little-endian toy: no GADT plumbing, just [empty], [insert]
   and [mem], with the real bit twiddling ([i land b = 0],
   [i land (b - 1)], and the lowest-set-bit trick [x land (-x)] that
   appears verbatim in the compiler's [unpack]).

   The block below is the whole logical story, proved once here and
   carried to the implementation and every client through this .cmi.
   The bit operations are MODELLED arithmetically -- under floor
   division, an integer IS its infinite two's-complement bit string,
   so masking low bits is [%], testing a bit is one inequality, and
   the branching bit is the lowest differing bit.  Every fact about
   them is a theorem; nothing bit-level is axiomatized. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * int * t * t

[%%vox.lean {lean|
-- Bit-level theory.  [mask i b] keeps the bits of [i] strictly below
-- the power-of-two bit [b]; [zbit i b] says bit [b] of [i] is clear.
-- Euclidean / and % are floor semantics for positive divisors, i.e.
-- exactly infinite two's complement.
def mask (i b : Int) : Int := i % b

def zbit (i b : Int) : Prop := i % (2*b) < b

instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

inductive isbit : Int -> Prop where
  | one : isbit 1
  | dbl : {b : Int} -> isbit b -> isbit (2*b)

-- Lowest differing bit of two distinct integers (junk 1 when equal).
def bbit (p0 p1 : Int) : Int :=
  if p0 = p1 then 1
  else if p0 % 2 = p1 % 2 then 2 * bbit (p0/2) (p1/2)
  else 1
termination_by (p0.natAbs + p1.natAbs)
decreasing_by omega

theorem isbit_pos {b : Int} (h : isbit b) : 1 ≤ b := by
  induction h <;> omega

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

theorem bbit_isbit (p0 p1 : Int) : isbit (bbit p0 p1) := by
  fun_induction bbit p0 p1 with
  | case1 => exact isbit.one
  | case2 _ _ _ _ ih => exact isbit.dbl ih
  | case3 => exact isbit.one
grind_pattern bbit_isbit => bbit p0 p1

theorem bbit_pos (p0 p1 : Int) : 1 ≤ bbit p0 p1 :=
  isbit_pos (bbit_isbit p0 p1)

-- Two distinct integers disagree AT their lowest differing bit ...
theorem bbit_diff (p0 p1 : Int) :
    p0 ≠ p1 → (zbit p0 (bbit p0 p1) ↔ ¬ zbit p1 (bbit p0 p1)) := by
  fun_induction bbit p0 p1 with
  | case1 h => intro hne; contradiction
  | case2 p0 p1 hne hpar ih =>
    intro _
    have hb := bbit_pos (p0/2) (p1/2)
    have e0 := emod_double p0 (2 * bbit (p0/2) (p1/2)) (by omega)
    have e1 := emod_double p1 (2 * bbit (p0/2) (p1/2)) (by omega)
    have ihh := ih (by omega)
    unfold zbit at *
    omega
  | case3 p0 p1 hne hpar =>
    intro _
    unfold zbit
    omega

-- ... and agree strictly below it.
theorem bbit_agree (p0 p1 : Int) :
    p0 ≠ p1 → mask p0 (bbit p0 p1) = mask p1 (bbit p0 p1) := by
  fun_induction bbit p0 p1 with
  | case1 h => intro hne; contradiction
  | case2 p0 p1 hne hpar ih =>
    intro _
    have hb := bbit_pos (p0/2) (p1/2)
    have e0 := emod_double p0 (bbit (p0/2) (p1/2)) (by omega)
    have e1 := emod_double p1 (bbit (p0/2) (p1/2)) (by omega)
    have ihh := ih (by omega)
    unfold mask at *
    omega
  | case3 p0 p1 hne hpar =>
    intro _
    unfold mask
    omega

-- Integers that differ below bit [b] have their lowest differing bit
-- below [b].
theorem bbit_lt (p0 p1 : Int) :
    ∀ b, isbit b → mask p0 b ≠ mask p1 b → bbit p0 p1 < b := by
  fun_induction bbit p0 p1 with
  | case1 h => intro b hb hm; exact absurd rfl hm
  | case2 p0 p1 hne hpar ih =>
    intro b hb hm
    cases hb with
    | one => exact absurd (by unfold mask; omega) hm
    | @dbl c hc =>
      have hcpos := isbit_pos hc
      have e0 := emod_double p0 c hcpos
      have e1 := emod_double p1 c hcpos
      have : mask (p0/2) c ≠ mask (p1/2) c := by unfold mask at *; omega
      have := ih c hc this
      omega
  | case3 p0 p1 hne hpar =>
    intro b hb hm
    cases hb with
    | one => exact absurd (by unfold mask; omega) hm
    | @dbl c hc => have := isbit_pos hc; omega

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

theorem mask_tele {b b' : Int} (i : Int) (hd : b' ∣ b) :
    mask (mask i b) b' = mask i b' := by
  unfold mask
  exact Int.emod_emod_of_dvd i hd

theorem zbit_mask {b b' : Int} (i : Int) (hd : 2*b' ∣ b) :
    zbit (mask i b) b' ↔ zbit i b' := by
  unfold zbit mask
  rw [Int.emod_emod_of_dvd i hd]

-- Tree model: naive membership over the WHOLE tree, and the Patricia
-- invariant -- at every branch on bit [b] with prefix [p], all keys
-- match [p] below [b], the zero side has bit [b] clear, the one side
-- has it set.
@[grind] def mem : Int -> Vox_Ptrie_t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ _ t0 t1 => mem i t0 ∨ mem i t1

@[grind] def allmatch : Vox_Ptrie_t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => mask j b = p
  | .Branch _ _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind] def allzero : Vox_Ptrie_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind] def allone : Vox_Ptrie_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind] def trie : Vox_Ptrie_t -> Prop
  | .Empty => True
  | .Leaf _ => True
  | .Branch p b t0 t1 =>
      isbit b ∧ mask p b = p ∧
      allmatch t0 p b ∧ allmatch t1 p b ∧
      allzero t0 b ∧ allone t1 b ∧ trie t0 ∧ trie t1

@[grind] def join (p0 : Int) (t0 : Vox_Ptrie_t) (p1 : Int) (t1 : Vox_Ptrie_t) :
    Vox_Ptrie_t :=
  if zbit p0 (bbit p0 p1)
  then .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t0 t1
  else .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t1 t0

@[grind] def insert (i : Int) : Vox_Ptrie_t -> Vox_Ptrie_t
  | .Empty => .Leaf i
  | .Leaf j => if i = j then .Leaf i else join i (.Leaf i) j (.Leaf j)
  | .Branch p b t0 t1 =>
      if mask i b = p then
        if zbit i b then .Branch p b (insert i t0) t1
        else .Branch p b t0 (insert i t1)
      else join i (.Leaf i) p (.Branch p b t0 t1)

-- The invariant makes one-path search complete: a key that fails the
-- prefix test, or sits on the other side of the branching bit, is in
-- NO subtree.
theorem not_mem_mismatch (i p b : Int) (t : Vox_Ptrie_t)
    (h : allmatch t p b) (hm : mask i b ≠ p) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_mismatch => mem i t, allmatch t p b

theorem not_mem_zero (i b : Int) (t : Vox_Ptrie_t)
    (h : allzero t b) (hz : ¬ zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_zero => mem i t, allzero t b

theorem not_mem_one (i b : Int) (t : Vox_Ptrie_t)
    (h : allone t b) (hz : zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_one => mem i t, allone t b

-- Insertion adds exactly one key ...
theorem mem_insert (x y : Int) (t : Vox_Ptrie_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t) := by
  induction t <;> grind
grind_pattern mem_insert => mem y (insert x t)

theorem allmatch_insert (x p b : Int) (t : Vox_Ptrie_t)
    (h : allmatch t p b) (hx : mask x b = p) : allmatch (insert x t) p b := by
  induction t <;> grind
grind_pattern allmatch_insert => allmatch (insert x t) p b

theorem allzero_insert (x b : Int) (t : Vox_Ptrie_t)
    (h : allzero t b) (hx : zbit x b) : allzero (insert x t) b := by
  induction t <;> grind
grind_pattern allzero_insert => allzero (insert x t) b

theorem allone_insert (x b : Int) (t : Vox_Ptrie_t)
    (h : allone t b) (hx : ¬ zbit x b) : allone (insert x t) b := by
  induction t <;> grind
grind_pattern allone_insert => allone (insert x t) b

theorem allmatch_weaken {b b' : Int} (p : Int) (t : Vox_Ptrie_t)
    (h : allmatch t p b) (hd : b' ∣ b) : allmatch t (mask p b') b' := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := mask_tele j hd
    grind [allmatch]
  | Branch q c t0 t1 ih0 ih1 => grind [allmatch]

theorem allzero_of_allmatch {b b' : Int} (p : Int) (t : Vox_Ptrie_t)
    (h : allmatch t p b) (hd : 2*b' ∣ b) (hp : zbit p b') : allzero t b' := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := zbit_mask j hd
    grind [allmatch, allzero]
  | Branch q c t0 t1 ih0 ih1 => grind [allmatch, allzero]

theorem allone_of_allmatch {b b' : Int} (p : Int) (t : Vox_Ptrie_t)
    (h : allmatch t p b) (hd : 2*b' ∣ b) (hp : ¬ zbit p b') : allone t b' := by
  induction t with
  | Empty => trivial
  | Leaf j =>
    have := zbit_mask j hd
    grind [allmatch, allone]
  | Branch q c t0 t1 ih0 ih1 => grind [allmatch, allone]

-- Joining a fresh leaf with a whole subtree keeps the invariant: the
-- branching bit of the two prefixes is a valid bit, both sides match
-- the shared prefix below it, and they part ways exactly at it.
theorem trie_join (x p : Int) (t : Vox_Ptrie_t)
    (ht : trie t) (hxnep : x ≠ p)
    (hall : allmatch t (mask p (bbit x p)) (bbit x p))
    (hz : zbit p (bbit x p) → allzero t (bbit x p))
    (ho : ¬ zbit p (bbit x p) → allone t (bbit x p)) :
    trie (join x (.Leaf x) p t) := by
  have hbb := bbit_isbit x p
  have hdiff := bbit_diff x p hxnep
  have hagree := bbit_agree x p hxnep
  have hidem : mask (mask x (bbit x p)) (bbit x p) = mask x (bbit x p) :=
    mask_tele x (Int.dvd_refl _)
  grind

-- ... and preserves the shape that makes the pruning sound.
theorem trie_insert (x : Int) (t : Vox_Ptrie_t) (h : trie t) :
    trie (insert x t) := by
  induction t with
  | Empty => grind
  | Leaf j =>
    by_cases hxj : x = j
    · grind
    · have := trie_join x j (.Leaf j) (by trivial) hxj
        (by grind) (by grind) (by grind)
      grind
  | Branch p b t0 t1 ih0 ih1 =>
    by_cases hmp : mask x b = p
    · by_cases hzx : zbit x b
      · have h1 : allmatch (insert x t0) p b :=
          allmatch_insert x p b t0 (by grind) hmp
        have h2 : allzero (insert x t0) b :=
          allzero_insert x b t0 (by grind) hzx
        grind
      · have h1 : allmatch (insert x t1) p b :=
          allmatch_insert x p b t1 (by grind) hmp
        have h2 : allone (insert x t1) b :=
          allone_insert x b t1 (by grind) hzx
        grind
    · -- the miss: x parts from this node's prefix at bbit x p, which
      -- sits strictly below b, so the whole subtree rides on one side
      have hib : isbit b := by grind
      have hpb : mask p b = p := by grind
      have hxnep : x ≠ p := by
        intro he; subst he; exact hmp hpb
      have hbb := bbit_isbit x p
      have hlt : bbit x p < b := bbit_lt x p b hib (by grind)
      have hdvd : bbit x p ∣ b :=
        isbit_dvd hib _ hbb (by omega)
      have hdvd2 : 2 * bbit x p ∣ b :=
        isbit_lt_dvd hib _ hbb hlt
      have hall : allmatch (.Branch p b t0 t1) (mask p (bbit x p)) (bbit x p) := by
        have h0 : allmatch t0 p b := by grind
        have h1 : allmatch t1 p b := by grind
        have := allmatch_weaken p t0 h0 hdvd
        have := allmatch_weaken p t1 h1 hdvd
        grind
      have hz : zbit p (bbit x p) → allzero (.Branch p b t0 t1) (bbit x p) := by
        intro hzp
        have h0 : allmatch t0 p b := by grind
        have h1 : allmatch t1 p b := by grind
        have := allzero_of_allmatch p t0 h0 hdvd2 hzp
        have := allzero_of_allmatch p t1 h1 hdvd2 hzp
        grind
      have ho : ¬ zbit p (bbit x p) → allone (.Branch p b t0 t1) (bbit x p) := by
        intro hzp
        have h0 : allmatch t0 p b := by grind
        have h1 : allmatch t1 p b := by grind
        have := allone_of_allmatch p t0 h0 hdvd2 hzp
        have := allone_of_allmatch p t1 h1 hdvd2 hzp
        grind
      have := trie_join x p (.Branch p b t0 t1) h hxnep hall hz ho
      grind
grind_pattern trie_insert => trie (insert x t)
|lean}]

(* The API type is the refined abbreviation: a set IS a tree
   satisfying the Patricia invariant. *)
type set = t{ trie _ }

val empty : set{ _ = Empty }

(* One-path search, proved equal to the model membership that
   quantifies the WHOLE tree: the invariant's lemmas bridge the path
   the code takes to the subtrees it skips. *)
val mem : (i : int) -> (s : set) -> bool{ _ = mem i s }

(* Insertion returns exactly the model's insert; [mem_insert] then
   characterizes the result completely at every client. *)
val insert : (i : int) -> (s : set) -> set{ _ = insert i s && mem i _ }
