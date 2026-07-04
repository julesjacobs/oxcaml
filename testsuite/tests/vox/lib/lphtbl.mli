(* A LINEAR-PROBING hash table over the POLYMORPHIC slices
   (lib/pslice at element type int): parallel keys/values arrays,
   keys NON-NEGATIVE with -1 the EMPTY sentinel, fixed width 8.
   Where the bucketed table's invariant is local (each bucket homed
   at its index), linear probing's is GLOBAL: every stored key is
   reachable from its home slot through an unbroken run of occupied
   slots ([occto], kept quantifier-free by fuel recursion and stated
   per ground slot -- the fixed width makes the whole invariant [wf]
   a ground conjunction the solver can chew).

   The model is FUEL-BOUNDED probing, and the bound is the
   semantics: probing 8 slots scans every slot, so [pf]/[pik]/[piv]
   are total with no load-factor side condition -- a full table
   simply misses (find) or returns unchanged (insert).  Partial
   correctness does the rest: the imperative loop needs no
   termination proof, and its per-arm contracts are EXACTLY the
   Int-fueled unfolding equations ([pfI_unfold] etc.).

   The client-facing theorems: [T1]/[T1_len]/[T1v_len] (insert
   preserves the invariant and both widths), [T2] (the inserted key
   is found -- under [hasfree], which clients DERIVE through the
   free-slot count: [freecnt_pconst] pins a fresh table at 8,
   [freecnt_ins] drops it by at most one per insert, and
   [freecnt_hasfree] converts positivity), [T3] (every other key's
   lookup is untouched -- the frame theorem, where filling a slot
   extends other keys' probe paths and [wf] argues they were never
   stored beyond it), and [T4] (a fresh table misses).
   [client_chain_demo] is the end-to-end pattern-chain check: two
   inserts from fresh, then a hit, by plain grind.

   Nothing here is assumed: every theorem below is proved from the
   list theory imported from Pslice.  The trust boundary is
   pslice's alone. *)

open Pslice

type opt =
  | Missing
  | Found of int

[%%vox.lean {lean|

-- Slot arithmetic for the probe loops: the reflected [mod] is
-- [Int.tmod]; a non-negative dividend lands in [0, 8).
public theorem tmod8_range (i : Int) (h : 0 <= i) : 0 <= Int.tmod i 8 ∧ Int.tmod i 8 < 8 := by
  exact ⟨Int.tmod_nonneg 8 (by omega), Int.tmod_lt_of_pos i (by omega)⟩
grind_pattern tmod8_range => Int.tmod i 8

-- ===== the hash =====
@[grind, expose] public def home (k : Int) : Int := Int.tmod k 8

public theorem home_range (k : Int) (h : 0 <= k) : 0 <= home k ∧ home k < 8 := by
  unfold home
  exact ⟨Int.tmod_nonneg 8 (by omega), Int.tmod_lt_of_pos k (by omega)⟩
grind_pattern home_range => home k

-- ===== probe-find (Nat fuel, linear position, slot = i mod 8) =====
@[grind, expose] public def pf : Nat -> Int -> Int -> List Int -> List Int -> Vox_Lphtbl_opt
  | 0, _, _, _, _ => .Missing
  | n + 1, i, k, ks, vs =>
      if pelem ks (Int.tmod i 8) = -1 then .Missing
      else if pelem ks (Int.tmod i 8) = k then .Found (pelem vs (Int.tmod i 8))
      else pf n (i + 1) k ks vs

-- all-scan variant: like pf but does NOT stop early at an empty slot.
-- On a wf table it agrees with pf, and it frames trivially.
@[grind, expose] public def pfa : Nat -> Int -> Int -> List Int -> List Int -> Vox_Lphtbl_opt
  | 0, _, _, _, _ => .Missing
  | n + 1, i, k, ks, vs =>
      if pelem ks (Int.tmod i 8) = k then .Found (pelem vs (Int.tmod i 8))
      else pfa n (i + 1) k ks vs

-- ===== probe-insert (keys and values as two functions, same slot) =====
@[grind, expose] public def pik : Nat -> Int -> Int -> List Int -> List Int
  | 0, _, _, ks => ks
  | n + 1, i, k, ks =>
      if pelem ks (Int.tmod i 8) = -1 then pupd ks (Int.tmod i 8) k
      else if pelem ks (Int.tmod i 8) = k then pupd ks (Int.tmod i 8) k
      else pik n (i + 1) k ks

@[grind, expose] public def piv : Nat -> Int -> Int -> Int -> List Int -> List Int -> List Int
  | 0, _, _, _, _, vs => vs
  | n + 1, i, k, v, ks, vs =>
      if pelem ks (Int.tmod i 8) = -1 then pupd vs (Int.tmod i 8) v
      else if pelem ks (Int.tmod i 8) = k then pupd vs (Int.tmod i 8) v
      else piv n (i + 1) k v ks vs

-- ===== Int-fuel wrappers + unfolding equations.  OCaml ints reflect
-- as Lean Int, so the imperative loop's per-iteration contract is
-- exactly the [pfI]/[pikI]/[pivI] unfolding below. =====
@[grind, expose] public def pfI (f i k : Int) (ks vs : List Int) : Vox_Lphtbl_opt :=
  pf f.toNat i k ks vs

@[grind, expose] public def pikI (f i k : Int) (ks : List Int) : List Int :=
  pik f.toNat i k ks

@[grind, expose] public def pivI (f i k v : Int) (ks vs : List Int) : List Int :=
  piv f.toNat i k v ks vs

public theorem pfI_unfold (f i k : Int) (ks vs : List Int) :
    pfI f i k ks vs =
      (if f <= 0 then .Missing
       else if pelem ks (Int.tmod i 8) = -1 then .Missing
       else if pelem ks (Int.tmod i 8) = k then .Found (pelem vs (Int.tmod i 8))
       else pfI (f - 1) (i + 1) k ks vs) := by
  unfold pfI
  by_cases hf : f <= 0
  · have h0 : f.toNat = 0 := by omega
    rw [h0]; grind
  · have h1 : f.toNat = (f - 1).toNat + 1 := by omega
    rw [h1]; grind
grind_pattern pfI_unfold => pfI f i k ks vs

public theorem pikI_unfold (f i k : Int) (ks : List Int) :
    pikI f i k ks =
      (if f <= 0 then ks
       else if pelem ks (Int.tmod i 8) = -1 then pupd ks (Int.tmod i 8) k
       else if pelem ks (Int.tmod i 8) = k then pupd ks (Int.tmod i 8) k
       else pikI (f - 1) (i + 1) k ks) := by
  unfold pikI
  by_cases hf : f <= 0
  · have h0 : f.toNat = 0 := by omega
    rw [h0]; grind
  · have h1 : f.toNat = (f - 1).toNat + 1 := by omega
    rw [h1]; grind
grind_pattern pikI_unfold => pikI f i k ks

public theorem pivI_unfold (f i k v : Int) (ks vs : List Int) :
    pivI f i k v ks vs =
      (if f <= 0 then vs
       else if pelem ks (Int.tmod i 8) = -1 then pupd vs (Int.tmod i 8) v
       else if pelem ks (Int.tmod i 8) = k then pupd vs (Int.tmod i 8) v
       else pivI (f - 1) (i + 1) k v ks vs) := by
  unfold pivI
  by_cases hf : f <= 0
  · have h0 : f.toNat = 0 := by omega
    rw [h0]; grind
  · have h1 : f.toNat = (f - 1).toNat + 1 := by omega
    rw [h1]; grind
grind_pattern pivI_unfold => pivI f i k v ks vs

-- ===== top-level operations (stated via the Int-fuel entry points) =====
@[grind, expose] public def pfind (k : Int) (ks vs : List Int) : Vox_Lphtbl_opt :=
  pfI 8 (home k) k ks vs

@[grind, expose] public def pinsk (k v : Int) (ks : List Int) : List Int :=
  pikI 8 (home k) k ks

@[grind, expose] public def pinsv (k v : Int) (ks vs : List Int) : List Int :=
  pivI 8 (home k) k v ks vs

-- bridges to the Nat-fuel forms the model theorems reason about
@[grind] public theorem pfind_pf (k : Int) (ks vs : List Int) :
    pfind k ks vs = pf 8 (home k) k ks vs := by
  unfold pfind pfI; rfl

@[grind] public theorem pinsk_pik (k v : Int) (ks : List Int) :
    pinsk k v ks = pik 8 (home k) k ks := by
  unfold pinsk pikI; rfl

@[grind] public theorem pinsv_piv (k v : Int) (ks vs : List Int) :
    pinsv k v ks vs = piv 8 (home k) k v ks vs := by
  unfold pinsv pivI; rfl

-- ===== landing position and its predicate =====
@[grind, expose] public def plnd : Nat -> Int -> Int -> List Int -> Int
  | 0, i, _, _ => i
  | n + 1, i, k, ks =>
      if pelem ks (Int.tmod i 8) = -1 then i
      else if pelem ks (Int.tmod i 8) = k then i
      else plnd n (i + 1) k ks

@[grind, expose] public def lands : Nat -> Int -> Int -> List Int -> Prop
  | 0, _, _, _ => False
  | n + 1, i, k, ks =>
      pelem ks (Int.tmod i 8) = -1 ∨ pelem ks (Int.tmod i 8) = k
        ∨ lands n (i + 1) k ks

-- ===== insert characterization =====
public theorem plnd_range (f : Nat) (i k : Int) (ks : List Int)
    (hl : lands f i k ks) : i <= plnd f i k ks ∧ plnd f i k ks < i + f := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

public theorem pik_eq (f : Nat) (i k : Int) (ks : List Int) (hl : lands f i k ks) :
    pik f i k ks = pupd ks (Int.tmod (plnd f i k ks) 8) k := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

public theorem piv_eq (f : Nat) (i k v : Int) (ks vs : List Int)
    (hl : lands f i k ks) :
    piv f i k v ks vs = pupd vs (Int.tmod (plnd f i k ks) 8) v := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

public theorem pik_noland (f : Nat) (i k : Int) (ks : List Int)
    (hl : ¬ lands f i k ks) : pik f i k ks = ks := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

public theorem piv_noland (f : Nat) (i k v : Int) (ks vs : List Int)
    (hl : ¬ lands f i k ks) : piv f i k v ks vs = vs := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

public theorem plnd_land (f : Nat) (i k : Int) (ks : List Int) (hl : lands f i k ks) :
    pelem ks (Int.tmod (plnd f i k ks) 8) = -1
      ∨ pelem ks (Int.tmod (plnd f i k ks) 8) = k := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have := ih (i + 1) (by grind); grind

-- find and insert probe the same path from the same home, so a
-- lookup of the just-inserted key lands on the write.
public theorem find_ins_hit (f : Nat) (i k v : Int) (ks vs : List Int)
    (hlen : plen ks = 8) (hlv : plen vs = 8) (hi : 0 <= i) (hk : 0 <= k)
    (hl : lands f i k ks) :
    pf f i k (pik f i k ks) (piv f i k v ks vs) = .Found v := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    have hs0 : (0 : Int) <= Int.tmod i 8 := Int.tmod_nonneg 8 (by omega)
    have hs8 : Int.tmod i 8 < 8 := Int.tmod_lt_of_pos i (by omega)
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · have := pelem_pupd ks (Int.tmod i 8) (Int.tmod i 8) k (by omega) (by omega)
      have := pelem_pupd vs (Int.tmod i 8) (Int.tmod i 8) v (by omega) (by omega)
      grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · have := pelem_pupd ks (Int.tmod i 8) (Int.tmod i 8) k (by omega) (by omega)
        have := pelem_pupd vs (Int.tmod i 8) (Int.tmod i 8) v (by omega) (by omega)
        grind
      · have hrec := ih (i + 1) (by omega) (by grind)
        have hpe := pik_eq n (i + 1) k ks (by grind)
        have hrange := plnd_range n (i + 1) k ks (by grind)
        have hland := plnd_land n (i + 1) k ks (by grind)
        have hl0 : (0 : Int) <= Int.tmod (plnd n (i + 1) k ks) 8 :=
          Int.tmod_nonneg 8 (by omega)
        have hl8 : Int.tmod (plnd n (i + 1) k ks) 8 < 8 :=
          Int.tmod_lt_of_pos (plnd n (i + 1) k ks) (by omega)
        have := pelem_pupd ks (Int.tmod (plnd n (i + 1) k ks) 8) (Int.tmod i 8) k
          (by omega) (by omega)
        grind

-- ===== the invariant (ground, quantifier-free) =====
-- occupied path: from linear position i, until slot mod 8 reaches j,
-- every slot is occupied (non-empty).
@[grind, expose] public def occto : Nat -> Int -> Int -> List Int -> Prop
  | 0, _, _, _ => True
  | n + 1, i, j, ks =>
      if Int.tmod i 8 = j then True
      else pelem ks (Int.tmod i 8) ≠ -1 ∧ occto n (i + 1) j ks

@[grind, expose] public def slotok (i : Int) (ks : List Int) : Prop :=
  pelem ks i = -1 ∨ (0 <= pelem ks i ∧ occto 8 (home (pelem ks i)) i ks)

@[grind, expose] public def wf (ks : List Int) : Prop :=
  plen ks = 8 ∧ slotok 0 ks ∧ slotok 1 ks ∧ slotok 2 ks ∧ slotok 3 ks
    ∧ slotok 4 ks ∧ slotok 5 ks ∧ slotok 6 ks ∧ slotok 7 ks

@[grind, expose] public def hasfree (ks : List Int) : Prop :=
  pelem ks 0 = -1 ∨ pelem ks 1 = -1 ∨ pelem ks 2 = -1 ∨ pelem ks 3 = -1
    ∨ pelem ks 4 = -1 ∨ pelem ks 5 = -1 ∨ pelem ks 6 = -1 ∨ pelem ks 7 = -1

-- occto is monotone: filling a slot with a non-empty value keeps
-- every occupied path occupied.
public theorem occto_upd (f : Nat) (i j s w : Int) (ks : List Int)
    (hs0 : 0 <= s) (hs8 : s < 8) (hlen : plen ks = 8) (hw : w ≠ -1)
    (h : occto f i j ks) : occto f i j (pupd ks s w) := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases hij : Int.tmod i 8 = j
    · grind
    · have he := pelem_pupd ks s (Int.tmod i 8) w hs0 (by omega)
      have := ih (i + 1)
      grind

-- the path the insert walked (home k .. landing) is all occupied.
public theorem occto_land (f : Nat) (i k : Int) (ks : List Int)
    (hk : k ≠ -1) (hl : lands f i k ks) :
    occto f i (Int.tmod (plnd f i k ks) 8) ks := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k
      · grind
      · have hrec := ih (i + 1) (by grind)
        have hland := plnd_land n (i + 1) k ks (by grind)
        grind

-- two linear positions within one turn of the ring have distinct slots
public theorem slot_ne (a b : Int) (ha : 0 <= a) (h1 : a < b) (h2 : b < a + 8) :
    Int.tmod b 8 ≠ Int.tmod a 8 := by
  rw [Int.tmod_eq_emod_of_nonneg (show (0 : Int) <= b by omega),
      Int.tmod_eq_emod_of_nonneg ha]
  omega

-- reaching the start slot is free
public theorem occto_refl (g : Nat) (a : Int) (ks : List Int) :
    occto g a (Int.tmod a 8) ks := by
  cases g <;> grind

-- every strict intermediate on an occupied path is occupied
public theorem occto_seg_occupied (g : Nat) (a b m : Int) (ks : List Int)
    (ha : 0 <= a) (hab : a <= b) (hbw : b < a + 8) (hg : b < a + g)
    (hocc : occto g a (Int.tmod b 8) ks)
    (ham : a <= m) (hmb : m < b) : pelem ks (Int.tmod m 8) ≠ -1 := by
  induction g generalizing a m with
  | zero => omega
  | succ n ih =>
    have hslot := slot_ne a b ha (by omega) (by omega)
    by_cases hma : m = a
    · grind
    · have hrec := ih (a + 1) m (by omega) (by omega) (by omega) (by omega)
        (by grind) (by omega) (by omega)
      grind

-- extend an occupied path by one already-occupied slot
public theorem occto_extend (g : Nat) (a b : Int) (ks : List Int)
    (ha : 0 <= a) (hab : a <= b) (hbw : b + 1 < a + 8) (hg : b + 1 < a + g)
    (hocc : occto g a (Int.tmod b 8) ks)
    (hbocc : pelem ks (Int.tmod b 8) ≠ -1) :
    occto g a (Int.tmod (b + 1) 8) ks := by
  induction g generalizing a with
  | zero => omega
  | succ n ih =>
    have hslotb1 := slot_ne a (b + 1) ha (by omega) (by omega)
    by_cases hab' : a = b
    · have := occto_refl n (a + 1) ks
      grind
    · have hslotb := slot_ne a b ha (by omega) (by omega)
      have hrec := ih (a + 1) (by omega) (by omega) (by omega) (by omega)
        (by grind)
      grind

-- inserting k at slot s (whose landing path is occupied) preserves
-- the per-slot invariant at every slot.
public theorem slotok_ins (j s k : Int) (ks : List Int)
    (hlen : plen ks = 8) (hs0 : 0 <= s) (hs8 : s < 8)
    (hj0 : 0 <= j) (hj8 : j < 8) (hk : 0 <= k)
    (hold : slotok j ks) (hocc : occto 8 (home k) s ks) :
    slotok j (pupd ks s k) := by
  by_cases hjs : j = s
  · subst hjs
    have he := pelem_pupd ks j j k hj0 (by omega)
    have hm := occto_upd 8 (home k) j j k ks hj0 hj8 hlen (by omega) hocc
    grind
  · have he := pelem_pupd ks s j k hs0 (by omega)
    have hm : occto 8 (home (pelem ks j)) j ks
        → occto 8 (home (pelem ks j)) j (pupd ks s k) :=
      fun hh => occto_upd 8 (home (pelem ks j)) j s k ks hs0 hs8 hlen (by omega) hh
    grind

-- ===== the empty table (constant -1 keys array) =====
public theorem wf_empty (ks : List Int) (hlen : plen ks = 8) (hc : pconst ks (-1)) :
    wf ks := by
  have e0 := pelem_pconst ks (-1) 0 hc (by omega) (by omega)
  have e1 := pelem_pconst ks (-1) 1 hc (by omega) (by omega)
  have e2 := pelem_pconst ks (-1) 2 hc (by omega) (by omega)
  have e3 := pelem_pconst ks (-1) 3 hc (by omega) (by omega)
  have e4 := pelem_pconst ks (-1) 4 hc (by omega) (by omega)
  have e5 := pelem_pconst ks (-1) 5 hc (by omega) (by omega)
  have e6 := pelem_pconst ks (-1) 6 hc (by omega) (by omega)
  have e7 := pelem_pconst ks (-1) 7 hc (by omega) (by omega)
  grind

-- probing 8 slots from any home covers the whole ring, so a free
-- slot guarantees the probe lands.
public theorem hf_lands (k : Int) (ks : List Int) (hk : 0 <= k) (hlen : plen ks = 8)
    (hf : hasfree ks) : lands 8 (home k) k ks := by
  obtain ⟨hr0, hr8⟩ := home_range k hk
  have hcases : home k = 0 ∨ home k = 1 ∨ home k = 2 ∨ home k = 3
      ∨ home k = 4 ∨ home k = 5 ∨ home k = 6 ∨ home k = 7 := by omega
  rcases hcases with h | h | h | h | h | h | h | h <;> rw [h] <;> grind

-- ===== T2 : the inserted key is found =====
public theorem T2 (k v : Int) (ks vs : List Int)
    (hwf : wf ks) (hlv : plen vs = 8) (hk : 0 <= k) (hfr : hasfree ks) :
    pfind k (pinsk k v ks) (pinsv k v ks vs) = .Found v := by
  have hlen : plen ks = 8 := by grind
  have hl := hf_lands k ks hk hlen hfr
  have hr := home_range k hk
  have := find_ins_hit 8 (home k) k v ks vs hlen hlv (by grind) hk hl
  grind

-- ===== T1 : insertion preserves the invariant =====
public theorem T1 (k v : Int) (ks : List Int) (hwf : wf ks) (hk : 0 <= k) :
    wf (pinsk k v ks) := by
  by_cases hl : lands 8 (home k) k ks
  · have hlen : plen ks = 8 := by grind
    have hr := home_range k hk
    have hpe := pik_eq 8 (home k) k ks hl
    have hrange := plnd_range 8 (home k) k ks hl
    have hs0 : (0 : Int) <= Int.tmod (plnd 8 (home k) k ks) 8 :=
      Int.tmod_nonneg 8 (by omega)
    have hs8 : Int.tmod (plnd 8 (home k) k ks) 8 < 8 :=
      Int.tmod_lt_of_pos (plnd 8 (home k) k ks) (by omega)
    have hocc := occto_land 8 (home k) k ks (by omega) hl
    have g0 := slotok_ins 0 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g1 := slotok_ins 1 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g2 := slotok_ins 2 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g3 := slotok_ins 3 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g4 := slotok_ins 4 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g5 := slotok_ins 5 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g6 := slotok_ins 6 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    have g7 := slotok_ins 7 (Int.tmod (plnd 8 (home k) k ks) 8) k ks hlen hs0 hs8
      (by omega) (by omega) hk (by grind) hocc
    grind
  · have := pik_noland 8 (home k) k ks hl
    grind

-- length is preserved (bundled in wf, restated for clients)
public theorem T1_len (k v : Int) (ks : List Int) (hlen : plen ks = 8) :
    plen (pinsk k v ks) = 8 := by
  by_cases hl : lands 8 (home k) k ks
  · have hpe := pik_eq 8 (home k) k ks hl
    grind
  · have := pik_noland 8 (home k) k ks hl
    grind

-- values-array width is preserved too (rides every client signature,
-- and lets a SECOND insert re-satisfy T2/T3's [plen vs = 8]).
public theorem T1v_len (k v : Int) (ks vs : List Int) (hlv : plen vs = 8) :
    plen (pinsv k v ks vs) = 8 := by
  by_cases hl : lands 8 (home k) k ks
  · have hpv := piv_eq 8 (home k) k v ks vs hl
    grind
  · have := piv_noland 8 (home k) k v ks vs hl
    grind

-- ===== T4 : lookup in the empty table misses =====
public theorem T4 (k : Int) (ks vs : List Int)
    (hlen : plen ks = 8) (hc : pconst ks (-1)) (hk : 0 <= k) :
    pfind k ks vs = .Missing := by
  obtain ⟨hr0, hr8⟩ := home_range k hk
  have ht : Int.tmod (home k) 8 = home k := by
    rw [Int.tmod_eq_emod_of_nonneg hr0]; omega
  have he := pelem_pconst ks (-1) (home k) hc hr0 (by omega)
  grind

-- ===== T3 : every other key is unchanged =====
-- pfa frames trivially: the one changed slot holds k, and both the
-- old and new contents there differ from k', so the k'-scan skips it.
public theorem pfa_frame (f : Nat) (i s k k' v : Int) (ks vs : List Int)
    (hs0 : 0 <= s) (hs8 : s < 8) (hlen : plen ks = 8) (hlv : plen vs = 8)
    (hsk : pelem ks s ≠ k') (hkk' : k ≠ k') :
    pfa f i k' (pupd ks s k) (pupd vs s v) = pfa f i k' ks vs := by
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    have hek := pelem_pupd ks s (Int.tmod i 8) k hs0 (by omega)
    have hev := pelem_pupd vs s (Int.tmod i 8) v hs0 (by omega)
    have := ih (i + 1)
    grind

-- from a reachable empty slot, the all-scan finds no k' (any occupied
-- slot ahead would have an occupied path through the empty slot).
public theorem pfa_miss (g : Nat) (a i k' : Int) (ks vs : List Int)
    (hk' : 0 <= k') (hwf : wf ks) (hi : home k' <= i)
    (hemp : pelem ks (Int.tmod i 8) = -1)
    (ha : i <= a) (haw : a + g <= home k' + 8) :
    pfa g a k' ks vs = .Missing := by
  have hr := home_range k' hk'
  induction g generalizing a with
  | zero => grind
  | succ n ih =>
    have hstep := ih (a + 1) (by omega) (by omega)
    by_cases hkey : pelem ks (Int.tmod a 8) = k'
    · exfalso
      have hslotok : slotok (Int.tmod a 8) ks := by
        have hsa0 : (0 : Int) <= Int.tmod a 8 := Int.tmod_nonneg 8 (by omega)
        have hsa8 : Int.tmod a 8 < 8 := Int.tmod_lt_of_pos a (by omega)
        have hlen : plen ks = 8 := by grind
        have hc : Int.tmod a 8 = 0 ∨ Int.tmod a 8 = 1 ∨ Int.tmod a 8 = 2
            ∨ Int.tmod a 8 = 3 ∨ Int.tmod a 8 = 4 ∨ Int.tmod a 8 = 5
            ∨ Int.tmod a 8 = 6 ∨ Int.tmod a 8 = 7 := by omega
        rcases hc with h | h | h | h | h | h | h | h <;> rw [h] <;> grind
      have hocc : occto 8 (home k') (Int.tmod a 8) ks := by grind
      by_cases hia : i = a
      · grind
      · have := occto_seg_occupied 8 (home k') a i ks (by omega) (by omega)
          (by omega) (by omega) (by grind) (by omega) (by omega)
        grind
    · grind

-- on a wf table, the early-stopping scan equals the all-scan.
public theorem pf_eq_pfa_gen (f : Nat) (i k' : Int) (ks vs : List Int)
    (hk' : 0 <= k') (hwf : wf ks) (hi : home k' <= i)
    (hreach : occto 8 (home k') (Int.tmod i 8) ks)
    (hf : i + f <= home k' + 8) :
    pf f i k' ks vs = pfa f i k' ks vs := by
  have hr := home_range k' hk'
  induction f generalizing i with
  | zero => grind
  | succ n ih =>
    by_cases h1 : pelem ks (Int.tmod i 8) = -1
    · have hmiss := pfa_miss (n + 1) i i k' ks vs hk' hwf hi h1 (by omega) (by omega)
      grind
    · by_cases h2 : pelem ks (Int.tmod i 8) = k'
      · grind
      · have hreach' : occto 8 (home k') (Int.tmod (i + 1) 8) ks := by
          by_cases hw : i + 1 < home k' + 8
          · exact occto_extend 8 (home k') i ks (by omega) hi (by omega)
              (by omega) hreach h1
          · have heq : Int.tmod (i + 1) 8 = Int.tmod (home k') 8 := by
              rw [Int.tmod_eq_emod_of_nonneg (by omega),
                  Int.tmod_eq_emod_of_nonneg (by omega)]
              omega
            rw [heq]; exact occto_refl 8 (home k') ks
        have hstep := ih (i + 1) (by omega) hreach' (by omega)
        grind

public theorem pf_eq_pfa (k' : Int) (ks vs : List Int)
    (hk' : 0 <= k') (hwf : wf ks) :
    pf 8 (home k') k' ks vs = pfa 8 (home k') k' ks vs := by
  have hr := home_range k' hk'
  exact pf_eq_pfa_gen 8 (home k') k' ks vs hk' hwf (by omega)
    (occto_refl 8 (home k') ks) (by omega)

public theorem T3 (k k' v : Int) (ks vs : List Int)
    (hwf : wf ks) (hk : 0 <= k) (hk' : 0 <= k') (hne : k' ≠ k) (hlv : plen vs = 8) :
    pfind k' (pinsk k v ks) (pinsv k v ks vs) = pfind k' ks vs := by
  by_cases hl : lands 8 (home k) k ks
  · have hlen : plen ks = 8 := by grind
    have hr := home_range k hk
    have hpe := pik_eq 8 (home k) k ks hl
    have hpv := piv_eq 8 (home k) k v ks vs hl
    have hland := plnd_land 8 (home k) k ks hl
    have hrange := plnd_range 8 (home k) k ks hl
    have hs0 : (0 : Int) <= Int.tmod (plnd 8 (home k) k ks) 8 :=
      Int.tmod_nonneg 8 (by omega)
    have hs8 : Int.tmod (plnd 8 (home k) k ks) 8 < 8 :=
      Int.tmod_lt_of_pos (plnd 8 (home k) k ks) (by omega)
    have hwf' : wf (pupd ks (Int.tmod (plnd 8 (home k) k ks) 8) k) := by
      have := T1 k v ks hwf hk; grind
    have hA := pf_eq_pfa k' (pupd ks (Int.tmod (plnd 8 (home k) k ks) 8) k)
      (pupd vs (Int.tmod (plnd 8 (home k) k ks) 8) v) hk' hwf'
    have hB := pfa_frame 8 (home k') (Int.tmod (plnd 8 (home k) k ks) 8) k k' v ks vs
      hs0 hs8 hlen hlv (by grind) (by omega)
    have hC := pf_eq_pfa k' ks vs hk' hwf
    grind
  · have h1 := pik_noland 8 (home k) k ks hl
    have h2 := piv_noland 8 (home k) k v ks vs hl
    grind

-- ===== free-slot count: makes [hasfree] CHAIN through inserts, so a
-- client that starts from a fresh (pconst) table can keep discharging
-- T2 after several inserts without ever evaluating [pelem] on an
-- opaque post-insert list. =====
@[grind, expose] public def freecnt (ks : List Int) : Int :=
  (if pelem ks 0 = -1 then 1 else 0) + (if pelem ks 1 = -1 then 1 else 0)
  + (if pelem ks 2 = -1 then 1 else 0) + (if pelem ks 3 = -1 then 1 else 0)
  + (if pelem ks 4 = -1 then 1 else 0) + (if pelem ks 5 = -1 then 1 else 0)
  + (if pelem ks 6 = -1 then 1 else 0) + (if pelem ks 7 = -1 then 1 else 0)

public theorem freecnt_hasfree (ks : List Int) (h : 0 < freecnt ks) : hasfree ks := by
  grind

public theorem freecnt_pconst (ks : List Int) (hc : pconst ks (-1)) (hlen : plen ks = 8) :
    freecnt ks = 8 := by
  have e0 := pelem_pconst ks (-1) 0 hc (by omega) (by omega)
  have e1 := pelem_pconst ks (-1) 1 hc (by omega) (by omega)
  have e2 := pelem_pconst ks (-1) 2 hc (by omega) (by omega)
  have e3 := pelem_pconst ks (-1) 3 hc (by omega) (by omega)
  have e4 := pelem_pconst ks (-1) 4 hc (by omega) (by omega)
  have e5 := pelem_pconst ks (-1) 5 hc (by omega) (by omega)
  have e6 := pelem_pconst ks (-1) 6 hc (by omega) (by omega)
  have e7 := pelem_pconst ks (-1) 7 hc (by omega) (by omega)
  grind

-- a structural free-slot count: [freecnt] on a length-8 list equals
-- [fcount], and the decrement bound is a cheap induction on [fcount]
-- (the flat 8-way [freecnt] alone makes grind split 2^8 ways).
@[grind, expose] public def fcount : List Int -> Int
  | [] => 0
  | x :: t => (if x = -1 then 1 else 0) + fcount t

public theorem fcount_pupd_ge (ks : List Int) (s k : Int) (hk : k ≠ -1) (hs : 0 <= s) :
    fcount ks - 1 <= fcount (pupd ks s k) := by
  induction ks generalizing s with
  | nil => grind
  | cons x t ih => have := ih (s - 1); grind

public theorem freecnt_eq_fcount (ks : List Int) (hlen : plen ks = 8) :
    freecnt ks = fcount ks := by
  rcases ks with _ | ⟨a0, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a1, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a2, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a3, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a4, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a5, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a6, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a7, ks⟩; · simp [plen] at hlen
  rcases ks with _ | ⟨a8, ks⟩
  · simp [freecnt, fcount, pelem]
    omega
  · have := plen_nonneg ks; simp only [plen] at hlen; omega

-- an insert drops at most one free slot (it writes k >= 0 at one slot;
-- a no-land insert leaves the list untouched).
public theorem freecnt_ins (k v : Int) (ks : List Int) (hk : 0 <= k) (hlen : plen ks = 8) :
    freecnt ks - 1 <= freecnt (pinsk k v ks) := by
  by_cases hl : lands 8 (home k) k ks
  · have hr := home_range k hk
    have hpe := pik_eq 8 (home k) k ks hl
    have hrange := plnd_range 8 (home k) k ks hl
    have hs0 : (0 : Int) <= Int.tmod (plnd 8 (home k) k ks) 8 :=
      Int.tmod_nonneg 8 (by omega)
    have hlink : pinsk k v ks = pupd ks (Int.tmod (plnd 8 (home k) k ks) 8) k := by
      rw [pinsk_pik]; exact hpe
    have hlenu : plen (pupd ks (Int.tmod (plnd 8 (home k) k ks) 8) k) = 8 := by
      rw [plen_pupd]; exact hlen
    have e1 := freecnt_eq_fcount ks hlen
    have e2 := freecnt_eq_fcount
      (pupd ks (Int.tmod (plnd 8 (home k) k ks) 8) k) hlenu
    have e3 := fcount_pupd_ge ks (Int.tmod (plnd 8 (home k) k ks) 8) k (by omega) hs0
    rw [hlink]; omega
  · have hlink : pinsk k v ks = ks := by
      rw [pinsk_pik]; exact pik_noland 8 (home k) k ks hl
    rw [hlink]; omega

-- ===== sanity: the Int-fuel unfolding fires under grind on the
-- SYMBOLIC per-arm goals an OCaml probe loop generates (each arm is a
-- single [grind] step via the [pfI_unfold]/[pikI_unfold]/[pivI_unfold]
-- patterns) =====
public theorem pfI_arm_empty (f i k : Int) (ks vs : List Int)
    (h0 : 0 < f) (he : pelem ks (Int.tmod i 8) = -1) :
    pfI f i k ks vs = .Missing := by grind

public theorem pfI_arm_found (f i k : Int) (ks vs : List Int)
    (h0 : 0 < f) (hne : pelem ks (Int.tmod i 8) ≠ -1)
    (hk : pelem ks (Int.tmod i 8) = k) :
    pfI f i k ks vs = .Found (pelem vs (Int.tmod i 8)) := by grind

public theorem pfI_arm_step (f i k : Int) (ks vs : List Int)
    (h0 : 0 < f) (hne : pelem ks (Int.tmod i 8) ≠ -1)
    (hnk : pelem ks (Int.tmod i 8) ≠ k) :
    pfI f i k ks vs = pfI (f - 1) (i + 1) k ks vs := by grind

public theorem pfI_arm_done (f i k : Int) (ks vs : List Int) (h0 : f <= 0) :
    pfI f i k ks vs = .Missing := by grind

public theorem pikI_arm_land (f i k : Int) (ks : List Int)
    (h0 : 0 < f) (he : pelem ks (Int.tmod i 8) = -1) :
    pikI f i k ks = pupd ks (Int.tmod i 8) k := by grind

public theorem pikI_arm_step (f i k : Int) (ks : List Int)
    (h0 : 0 < f) (hne : pelem ks (Int.tmod i 8) ≠ -1)
    (hnk : pelem ks (Int.tmod i 8) ≠ k) :
    pikI f i k ks = pikI (f - 1) (i + 1) k ks := by grind

public theorem pivI_arm_land (f i k v : Int) (ks vs : List Int)
    (h0 : 0 < f) (he : pelem ks (Int.tmod i 8) = -1) :
    pivI f i k v ks vs = pupd vs (Int.tmod i 8) v := by grind

public theorem pivI_arm_step (f i k v : Int) (ks vs : List Int)
    (h0 : 0 < f) (hne : pelem ks (Int.tmod i 8) ≠ -1)
    (hnk : pelem ks (Int.tmod i 8) ≠ k) :
    pivI f i k v ks vs = pivI (f - 1) (i + 1) k v ks vs := by grind

-- ===== E-matching interface for client VCs =====
grind_pattern T1 => pinsk k v ks
grind_pattern T1_len => pinsk k v ks
grind_pattern T1v_len => pinsv k v ks vs
grind_pattern T2 => pfind k (pinsk k v ks) (pinsv k v ks vs)
grind_pattern T3 => pfind k' (pinsk k v ks) (pinsv k v ks vs)
grind_pattern T4 => pfind k ks vs, pconst ks (-1)
grind_pattern wf_empty => pconst ks (-1)
grind_pattern freecnt_hasfree => hasfree ks
grind_pattern freecnt_pconst => freecnt ks, pconst ks (-1)
grind_pattern freecnt_ins => freecnt (pinsk k v ks)

-- ===== end-to-end client chain: two nested inserts starting from a
-- fresh (pconst) table, then a hit lookup of the last key -- [hasfree]
-- is DERIVED at each level (freecnt_pconst = 8, dropped >= 1 per insert
-- via freecnt_ins, still > 0), never assumed. Discharged by plain
-- grind, exercising the whole pattern chain a client would hit. =====
public theorem client_chain_demo (k1 v1 k2 v2 : Int) (ks0 vs0 : List Int)
    (hc : pconst ks0 (-1)) (hlen : plen ks0 = 8) (hlv : plen vs0 = 8)
    (hk1 : 0 <= k1) (hk2 : 0 <= k2) :
    pfind k2 (pinsk k2 v2 (pinsk k1 v1 ks0))
      (pinsv k2 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0)) = .Found v2 := by
  grind

-- the MISS after two inserts: a T3 -> T3 -> T4 chain.  Proved by
-- explicit hops (grind will not chain three instantiations at the
-- nested terms on its own), then exported as one pattern so a client's
-- two-insert miss VC closes in a single grind.
public theorem client_chain_miss (k1 v1 k2 v2 k3 : Int) (ks0 vs0 : List Int)
    (hc : pconst ks0 (-1)) (hlen : plen ks0 = 8) (hlv : plen vs0 = 8)
    (hk1 : 0 <= k1) (hk2 : 0 <= k2) (hk3 : 0 <= k3)
    (hne1 : k3 ≠ k1) (hne2 : k3 ≠ k2) :
    pfind k3 (pinsk k2 v2 (pinsk k1 v1 ks0))
      (pinsv k2 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0)) = .Missing := by
  have h1 : wf ks0 := wf_empty ks0 hlen hc
  have hw1 := T1 k1 v1 ks0 h1 hk1
  have hlv1 := T1v_len k1 v1 ks0 vs0 hlv
  have hA := T3 k2 k3 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0)
    hw1 hk2 hk3 hne2 hlv1
  have hB := T3 k1 k3 v1 ks0 vs0 h1 hk1 hk3 hne1 hlv
  have hC := T4 k3 ks0 vs0 hlen hc hk3
  grind
grind_pattern client_chain_miss =>
  pfind k3 (pinsk k2 v2 (pinsk k1 v1 ks0))
    (pinsv k2 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0))

-- acceptance: the client's actual miss VC (equality noise from pcts
-- ghosts and tuple projections) closes by plain grind via the pattern.
public theorem demo_miss_vc (hit3 hit11 miss : Vox_Lphtbl_opt)
    (ks vs ks2 vs2 ks3 vs3 ks4 vs4 ks5 vs5 ks6 vs6 : List Int)
    (h0 : miss = pfind 4 ks2 vs2)
    (h1 : ks = ks2 ∧ wf ks) (h2 : vs = vs2 ∧ plen vs = 8)
    (h3 : hit11 = pfind 11 ks3 vs3)
    (h4 : ks2 = ks3 ∧ wf ks2) (h5 : vs2 = vs3 ∧ plen vs2 = 8)
    (h6 : hit3 = pfind 3 ks4 vs4)
    (h7 : ks3 = ks4 ∧ wf ks3) (h8 : vs3 = vs4 ∧ plen vs3 = 8)
    (h9 : ks4 = pinsk 11 5 ks5 ∧ wf ks4)
    (h10 : vs4 = pinsv 11 5 ks5 vs5 ∧ plen vs4 = 8)
    (h11 : ks5 = pinsk 3 7 ks6 ∧ wf ks5)
    (h12 : vs5 = pinsv 3 7 ks6 vs6 ∧ plen vs5 = 8)
    (h13 : wf ks6 ∧ pconst ks6 (-1))
    (h14 : plen vs6 = 8) :
    miss = .Missing := by grind
|lean}]

(* A fresh table: keys all -1 (pconst pins the free count at 8; wf
   holds vacuously), values all 0. *)
val create :
  unit ->
  (int varr{ wf (pcts _) && pconst (pcts _) (-1) }
   * int varr{ plen (pcts _) = 8 }) @ unique

(* IN-PLACE probe-insert: hash the key, walk to the first free or
   matching slot, write both arrays through their loans.  The
   residuals are exactly the model insert [pinsk]/[pinsv]; on a full
   table (freecnt 0) the model -- and the code -- leave the table
   unchanged. *)
val add :
  (k : int{ 0 <= _ }) -> (v : int) ->
  (ks : int varr{ wf (pcts _) }) @ unique ->
  (vs : int varr{ plen (pcts _) = 8 }) @ unique ->
  (int varr{ pcts _ = pinsk k v (pcts ks) && wf (pcts _) }
   * int varr{ pcts _ = pinsv k v (pcts ks) (pcts vs) && plen (pcts _) = 8 })
    @ unique

(* Probe-lookup, equal to the model scan [pfind]; both arrays come
   back untouched. *)
val find :
  (k : int{ 0 <= _ }) ->
  (ks : int varr{ wf (pcts _) }) @ unique ->
  (vs : int varr{ plen (pcts _) = 8 }) @ unique ->
  (opt{ _ = pfind k (pcts ks) (pcts vs) }
   * int varr{ pcts _ = pcts ks && wf (pcts _) }
   * int varr{ pcts _ = pcts vs && plen (pcts _) = 8 }) @ unique
