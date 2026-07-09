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
-- ===== the hash =====
@[grind, expose] public def home (k : Int) : Int := Int.tmod k 8
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
-- ===== top-level operations (stated via the Int-fuel entry points) =====
@[grind, expose] public def pfind (k : Int) (ks vs : List Int) : Vox_Lphtbl_opt :=
  pfI 8 (home k) k ks vs
@[grind, expose] public def pinsk (k v : Int) (ks : List Int) : List Int :=
  pikI 8 (home k) k ks
@[grind, expose] public def pinsv (k v : Int) (ks vs : List Int) : List Int :=
  pivI 8 (home k) k v ks vs
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
-- ===== the empty table (constant -1 keys array) =====
public axiom wf_empty (ks : List Int) (hlen : plen ks = 8) (hc : pconst ks (-1)) :
    wf ks
-- ===== T2 : the inserted key is found =====
public axiom T2 (k v : Int) (ks vs : List Int)
    (hwf : wf ks) (hlv : plen vs = 8) (hk : 0 <= k) (hfr : hasfree ks) :
    pfind k (pinsk k v ks) (pinsv k v ks vs) = .Found v
-- ===== T1 : insertion preserves the invariant =====
public axiom T1 (k v : Int) (ks : List Int) (hwf : wf ks) (hk : 0 <= k) :
    wf (pinsk k v ks)
-- length is preserved (bundled in wf, restated for clients)
public axiom T1_len (k v : Int) (ks : List Int) (hlen : plen ks = 8) :
    plen (pinsk k v ks) = 8
-- values-array width is preserved too (rides every client signature,
-- and lets a SECOND insert re-satisfy T2/T3's [plen vs = 8]).
public axiom T1v_len (k v : Int) (ks vs : List Int) (hlv : plen vs = 8) :
    plen (pinsv k v ks vs) = 8
-- ===== T4 : lookup in the empty table misses =====
public axiom T4 (k : Int) (ks vs : List Int)
    (hlen : plen ks = 8) (hc : pconst ks (-1)) (hk : 0 <= k) :
    pfind k ks vs = .Missing
public axiom T3 (k k' v : Int) (ks vs : List Int)
    (hwf : wf ks) (hk : 0 <= k) (hk' : 0 <= k') (hne : k' ≠ k) (hlv : plen vs = 8) :
    pfind k' (pinsk k v ks) (pinsv k v ks vs) = pfind k' ks vs
-- ===== free-slot count: makes [hasfree] CHAIN through inserts, so a
-- client that starts from a fresh (pconst) table can keep discharging
-- T2 after several inserts without ever evaluating [pelem] on an
-- opaque post-insert list. =====
@[grind, expose] public def freecnt (ks : List Int) : Int :=
  (if pelem ks 0 = -1 then 1 else 0) + (if pelem ks 1 = -1 then 1 else 0)
  + (if pelem ks 2 = -1 then 1 else 0) + (if pelem ks 3 = -1 then 1 else 0)
  + (if pelem ks 4 = -1 then 1 else 0) + (if pelem ks 5 = -1 then 1 else 0)
  + (if pelem ks 6 = -1 then 1 else 0) + (if pelem ks 7 = -1 then 1 else 0)
public axiom freecnt_hasfree (ks : List Int) (h : 0 < freecnt ks) : hasfree ks
public axiom freecnt_pconst (ks : List Int) (hc : pconst ks (-1)) (hlen : plen ks = 8) :
    freecnt ks = 8
-- a structural free-slot count: [freecnt] on a length-8 list equals
-- [fcount], and the decrement bound is a cheap induction on [fcount]
-- (the flat 8-way [freecnt] alone makes grind split 2^8 ways).
@[grind, expose] public def fcount : List Int -> Int
  | [] => 0
  | x :: t => (if x = -1 then 1 else 0) + fcount t
-- an insert drops at most one free slot (it writes k >= 0 at one slot;
-- a no-land insert leaves the list untouched).
public axiom freecnt_ins (k v : Int) (ks : List Int) (hk : 0 <= k) (hlen : plen ks = 8) :
    freecnt ks - 1 <= freecnt (pinsk k v ks)
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
-- the MISS after two inserts: a T3 -> T3 -> T4 chain.  Proved by
-- explicit hops (grind will not chain three instantiations at the
-- nested terms on its own), then exported as one pattern so a client's
-- two-insert miss VC closes in a single grind.
public axiom client_chain_miss (k1 v1 k2 v2 k3 : Int) (ks0 vs0 : List Int)
    (hc : pconst ks0 (-1)) (hlen : plen ks0 = 8) (hlv : plen vs0 = 8)
    (hk1 : 0 <= k1) (hk2 : 0 <= k2) (hk3 : 0 <= k3)
    (hne1 : k3 ≠ k1) (hne2 : k3 ≠ k2) :
    pfind k3 (pinsk k2 v2 (pinsk k1 v1 ks0))
      (pinsv k2 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0)) = .Missing
grind_pattern client_chain_miss =>
  pfind k3 (pinsk k2 v2 (pinsk k1 v1 ks0))
    (pinsv k2 v2 (pinsk k1 v1 ks0) (pinsv k1 v1 ks0 vs0))
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
