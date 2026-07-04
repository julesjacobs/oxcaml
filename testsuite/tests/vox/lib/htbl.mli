(* A fixed-width hash table over int keys and int values, behind a
   SPECCED interface.  The block below is the whole logical story --
   the reflected model (per-bucket association lists, the whole-table
   scan [tfind], the hash [index] and its range, the bucket invariant
   [twf]) and its theorems, proved once here and carried to the
   implementation and every client through this .cmi.

   The point of the exercise is [tfind_eq_jump]: a lookup that hashes
   the key and inspects exactly ONE bucket equals a scan of the WHOLE
   table.  The bucket invariant -- every key in bucket [p] hashes to
   [p] -- rides the table's refinement and makes the skipped buckets
   provably irrelevant, exactly as the ordering invariant makes BST
   search one-path complete.

   Keys are NON-NEGATIVE (the refinement on every key parameter):
   reflected [mod] is T-mod, so a negative key would hash to a
   negative bucket; the range lemma [index_range] holds only for
   [0 <= k].  Nothing anywhere is assumed. *)

type bucket =
  | BNil
  | BCons of int * int * bucket

type table =
  | TNil
  | TCons of bucket * table

type opt =
  | Missing
  | Found of int

[%%vox.lean {lean|
-- The table has [width] buckets; a key hashes to [index k].  OCaml's
-- [mod] is [Int.tmod] (truncating), so for non-negative keys the
-- bucket is in range -- proved, not assumed.
@[grind] def index (k : Int) : Int := Int.tmod k 8

theorem index_range (k : Int) (h : 0 <= k) : 0 <= index k ∧ index k < 8 := by
  unfold index
  exact ⟨Int.tmod_nonneg 8 (by omega), Int.tmod_lt_of_pos k (by omega)⟩
grind_pattern index_range => index k

-- Bucket = association list.  [bfind] returns the first value bound
-- to [k]; [badd] inserts or replaces.
@[grind] def bfind : Int -> Vox_Htbl_bucket -> Vox_Htbl_opt
  | _, .BNil => .Missing
  | k, .BCons k' v r => if k = k' then .Found v else bfind k r

@[grind] def badd : Int -> Int -> Vox_Htbl_bucket -> Vox_Htbl_bucket
  | k, v, .BNil => .BCons k v .BNil
  | k, v, .BCons k' v' r =>
      if k = k' then .BCons k v r else .BCons k' v' (badd k v r)

-- Bucket invariant: every key in the bucket hashes to [p].
@[grind] def bhome : Vox_Htbl_bucket -> Int -> Prop
  | .BNil, _ => True
  | .BCons k _ r, p => index k = p ∧ bhome r p

-- Whole-table scan: the first bucket (in spine order) that binds [k].
@[grind] def tfind : Int -> Vox_Htbl_table -> Vox_Htbl_opt
  | _, .TNil => .Missing
  | k, .TCons b r =>
      match bfind k b with
      | .Found v => .Found v
      | .Missing => tfind k r

-- Spine length.
@[grind] def tlen : Vox_Htbl_table -> Int
  | .TNil => 0
  | .TCons _ r => 1 + tlen r

-- Direct access to the bucket at spine offset [o] (junk .BNil off
-- the end, which reads as an empty bucket -- sound).
@[grind] def tnth : Vox_Htbl_table -> Int -> Vox_Htbl_bucket
  | .TNil, _ => .BNil
  | .TCons b r, o => if o <= 0 then b else tnth r (o - 1)

-- Point update of the spine at offset [o].
@[grind] def tset : Vox_Htbl_table -> Int -> Vox_Htbl_bucket -> Vox_Htbl_table
  | .TNil, _, _ => .TNil
  | .TCons b r, o, nb => if o <= 0 then .TCons nb r else .TCons b (tset r (o - 1) nb)

-- Well-formedness from base position [p]: bucket at [p] is [bhome p],
-- the next at [p+1], and so on.
@[grind] def twf : Vox_Htbl_table -> Int -> Prop
  | .TNil, _ => True
  | .TCons b r, p => bhome b p ∧ twf r (p + 1)

-- The model-level insert: replace the home bucket with [badd].
@[grind] def madd (k v : Int) (t : Vox_Htbl_table) : Vox_Htbl_table :=
  tset t (index k) (badd k v (tnth t (index k)))

-- The fixed empty table -- eight empty buckets -- satisfies the
-- invariant and has the fixed width.  grind does not evaluate the
-- recursive [twf]/[tlen] on a literal spine, so these ground facts
-- (proved by [simp]) discharge [empty]'s obligation.
@[grind] theorem twf_empty :
    twf (.TCons .BNil (.TCons .BNil (.TCons .BNil (.TCons .BNil
     (.TCons .BNil (.TCons .BNil (.TCons .BNil (.TCons .BNil .TNil)))))))) 0 := by
  simp [twf, bhome]

@[grind] theorem tlen_empty :
    tlen (.TCons .BNil (.TCons .BNil (.TCons .BNil (.TCons .BNil
     (.TCons .BNil (.TCons .BNil (.TCons .BNil (.TCons .BNil .TNil)))))))) = 8 := by
  simp [tlen]

-- ===== bucket lemmas =====

-- A key that hashes elsewhere is not in a bucket homed at [p].
theorem bfind_miss (k p : Int) (b : Vox_Htbl_bucket)
    (hb : bhome b p) (hne : index k ≠ p) : bfind k b = .Missing := by
  induction b <;> grind
grind_pattern bfind_miss => bfind k b, bhome b p

theorem bfind_badd_eq (k v : Int) (b : Vox_Htbl_bucket) :
    bfind k (badd k v b) = .Found v := by
  induction b <;> grind

theorem bfind_badd_ne (k k' v : Int) (b : Vox_Htbl_bucket) (hne : k' ≠ k) :
    bfind k' (badd k v b) = bfind k' b := by
  induction b <;> grind

theorem bhome_badd (k v p : Int) (b : Vox_Htbl_bucket)
    (hb : bhome b p) (hk : index k = p) : bhome (badd k v b) p := by
  induction b <;> grind

-- ===== spine lemmas =====

theorem tlen_tset (t : Vox_Htbl_table) (o : Int) (nb : Vox_Htbl_bucket) :
    tlen (tset t o nb) = tlen t := by
  induction t generalizing o <;> grind
grind_pattern tlen_tset => tlen (tset t o nb)

-- The bucket at offset [o] of a table well-formed from base [p] is
-- homed at position [p + o].
theorem twf_nth (t : Vox_Htbl_table) (p o : Int)
    (hwf : twf t p) (ho : 0 <= o) : bhome (tnth t o) (p + o) := by
  induction t generalizing p o with
  | TNil => grind
  | TCons b r ih =>
    by_cases h0 : o <= 0
    · grind
    · have := ih (p + 1) (o - 1) (by grind) (by grind)
      grind

-- Replacing offset [o] (in range) with a bucket homed at [p + o]
-- preserves well-formedness.
theorem twf_tset (t : Vox_Htbl_table) (p o : Int) (nb : Vox_Htbl_bucket)
    (hwf : twf t p) (ho : 0 <= o) (hnb : bhome nb (p + o)) : twf (tset t o nb) p := by
  induction t generalizing p o with
  | TNil => grind
  | TCons b r ih =>
    by_cases h0 : o <= 0
    · grind
    · have := ih (p + 1) (o - 1) (by grind) (by grind) (by grind)
      grind

-- Reading back a point update at an in-range offset: the changed
-- offset takes the new bucket, every other offset is unchanged.
theorem tnth_tset (t : Vox_Htbl_table) (o o' : Int) (nb : Vox_Htbl_bucket)
    (ho : 0 <= o) (ho' : 0 <= o') (hlt : o < tlen t) :
    tnth (tset t o nb) o' = (if o' = o then nb else tnth t o') := by
  induction t generalizing o o' with
  | TNil => grind
  | TCons b r ih =>
    rw [tset]
    by_cases h0 : o <= 0
    · simp only [h0, if_true]
      rw [tnth]
      by_cases h0' : o' <= 0 <;> grind
    · simp only [h0, if_false]
      rw [tnth]
      by_cases h0' : o' <= 0
      · grind
      · have := ih (o - 1) (o' - 1) (by grind) (by grind) (by grind)
        grind

-- ===== the hash-table theorem =====

-- A miss below the base: if [k] hashes below every position in [t],
-- the scan finds nothing.
theorem tfind_miss (k : Int) (t : Vox_Htbl_table) (p : Int)
    (hwf : twf t p) (hlt : index k < p) : tfind k t = .Missing := by
  induction t generalizing p with
  | TNil => grind
  | TCons b r ih =>
    have := bfind_miss k p b (by grind) (by grind)
    have := ih (p + 1) (by grind) (by grind)
    grind

-- THE POINT: scanning the whole table equals jumping to the one
-- bucket the key hashes to.  Proved by induction on the spine -- the
-- buckets before the home position miss (their keys hash lower), the
-- home bucket decides, and the remainder is irrelevant.
theorem tfind_eq_jump (k : Int) (t : Vox_Htbl_table) (p : Int)
    (hwf : twf t p) (hlo : p <= index k) :
    tfind k t = bfind k (tnth t (index k - p)) := by
  induction t generalizing p with
  | TNil => grind
  | TCons b r ih =>
    by_cases heq : index k = p
    · -- home bucket is the head; the tail hashes higher, so misses
      have := tfind_miss k r (p + 1) (by grind) (by grind)
      grind
    · -- head misses (homed at [p], key hashes higher); recurse
      have := bfind_miss k p b (by grind) (by grind)
      have := ih (p + 1) (by grind) (by grind)
      grind
grind_pattern tfind_eq_jump => tfind k t, twf t p

-- ===== add characterization (via the jump theorem) =====

theorem twf_madd (k v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hk : 0 <= k) : twf (madd k v t) 0 := by
  have hr := index_range k hk
  have hn := twf_nth t 0 (index k) hwf (by grind)
  have hb := bhome_badd k v (index k) (tnth t (index k)) (by grind) rfl
  have := twf_tset t 0 (index k) (badd k v (tnth t (index k))) hwf (by grind) (by grind)
  unfold madd
  grind
grind_pattern twf_madd => madd k v t, twf t 0

theorem tlen_madd (k v : Int) (t : Vox_Htbl_table) :
    tlen (madd k v t) = tlen t := by
  unfold madd
  grind [tlen_tset]
grind_pattern tlen_madd => tlen (madd k v t)

-- The added key maps to its value ...
theorem tfind_madd_eq (k v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hlen : tlen t = 8) (hk : 0 <= k) :
    tfind k (madd k v t) = .Found v := by
  have hr := index_range k hk
  have hwf' := twf_madd k v t hwf hk
  have hj := tfind_eq_jump k (madd k v t) 0 hwf' (by grind)
  have ht := tnth_tset t (index k) (index k) (badd k v (tnth t (index k)))
    (by grind) (by grind) (by grind)
  have := bfind_badd_eq k v (tnth t (index k))
  unfold madd at hj
  grind
grind_pattern tfind_madd_eq => tfind k (madd k v t)

-- ... and every other key is unchanged.
theorem tfind_madd_ne (k k' v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hlen : tlen t = 8) (hk : 0 <= k) (hk' : 0 <= k')
    (hne : k' ≠ k) :
    tfind k' (madd k v t) = tfind k' t := by
  have hr := index_range k hk
  have hr' := index_range k' hk'
  have hwf' := twf_madd k v t hwf hk
  have hj := tfind_eq_jump k' (madd k v t) 0 hwf' (by grind)
  have hj0 := tfind_eq_jump k' t 0 hwf (by grind)
  have ht := tnth_tset t (index k) (index k') (badd k v (tnth t (index k)))
    (by grind) (by grind) (by grind)
  have hbne := bfind_badd_ne k k' v (tnth t (index k)) hne
  unfold madd at hj
  grind
grind_pattern tfind_madd_ne => tfind k' (madd k v t)
|lean}]

(* The API type IS the refined table: a hash table is a spine of the
   fixed [width] whose every bucket satisfies the home invariant. *)
type t = table{ twf _ 0 && tlen _ = 8 }

(* The fixed empty table: eight empty buckets. *)
val empty
  : t{ _
       = TCons
           ( BNil,
             TCons
               ( BNil,
                 TCons
                   ( BNil,
                     TCons
                       ( BNil,
                         TCons
                           (BNil, TCons (BNil, TCons (BNil, TCons (BNil, TNil))))
                       ) ) ) ) }

(* The hash of a key (non-negative, so the bucket is in range). *)
val index : (k : int{ 0 <= _ }) -> int{ _ = index k }

(* Efficient one-bucket lookup, proved equal to the whole-table scan
   [tfind] (which quantifies EVERY bucket): [tfind_eq_jump] bridges
   the bucket visited to the buckets skipped. *)
val find : (k : int{ 0 <= _ }) -> (t : t) -> opt{ _ = tfind k t }

(* Insertion returns exactly the model's [madd]; the exported
   theorems [tfind_madd_eq] (the key now maps to [v]) and
   [tfind_madd_ne] (every other key unchanged) then characterize the
   result completely at every client. *)
val add : (k : int{ 0 <= _ }) -> (v : int) -> (t : t) -> t{ _ = madd k v t }
