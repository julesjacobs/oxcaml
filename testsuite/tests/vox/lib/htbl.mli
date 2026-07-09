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
@[grind, expose] public def index (k : Int) : Int := Int.tmod k 8

public axiom index_range (k : Int) (h : 0 <= k) : 0 <= index k ∧ index k < 8
grind_pattern index_range => index k

-- Bucket = association list.  [bfind] returns the first value bound
-- to [k]; [badd] inserts or replaces.
@[grind, expose] public def bfind : Int -> Vox_Htbl_bucket -> Vox_Htbl_opt
  | _, .BNil => .Missing
  | k, .BCons k' v r => if k = k' then .Found v else bfind k r

@[grind, expose] public def badd : Int -> Int -> Vox_Htbl_bucket -> Vox_Htbl_bucket
  | k, v, .BNil => .BCons k v .BNil
  | k, v, .BCons k' v' r =>
      if k = k' then .BCons k v r else .BCons k' v' (badd k v r)

-- Bucket invariant: every key in the bucket hashes to [p].
@[grind, expose] public def bhome : Vox_Htbl_bucket -> Int -> Prop
  | .BNil, _ => True
  | .BCons k _ r, p => index k = p ∧ bhome r p

-- Whole-table scan: the first bucket (in spine order) that binds [k].
@[grind, expose] public def tfind : Int -> Vox_Htbl_table -> Vox_Htbl_opt
  | _, .TNil => .Missing
  | k, .TCons b r =>
      match bfind k b with
      | .Found v => .Found v
      | .Missing => tfind k r

-- Spine length.
@[grind, expose] public def tlen : Vox_Htbl_table -> Int
  | .TNil => 0
  | .TCons _ r => 1 + tlen r

-- Direct access to the bucket at spine offset [o] (junk .BNil off
-- the end, which reads as an empty bucket -- sound).
@[grind, expose] public def tnth : Vox_Htbl_table -> Int -> Vox_Htbl_bucket
  | .TNil, _ => .BNil
  | .TCons b r, o => if o <= 0 then b else tnth r (o - 1)

-- Point update of the spine at offset [o].
@[grind, expose] public def tset : Vox_Htbl_table -> Int -> Vox_Htbl_bucket -> Vox_Htbl_table
  | .TNil, _, _ => .TNil
  | .TCons b r, o, nb => if o <= 0 then .TCons nb r else .TCons b (tset r (o - 1) nb)

-- Well-formedness from base position [p]: bucket at [p] is [bhome p],
-- the next at [p+1], and so on.
@[grind, expose] public def twf : Vox_Htbl_table -> Int -> Prop
  | .TNil, _ => True
  | .TCons b r, p => bhome b p ∧ twf r (p + 1)

-- The model-level insert: replace the home bucket with [badd].
@[grind, expose] public def madd (k v : Int) (t : Vox_Htbl_table) : Vox_Htbl_table :=
  tset t (index k) (badd k v (tnth t (index k)))

-- THE POINT: scanning the whole table equals jumping to the one
-- bucket the key hashes to.  Proved by induction on the spine -- the
-- buckets before the home position miss (their keys hash lower), the
-- home bucket decides, and the remainder is irrelevant.
public axiom tfind_eq_jump (k : Int) (t : Vox_Htbl_table) (p : Int)
    (hwf : twf t p) (hlo : p <= index k) :
    tfind k t = bfind k (tnth t (index k - p))
grind_pattern tfind_eq_jump => tfind k t, twf t p

-- ===== add characterization (via the jump theorem) =====

public axiom twf_madd (k v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hk : 0 <= k) : twf (madd k v t) 0
grind_pattern twf_madd => madd k v t, twf t 0

public axiom tlen_madd (k v : Int) (t : Vox_Htbl_table) :
    tlen (madd k v t) = tlen t
grind_pattern tlen_madd => tlen (madd k v t)

-- The added key maps to its value ...
public axiom tfind_madd_eq (k v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hlen : tlen t = 8) (hk : 0 <= k) :
    tfind k (madd k v t) = .Found v
grind_pattern tfind_madd_eq => tfind k (madd k v t)

-- ... and every other key is unchanged.
public axiom tfind_madd_ne (k k' v : Int) (t : Vox_Htbl_table)
    (hwf : twf t 0) (hlen : tlen t = 8) (hk : 0 <= k) (hk' : 0 <= k')
    (hne : k' ≠ k) :
    tfind k' (madd k v t) = tfind k' t
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

(* The bucket-level operations, exported for layers that reach a
   bucket some other way (the mutable table reads one through a
   loan): assoc-list search and insert/replace, each equal to its
   model. *)
val bucket_find : (k : int) -> (b : bucket) -> opt{ _ = bfind k b }
val bucket_add : (k : int) -> (v : int) -> (b : bucket) -> bucket{ _ = badd k v b }

(* Efficient one-bucket lookup, proved equal to the whole-table scan
   [tfind] (which quantifies EVERY bucket): [tfind_eq_jump] bridges
   the bucket visited to the buckets skipped. *)
val find : (k : int{ 0 <= _ }) -> (t : t) -> opt{ _ = tfind k t }

(* Insertion returns exactly the model's [madd]; the exported
   theorems [tfind_madd_eq] (the key now maps to [v]) and
   [tfind_madd_ne] (every other key unchanged) then characterize the
   result completely at every client. *)
val add : (k : int{ 0 <= _ }) -> (v : int) -> (t : t) -> t{ _ = madd k v t }
