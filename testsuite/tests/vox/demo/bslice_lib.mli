(* Mutable BUCKET-ARRAY slices as RUSTHORN-STYLE BORROWS: the
   bucket-typed twin of demo/slice_lib, with the SAME prophecy
   discipline (now/fin, [pv], [@ local unique] loans) but element type
   [Htbl.bucket] rather than [int].  Because a bucket is an IMMUTABLE
   boxed value, a read ([sget]) takes the loan by SHARED borrow
   ([@ local]) and hands back the bucket GLOBALLY -- overwriting the
   cell later cannot mutate the value already read.  Only the ops an
   in-place [add] needs are provided: no [split] (no reborrow), no
   parallelism.

   TRUSTED: [varr]/[proph]/[slice] are abstract and boxed; every
   [assume_unchecked_] below asserts its signature's ghost facts hold
   of the real bucket array.  This is the mutable phase's whole trust
   boundary (six functions); every client fact is PROVED from these
   signatures and the model reused from Htbl. *)

open Htbl

type varr
type proph
type slice

[%%vox.lean {lean|
-- ghosts: an owned bucket-array denotes its contents [bcts]; a live
-- loan denotes current/prophesied-final contents [bnow]/[bfin]; a
-- prophecy denotes the sequence it resolves to [bpv].  All are
-- sequences of BUCKETS.
opaque bcts : VoxU -> List Vox_Htbl_bucket
opaque bnow : VoxU -> List Vox_Htbl_bucket
opaque bfin : VoxU -> List Vox_Htbl_bucket
opaque bpv : VoxU -> List Vox_Htbl_bucket

-- Bucket-indexed list operations (twins of slice_lib's len/elem/upd).
@[grind] def blen : List Vox_Htbl_bucket -> Int
  | [] => 0
  | _ :: t => 1 + blen t

@[grind] def belem : List Vox_Htbl_bucket -> Int -> Vox_Htbl_bucket
  | [], _ => .BNil
  | x :: t, i => if i = 0 then x else belem t (i - 1)

@[grind] def bupd : List Vox_Htbl_bucket -> Int -> Vox_Htbl_bucket -> List Vox_Htbl_bucket
  | [], _, _ => []
  | x :: t, i, v => if i = 0 then v :: t else x :: bupd t (i - 1) v

theorem blen_nonneg (l : List Vox_Htbl_bucket) : 0 <= blen l := by
  induction l <;> grind
grind_pattern blen_nonneg => blen l

theorem blen_bupd (l : List Vox_Htbl_bucket) (i : Int) (v : Vox_Htbl_bucket) :
    blen (bupd l i v) = blen l := by
  induction l generalizing i <;> grind
grind_pattern blen_bupd => blen (bupd l i v)

theorem belem_bupd (l : List Vox_Htbl_bucket) (i j : Int) (v : Vox_Htbl_bucket)
    (h1 : 0 <= i) (h2 : i < blen l) :
    belem (bupd l i v) j = if j = i then v else belem l j := by
  induction l generalizing i j <;> grind
grind_pattern belem_bupd => belem (bupd l i v) j
|lean}]

(* Allocate [n] copies of a bucket. *)
val bnew : (n : int{ 0 <= _ }) -> (b : bucket) -> varr{ blen (bcts _) = n } @ unique

val new_proph : unit -> proph @ unique

(* Open a borrow bracket: the continuation gets the root loan over the
   whole array; the residual comes back at the prophesied contents. *)
val borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ bnow _ = bcts x && bfin _ = bpv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ bcts _ = bpv p } * 'b) @ unique

(* Read a bucket cell (the loan is threaded, so the read bucket comes
   back [local] to the bracket). *)
val sget :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < blen (bnow m) }) ->
  (bucket{ _ = belem (bnow m) i } * slice{ bnow _ = bnow m && bfin _ = bfin m })
    @ local unique

(* Globalize an immutable bucket read from a loan: sound because the
   value lives on the heap and is never mutated in place.  TRUSTED. *)
val gbl : (b : bucket) @ local -> bucket{ _ = b }

(* Write a bucket cell: consumes the loan, re-issues it advanced. *)
val sset :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < blen (bnow m) }) ->
  (b : bucket) ->
  slice{ bnow _ = bupd (bnow m) i b && bfin _ = bfin m } @ local unique

val sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m }
