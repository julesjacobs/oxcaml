(* A mutable BUCKET ARRAY as a RUSTHORN-STYLE BORROW, whose ghost is
   the immutable table model: an owned array denotes a value of
   Htbl's SPINE datatype [table], so every fact Htbl proves about the
   model ([twf]/[tlen]/[tnth]/[tset]/[madd]/[tfind] and their
   theorems) applies to the mutable table verbatim -- the mutable
   layer re-derives NOTHING.  (Contrast lib/pslice, the GENERIC
   borrow library over [@@vox.poly] sorts: its list ghosts serve
   every element type, at the cost of bridging to a client's own
   model; the SPECIALIZED ghost sort here erases that bridge for the
   one client whose model is a reflected datatype.)  The prophecy discipline is
   demo/slice_lib's ([@ local unique] loans, now/fin, resolution by
   [sdrop]); the element type is [Htbl.bucket] rather than [int].
   Because a bucket is an IMMUTABLE boxed value, a read ([sget])
   hands the bucket back GLOBALLY ([gbl]) -- overwriting the cell
   later cannot mutate a value already read.  Only the ops an
   in-place hash table needs are provided: no [split] (no reborrow),
   no parallelism.

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
-- ghosts: an owned bucket-array denotes its contents as a model
-- SPINE [bcts]; a live loan denotes current/prophesied-final
-- contents [bnow]/[bfin]; a prophecy denotes the spine it resolves
-- to [bpv].  All at Htbl's table datatype -- no parallel list
-- theory, no conversion.  ([opaque] wants inhabitation, which an
-- inductive does not carry automatically.)
instance : Inhabited Vox_Htbl_table := ⟨.TNil⟩
opaque bcts : VoxU -> Vox_Htbl_table
opaque bnow : VoxU -> Vox_Htbl_table
opaque bfin : VoxU -> Vox_Htbl_table
opaque bpv : VoxU -> Vox_Htbl_table
|lean}]

(* Allocate a mutable image of a model table: the ghost of the fresh
   array IS the argument.  The caller's facts about [m] (a spine
   literal, an invariant riding [Htbl.t]) transfer to the array. *)
val of_model : (m : table) -> varr{ bcts _ = m } @ unique

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
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
  (bucket{ _ = tnth (bnow m) i } * slice{ bnow _ = bnow m && bfin _ = bfin m })
    @ local unique

(* Globalize an immutable bucket read from a loan: sound because the
   value lives on the heap and is never mutated in place.  TRUSTED. *)
val gbl : (b : bucket) @ local -> bucket{ _ = b }

(* Write a bucket cell: consumes the loan, re-issues it advanced. *)
val sset :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
  (b : bucket) ->
  slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m } @ local unique

val sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m }
