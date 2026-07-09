(* An IMPERATIVE hash table, UNIQUELY OWNED: the runtime value is a
   mutable bucket array (Bslice.varr) mutated in place, and its ghost
   [bcts] is the immutable model table -- so the contracts below are
   the SAME functions Htbl's interface proves theorems about.  A
   client reasons about a mutable table with [tfind_madd_eq] /
   [tfind_madd_ne] / [tfind_eq_jump] exactly as it would about the
   immutable one; nothing is re-proved and nothing new is assumed
   (the trust boundary is Bslice's, unchanged).

   Ownership is threaded: every operation consumes the table
   [@ unique] and returns it (alone, or beside its result), so writes
   are unobservable -- there is no alias to see the old state.  [add]
   is a borrow bracket around ONE bucket read and ONE bucket write;
   its residual's ghost is exactly the model insert [madd].  [find]
   mutates nothing and hands the table back with its ghost intact. *)

open Htbl
open Bslice

(* The table type IS the refined array: a spine of the fixed width
   whose every bucket satisfies the home invariant -- the mutable
   twin of Htbl.t. *)
type t = varr{ twf (bcts _) 0 && tlen (bcts _) = 8 }

(* A fresh table's ghost is the model's empty (eight empty buckets):
   [Htbl.empty]'s exported value fact pins the spine literal, so a
   client's find-after-create discharges by unfolding. *)
val create : unit -> t{ bcts _ = empty } @ unique

(* IN-PLACE insert: hash the key, rewrite the one home bucket through
   a loan.  The residual is the same table, its ghost advanced to
   exactly the model insert -- [tfind_madd_eq]/[tfind_madd_ne] then
   characterize every subsequent lookup. *)
val add :
  (k : int{ 0 <= _ }) -> (v : int) -> (h : t) @ unique ->
  t{ bcts _ = madd k v (bcts h) } @ unique

(* One-bucket lookup, equal to the whole-table scan [tfind] by
   Htbl's [tfind_eq_jump]; the table comes back untouched. *)
val find :
  (k : int{ 0 <= _ }) -> (h : t) @ unique ->
  (opt{ _ = tfind k (bcts h) } * t{ bcts _ = bcts h }) @ unique
