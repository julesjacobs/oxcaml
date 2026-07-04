(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/htbl.mli ../lib/htbl.ml bslice_lib.mli bslice_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IN-PLACE hash-table insert over RustHorn-style BUCKET-array borrows
   (bslice_lib), verified against the SAME model as the immutable
   table.  A mutable table is a [varr] of buckets; [madd_mut] borrows
   it, reads the home bucket, writes back [badd k v] of it through the
   loan, and resolves -- the residual's ghost contents are EXACTLY the
   list-level model insert [lmadd], mirroring lean_flip's
   [model _ = f (model h)] and lean_qsort's borrow/probe/resolve
   shape.  Everything reuses Htbl's bucket-level model (index, badd,
   bhome, and their lemmas travel through Htbl's .cmi); only the
   list-level table lemmas are re-derived here.  The single trust
   boundary is bslice_lib; nothing here is assumed. *)

open Htbl
open Bslice_lib

[%%vox.lean {lean|
-- Home invariant over a bucket LIST from base [p] (the list-level
-- mirror of Htbl.twf): every key in the bucket at position p hashes
-- to p.  [bhome] is Htbl's, reused.
@[grind] def lwf : List Vox_Htbl_bucket -> Int -> Prop
  | [], _ => True
  | b :: r, p => bhome b p ∧ lwf r (p + 1)

-- The list-level model insert: replace the home bucket (index k) with
-- Htbl.badd of it.  This is the ghost the in-place write produces.
@[grind] def lmadd (k v : Int) (l : List Vox_Htbl_bucket) : List Vox_Htbl_bucket :=
  bupd l (index k) (badd k v (belem l (index k)))

-- The bucket at offset [o] of a list well-formed from [p] is homed at
-- [p + o] (mirror of Htbl.twf_nth).
theorem lwf_belem (l : List Vox_Htbl_bucket) (p o : Int)
    (hwf : lwf l p) (ho : 0 <= o) : bhome (belem l o) (p + o) := by
  induction l generalizing p o with
  | nil => grind
  | cons b r ih =>
    by_cases h0 : o = 0
    · grind
    · have := ih (p + 1) (o - 1) (by grind) (by grind)
      grind

-- Replacing offset [o] with a bucket homed at [p + o] preserves the
-- invariant (mirror of Htbl.twf_tset).
theorem lwf_bupd (l : List Vox_Htbl_bucket) (p o : Int) (nb : Vox_Htbl_bucket)
    (hwf : lwf l p) (ho : 0 <= o) (hnb : bhome nb (p + o)) : lwf (bupd l o nb) p := by
  induction l generalizing p o with
  | nil => grind
  | cons b r ih =>
    by_cases h0 : o = 0
    · grind
    · have := ih (p + 1) (o - 1) (by grind) (by grind)
      grind

-- The model insert preserves both width and the home invariant --
-- so a valid table stays valid (reuses Htbl.bhome_badd/index_range).
theorem blen_lmadd (k v : Int) (l : List Vox_Htbl_bucket) :
    blen (lmadd k v l) = blen l := by
  unfold lmadd
  grind [blen_bupd]
grind_pattern blen_lmadd => blen (lmadd k v l)

theorem lwf_lmadd (k v : Int) (l : List Vox_Htbl_bucket)
    (hwf : lwf l 0) (hk : 0 <= k) (hlt : index k < blen l) :
    lwf (lmadd k v l) 0 := by
  have hr := index_range k hk
  have hb := lwf_belem l 0 (index k) hwf (by grind)
  have := bhome_badd k v (index k) (belem l (index k)) (by grind) rfl
  have := lwf_bupd l 0 (index k) (badd k v (belem l (index k))) hwf (by grind) (by grind)
  unfold lmadd
  grind
grind_pattern lwf_lmadd => lwf (lmadd k v l) 0
|lean}]

(* Association-list insert, proved equal to Htbl's model [badd]
   (which travels through Htbl's .cmi). *)
let rec bucket_add : (k : int) -> (v : int) -> (b : bucket) -> bucket{ _ = badd k v b } =
  fun k v b ->
    match b with
    | BNil -> BCons (k, v, BNil)
    | BCons (k', v', r) ->
      if k = k' then BCons (k, v, r)
      else begin
        let r' = bucket_add k v r in
        BCons (k', v', r')
      end

(* The in-place insert: borrow the table, read the home bucket, write
   [badd k v] of it back through the loan, resolve.  The residual's
   ghost is exactly [lmadd k v (bcts x)] -- the model insert. *)
let madd_mut :
  (k : int{ 0 <= _ }) -> (v : int) -> (x : varr{ blen (bcts _) = 8 }) @ unique ->
  varr{ bcts _ = lmadd k v (bcts x) && blen (bcts _) = 8 } @ unique =
  fun k v x ->
    let p = new_proph () in
    let i = k mod 8 in
    let (x', u) =
      borrow p x (fun m ->
        let (b0, m1) = sget m i in
        let b = gbl b0 in
        let b' = bucket_add k v b in
        let m2 = sset m1 i b' in
        let _u = sdrop m2 in
        (() : unit{ bpv p = lmadd k v (bcts x) }))
    in
    ignore u;
    (x' : varr{ bcts _ = lmadd k v (bcts x) && blen (bcts _) = 8 })

(* End to end: a fresh eight-bucket table, two in-place inserts (keys
   3 and 11 collide in bucket 3).  Each residual provably carries the
   model insert of the one before -- the caller is left with a table
   whose ghost is [lmadd 11 5 (lmadd 3 7 ...)]. *)
let run : unit -> varr @ unique =
  fun () ->
    let x0 = bnew 8 BNil in
    let x1 = madd_mut 3 7 x0 in
    let x2 = madd_mut 11 5 x1 in
    x2
