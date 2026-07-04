(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Hash-table soundness probes, caught by the model from lib/htbl.mli
   (inlined here over this unit's own datatypes -- a mechanics expect
   test is self-contained).  Membership is spelled with [bool] rather
   than the library's [opt] to keep every rejection below at the Lean
   PROOF layer.  The broken variants are exactly the code a tired
   programmer writes. *)

type bucket =
  | BNil
  | BCons of int * int * bucket

type table =
  | TNil
  | TCons of bucket * table

[%%vox.lean {lean|
@[grind] def index (k : Int) : Int := Int.tmod k 8

@[grind] def bmem : Int -> Vox_bucket -> Bool
  | _, .BNil => false
  | k, .BCons k' _ r => if k = k' then true else bmem k r

@[grind] def bhome : Vox_bucket -> Int -> Prop
  | .BNil, _ => True
  | .BCons k _ r, p => index k = p ∧ bhome r p

@[grind] def tmem : Int -> Vox_table -> Bool
  | _, .TNil => false
  | k, .TCons b r => bmem k b || tmem k r

@[grind] def tnth : Vox_table -> Int -> Vox_bucket
  | .TNil, _ => .BNil
  | .TCons b r, o => if o <= 0 then b else tnth r (o - 1)

@[grind] def twf : Vox_table -> Int -> Prop
  | .TNil, _ => True
  | .TCons b r, p => bhome b p ∧ twf r (p + 1)
|lean}]
[%%expect{|
type bucket = BNil | BCons of int * int * bucket
type table = TNil | TCons of bucket * table
|}]

(* Honest helpers -- these DO verify, and are the pieces the broken
   lookup below misuses. *)
let rec nth_bucket : (t : table) -> (o : int) -> bucket{ _ = tnth t o } =
  fun t o ->
    match t with
    | TNil -> BNil
    | TCons (b, r) -> if o <= 0 then b else nth_bucket r (o - 1)

let rec bucket_mem : (k : int) -> (b : bucket) -> bool{ _ = bmem k b } =
  fun k b ->
    match b with
    | BNil -> false
    | BCons (k', _, r) -> if k = k' then true else bucket_mem k r
[%%expect{|
val nth_bucket : (t : table) -> (o : int) -> bucket{ _ = (tnth t o) } = <fun>
val bucket_mem : (k : int) -> (b : bucket) -> bool{ _ = (bmem k b) } = <fun>
|}]

(* PROBE (a): looking in the WRONG bucket.  A key hashes to [index k];
   inspecting bucket [index k + 1] cannot decide membership in the
   whole table, so the equation with the model [tmem] fails -- the
   counterexample is a key present in its own bucket but missed by the
   off-by-one probe. *)
let mem_wrong :
  (k : int{ 0 <= _ }) -> (t : table{ twf _ 0 }) -> bool{ _ = tmem k t } =
  fun k t ->
    let i = (k mod 8) + 1 in
    let b = nth_bucket t i in
    bucket_mem k b
[%%expect{|
Line 6, characters 4-18:
6 |     bucket_mem k b
        ^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: *unknown4* = (tmem k t)
Hypotheses:
  *unknown4* = (bmem k b)
  b = (tnth t i)
  i = ((k mod 8) + 1)
  twf t 0
  0 <= k
Possible counterexample:
  k = 0
  i = 1
(lean: error: `grind` failed)
|}]

(* PROBE (b): filing a pair under the WRONG index.  Key 3 hashes to
   bucket 3, but here it is placed in bucket 0; the table then
   violates the home invariant [twf]. *)
let forged : table{ twf _ 0 } =
  TCons (BCons (3, 7, BNil), TNil)
[%%expect{|
Line 2, characters 2-34:
2 |   TCons (BCons (3, 7, BNil), TNil)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: twf (TCons (BCons (3, 7, BNil), TNil)) 0
Hypotheses: <none>
Possible counterexample:
  index 3 = 3
(lean: error: `grind` failed)
|}]
