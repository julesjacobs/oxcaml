(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Mutable hash-table (bucket-borrow) soundness probes, each pinned to
   its rejection layer.  The bucket-slice API is ascribed inline (the
   same shape lib/bslice gives, which lib/mhtbl builds the imperative
   table on): array ghosts at the SPINE datatype, exactly as bslice
   models them at Htbl's table.  Lean rejects a stale-contents claim
   after a write; the mode checker rejects reusing a loan already
   consumed by a strong update, and reusing a prophecy already
   consumed by a borrow. *)

type bucket =
  | BNil
  | BCons of int * int * bucket

type table =
  | TNil
  | TCons of bucket * table

[%%vox.lean {lean|
instance : Inhabited Vox_table := ⟨.TNil⟩
opaque bcts : VoxU -> Vox_table
opaque bnow : VoxU -> Vox_table
opaque bfin : VoxU -> Vox_table
opaque bpv : VoxU -> Vox_table

@[grind] def tlen : Vox_table -> Int
  | .TNil => 0
  | .TCons _ r => 1 + tlen r

@[grind] def tset : Vox_table -> Int -> Vox_bucket -> Vox_table
  | .TNil, _, _ => .TNil
  | .TCons b r, o, nb => if o <= 0 then .TCons nb r else .TCons b (tset r (o - 1) nb)
|lean}]
[%%expect{|
type bucket = BNil | BCons of int * int * bucket
type table = TNil | TCons of bucket * table
|}]

module S : sig
  type varr
  type proph
  type slice

  val of_model : (m : table) -> varr{ bcts _ = m } @ unique

  val new_proph : unit -> proph @ unique

  val borrow :
    (p : proph) @ unique -> (x : varr) @ unique ->
    ((m : slice{ bnow _ = bcts x && bfin _ = bpv p }) @ local unique -> 'b @ unique)
      @ once local ->
    (varr{ bcts _ = bpv p } * 'b) @ unique

  val sset :
    (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
    (b : bucket) ->
    slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m } @ local unique

  val sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m }
end = struct
  type varr = A of { base : bucket array }
  type proph = P of { u : unit }
  type slice = L of { base : bucket array; off_ : int; len_ : int }

  let rec model_len (t : table) =
    match t with
    | TNil -> 0
    | TCons (_, r) -> 1 + model_len r

  let of_model : (m : table) -> varr{ bcts _ = m } @ unique =
    fun m ->
      let base = Array.make (model_len m) BNil in
      let rec fill (t : table) (i : int) =
        match t with
        | TNil -> ()
        | TCons (b, r) ->
          base.(i) <- b;
          fill r (i + 1)
      in
      fill m 0;
      assume_unchecked_ (Obj.magic_unique (A { base }))

  let new_proph : unit -> proph @ unique = fun () -> Obj.magic_unique (P { u = () })

  let borrow :
    (p : proph) @ unique -> (x : varr) @ unique ->
    ((m : slice{ bnow _ = bcts x && bfin _ = bpv p }) @ local unique -> 'b @ unique)
      @ once local ->
    (varr{ bcts _ = bpv p } * 'b) @ unique =
    fun p x k ->
      let (P _) = p in
      let (A { base }) = x in
      let m0 =
        (assume_unchecked_
           (Obj.magic_unique (L { base; off_ = 0; len_ = Array.length base }))
          : slice{ bnow _ = bcts x && bfin _ = bpv p })
      in
      let b = k m0 in
      Obj.magic_unique ((assume_unchecked_ (A { base }) : varr{ bcts _ = bpv p }), b)

  let sset :
    (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
    (b : bucket) ->
    slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m } @ local unique =
    fun m i b ->
      let (L { base; off_; len_ }) = m in
      base.(off_ + i) <- b;
      exclave_
        (Obj.magic_unique
           (assume_unchecked_ (L { base; off_; len_ })
             : slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m }))

  let sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m } =
    fun m ->
      let (L _) = m in
      assume_unchecked_ ()
end
[%%expect{|
module S :
  sig
    type varr
    type proph
    type slice
    val of_model : (m : table) -> varr{ bcts _ = m } @ unique
    val new_proph : unit -> proph @ unique
    val borrow :
      (p : proph) @ unique ->
      (x : varr) @ unique ->
      (slice{ bnow _ = bcts x && bfin _ = bpv p } @ local unique ->
       'b @ unique) @ local
      once -> varr{ bcts _ = bpv p } * 'b @ unique
    val sset :
      (m : slice) @ local unique ->
      (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
      (b : bucket) ->
      slice{ bnow _ = tset (bnow m) i b && bfin _ = bfin m } @ local unique
    val sdrop : (m : slice) @ local unique -> unit{ bfin m = bnow m }
  end
|}]

open S

(* PROBE (a), LEAN LAYER: a stale-contents claim.  After writing bucket
   [i], the loan's contents are [tset (bnow m) i b], NOT the old
   [bnow m]; claiming they are unchanged fails with a counterexample. *)
let stale :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < tlen (bnow m) }) ->
  (b : bucket) -> slice{ bnow _ = bnow m } @ local unique =
  fun m i b -> exclave_ (sset m i b)
[%%expect{|
Line 9, characters 24-36:
9 |   fun m i b -> exclave_ (sset m i b)
                            ^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: bnow *unknown8* = bnow m
Hypotheses:
  bnow *unknown8* = tset (bnow m) i b && bfin *unknown8* = bfin m
  0 <= i && i < tlen (bnow m)
Possible counterexample:
  i = 0
  tlen (bnow m) = 1
(lean: error: `grind` failed)
|}]

(* PROBE (b), MODE LAYER: a strong update consumes the loan, so writing
   twice through the same loan name is a stale view -- rejected. *)
let reuse : (m : slice{ 2 <= tlen (bnow _) }) @ local unique -> (b : bucket) -> unit =
  fun m b ->
    let m1 = sset m 0 b in
    let m2 = sset m 1 b in
    let _u1 = sdrop m1 in
    let _u2 = sdrop m2 in
    ()
[%%expect{|
Line 4, characters 18-19:
4 |     let m2 = sset m 1 b in
                      ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 18-19:
3 |     let m1 = sset m 0 b in
                      ^

|}]

(* PROBE (c), MODE LAYER: a prophecy is consumed by its borrow; reusing
   it for a second borrow would let two resolutions prove False. *)
let preuse : (x : varr) @ unique -> (y : varr) @ unique -> unit =
  fun x y ->
    let p = new_proph () in
    let (x', _u1) = borrow p x (fun m -> let _ = m in (() : unit)) in
    let (y', _u2) = borrow p y (fun m -> let _ = m in (() : unit)) in
    ignore x'; ignore y'
[%%expect{|
Line 5, characters 27-28:
5 |     let (y', _u2) = borrow p y (fun m -> let _ = m in (() : unit)) in
                               ^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 27-28:
4 |     let (x', _u1) = borrow p x (fun m -> let _ = m in (() : unit)) in
                               ^

|}]
