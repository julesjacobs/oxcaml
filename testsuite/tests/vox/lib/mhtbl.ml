(* Implementation of mhtbl.mli: the imperative table over Bslice's
   bucket-array borrows, verified against Htbl's model.  Each
   operation opens a borrow bracket, touches the ONE home bucket, and
   resolves; the bracket's unit (or result) refinement carries the
   prophecy's value out, and Htbl's exported lemmas ([twf_madd],
   [tlen_madd], [tfind_eq_jump], [index_range]) re-establish the
   table invariant.  Nothing here is assumed. *)

open Htbl
open Bslice

type t = varr{ twf (bcts _) 0 && tlen (bcts _) = 8 }

let create : unit -> t{ bcts _ = empty } @ unique = fun () -> of_model empty

(* [Htbl.bucket_find]'s result, rebuilt so it is UNIQUE (a borrow
   bracket must return one): the constant [Missing] mode-crosses, and
   [Found] is reconstructed fresh. *)
let ufind : (k : int) -> (b : bucket) -> opt{ _ = bfind k b } @ unique =
  fun k b ->
    let r = bucket_find k b in
    match r with
    | Missing -> Missing
    | Found v -> Found v

let add :
  (k : int{ 0 <= _ }) -> (v : int) -> (h : t) @ unique ->
  t{ bcts _ = madd k v (bcts h) } @ unique =
  fun k v h ->
    let p = new_proph () in
    let i = index k in
    let (h', u) =
      borrow p h (fun m ->
        let (b0, m1) = sget m i in
        let b = gbl b0 in
        let b' = bucket_add k v b in
        let m2 = sset m1 i b' in
        let _u = sdrop m2 in
        (() : unit{ bpv p = madd k v (bcts h) }))
    in
    ignore u;
    (h' : t{ bcts _ = madd k v (bcts h) })

let find :
  (k : int{ 0 <= _ }) -> (h : t) @ unique ->
  (opt{ _ = tfind k (bcts h) } * t{ bcts _ = bcts h }) @ unique =
  fun k h ->
    let p = new_proph () in
    let i = index k in
    let (h', r) =
      borrow p h (fun m ->
        let (b0, m1) = sget m i in
        let b = gbl b0 in
        let _u = sdrop m1 in
        (ufind k b : opt{ _ = tfind k (bcts h) && bpv p = bcts h }))
    in
    ((r : opt{ _ = tfind k (bcts h) }), (h' : t{ bcts _ = bcts h }))
