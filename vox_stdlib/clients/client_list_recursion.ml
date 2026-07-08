(* F-B1 INVERTED (2026-07-08 transparency flip).  With Vlist's repr now
   EXPOSED (`type t = Nil | Cons of int * t`), a client can HAND-ROLL
   structural recursion over the list and prove it against the module's
   shipped model (ll_len / ll_sum / ll_app), matching and building the
   native Nil/Cons.  Patterns mint facts (l = .Cons x r), the exposed
   recursive defs reduce on those constructors, and the recursion closes --
   exactly what was BLOCKED when Vlist was via-abstract (F-B1: the
   head/tail/is_empty eliminator surface could not support consumer-side
   recursive proofs).  Also forces the F-B2 sum laws (ll_sum_cons /
   ll_sum_app) via the opaque Vlist.cons/append face.
   Verifies against Vlist.cmi + VoxSig_Vlist.olean only (no repr .ml). *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vlist

(* hand-rolled length, proved EQUAL to the module's ll_len -- reduction on the
   native .Cons/.Nil a client itself builds and matches. *)
let rec my_length : (l : Vlist.t) -> int{ _ = ll_len l } =
  fun l -> match l with
    | Nil -> 0
    | Cons (_, r) -> let n = my_length r in 1 + n

(* hand-rolled sum, proved equal to ll_sum (exercises the exposed ll_sum). *)
let rec my_sum : (l : Vlist.t) -> int{ _ = ll_sum l } =
  fun l -> match l with
    | Nil -> 0
    | Cons (x, r) -> let s = my_sum r in x + s

(* hand-rolled append, proved equal to ll_app -- builds native Cons. *)
let rec my_append : (a : Vlist.t) -> (b : Vlist.t) -> Vlist.t{ _ = ll_app a b } =
  fun a b -> match a with
    | Nil -> b
    | Cons (x, r) -> let rest = my_append r b in Cons (x, rest)

(* hand-rolled membership, proved equal to ll_mem. *)
let rec my_mem : (x : int) -> (l : Vlist.t) -> bool{ _ = ll_mem x l } =
  fun x l -> match l with
    | Nil -> false
    | Cons (y, r) -> if x = y then true else my_mem x r

(* F-B2 liveness: sum through the OPAQUE Vlist.cons face needs ll_sum_cons. *)
let sum_cons (x : int) (l : Vlist.t) : int{ _ = x + ll_sum l } =
  let c = Vlist.cons x l in my_sum c

(* F-B2 liveness: sum through the OPAQUE Vlist.append face needs ll_sum_app. *)
let sum_app (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_sum a + ll_sum b } =
  let ab = Vlist.append a b in my_sum ab
