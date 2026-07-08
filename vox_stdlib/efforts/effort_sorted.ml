(* WP-6 Effort B -- list aggregation with EXACT-law stress (Vlist + Vint).

   ORIGINAL GOAL: a sorted insert with a sortedness invariant + length/sum
   preservation. That hand-rolled shape is BLOCKED at the consumer level (see the
   FINDINGS block at the bottom): a client cannot structurally recurse over a
   via-abstracted Vlist, because the bridge laws it would need
   (is_empty -> length 0; the cons-of-head-tail reconstruction inside a recursion)
   are not shipped / do not fire. So this effort verifies the length/sum
   preservation story through the surface that IS consumable -- the exact-output
   fold laws -- plus Vint for a bounded aggregate. This is the honest reroute a
   real user would take, and the blocked shape is recorded as the primary finding. *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vlist

(* SUM preservation, exact: folding (+) over l is exactly ll_sum l
   (ll_relFold_sum_exact). The reviewer's "the total is the sum of the parts". *)
let total (l : Vlist.t) : int{ _ = ll_sum l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) 0 l

(* COUNT preservation, exact: folding (+1) is exactly the length. *)
let count (l : Vlist.t) : int{ _ = ll_len l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + 1) (fun acc x -> acc + 1) 0 l

(* PUSH: consing one element raises the length by exactly one (ll_len_cons) --
   the per-step invariant an insert would have to preserve. *)
let push_len (x : int) (l : Vlist.t) : int{ _ = 1 + ll_len l } =
  Vlist.length (Vlist.cons x l)

(* CONCAT: appending two lists adds their lengths (ll_len_app) -- the
   divide-and-conquer length law. *)
let concat_len (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_len a + ll_len b } =
  Vlist.length (Vlist.append a b)

(* SUM over a nonzero start: total from an initial accumulator (Vint at play:
   the running total is clamped into [0, 1000] with Vint.iclamp -- a bounded
   aggregate a real report would compute). *)
let bounded_total (l : Vlist.t) : int =
  let t = total l in
  Vint.iclamp 0 1000 t

(* ============================ FINDINGS (Effort B) ============================
   The originally-requested shape — a hand-rolled sorted insert with a sortedness
   invariant + length/sum preservation — is BLOCKED at the consumer level. The
   length/sum preservation story is instead verified through the exact-output fold
   laws, which ARE consumable. Findings, in severity order:

   F-B1 (MAJOR): a client cannot STRUCTURALLY RECURSE over a via-abstracted Vlist.
   The natural insert (`if is_empty l then .. else let h = head l .. insert x
   (tail l)`) fails already at the base case: from [ll_isnil l] grind cannot
   derive [ll_len l = 0] — Vlist ships NO is_empty<->length bridge
   ([ll_isnil l <-> ll_len l = 0] / [ll_isnil l -> ll_len l = 0]). And in the
   recursive case the cons-of-head-tail reconstruction ([ll_cons_head_tail]) does
   not fire unless the client materialises [cons (head l) (tail l)] explicitly.
   Net: the head/tail/is_empty "eliminator surface" is insufficient for
   client-side recursive verification. Route around: use the shipped HOFs
   (fold_left/map/filter), which are proven INSIDE the module over the repr.
   Suggest: ship the missing bridge laws (is_empty<->len, a firing reconstruction
   trigger), OR ship insert/sort as Vlist OPS (module-internal, see the repr).

   F-B2 (MEDIUM): [ll_sum] has no cons law. [ll_sum (cons x l) = x + ll_sum l] is
   NOT client-provable — [ll_cons] is opaque (dead-law house rule) and [ll_sum]
   cannot reduce on it, exactly as [ll_len] needs [ll_len_cons]. But no
   [ll_sum_cons] is shipped, so sum-through-cons is unavailable; sum is only
   reachable via [fold_left]'s exact law. Suggest: ship [ll_sum_cons] (+ maybe
   [ll_sum_app]) alongside the length laws.

   F-B3 (MEDIUM): a sortedness INVARIANT is not client-expressible on
   concretely-built lists: a client [ll_sorted] predicate cannot reduce on a list
   built from the opaque [ll_cons], so even stating "this built list is sorted"
   does not evaluate. Sorted containers need module-internal support (an insert op
   with the invariant proven over the repr, per the invariant-liveness pattern).

   F-B4 (NONE, positive): the exact-output fold laws (ll_relFold_sum/count_exact)
   are the consumable workhorse — sum and count preservation verify directly over
   an abstract list, and append/cons length laws (ll_len_app/ll_len_cons) compose.
   Vint.iclamp drops in for a bounded aggregate. *)
