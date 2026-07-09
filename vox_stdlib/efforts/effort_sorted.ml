(* WP-6 Effort B -- sorted insertion + list aggregation (Vlist + Vint).

   PREVIOUSLY BLOCKED, NOW UNBLOCKED (2026-07-08 transparency flip): the
   originally-requested shape -- a hand-rolled sorted insert with a
   sortedness invariant + length preservation -- verifies as an honest
   CLIENT now that Vlist exposes its repr.  `insert` STRUCTURALLY RECURSES
   over the exported `Nil | Cons` type: patterns mint facts (l = .Cons y r),
   the client's own exposed model defs (cl_lb / cl_sorted) reduce on the
   native constructors the client builds, and the recursion's postcondition
   threads the sortedness invariant.  This was impossible under the old
   via-abstract face (F-B1: head/tail eliminator insufficient for
   consumer-side recursion).  The length/sum aggregation via the exact-output
   fold laws (which was the fallback last round) is retained below. *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vlist

[%%vox.lean {lean|
-- cl_lb b l : b is a lower bound of every element of l (structural, no ∀).
@[grind, expose] def cl_lb (b : Int) : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons y t => b ≤ y ∧ cl_lb b t
-- cl_sorted : each head is a lower bound of its tail, recursively.
@[grind, expose] def cl_sorted : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons x t => cl_lb x t ∧ cl_sorted t
-- cl_ins_lb : insert's self-supporting lower-bound postcondition -- any bound
-- b that bounds the input AND is ≤ the inserted x still bounds the output.
-- This is what lets the recursive case re-establish the head's lower bound.
@[grind, expose] def cl_ins_lb (l : Vox_Vlist_t) (x : Int) (res : Vox_Vlist_t) : Prop :=
  ∀ b, cl_lb b l -> b ≤ x -> cl_lb b res

-- monotonicity of the lower bound: a weaker bound still bounds (then-branch).
theorem cl_lb_mono (a b : Int) (t : Vox_Vlist_t) (h : a ≤ b) (hb : cl_lb b t) :
    cl_lb a t := by
  induction t <;> grind
grind_pattern cl_lb_mono => cl_lb a t, cl_lb b t
|lean}]

(* SORTED INSERT (the originally-blocked shape, now a working client).
   Precondition: l is sorted.  Postcondition: the result is sorted, its
   length is one more, and it carries the self-supporting lower-bound fact. *)
let rec insert : (x : int) -> (l : Vlist.t{ cl_sorted _ }) ->
    Vlist.t{ cl_sorted _ && cl_ins_lb l x _ && ll_len _ = 1 + ll_len l } =
  fun x l ->
    match l with
    | Nil -> Cons (x, Nil)
    | Cons (y, r) ->
        if x <= y then Cons (x, l)
        else let r' = insert x r in Cons (y, r')

(* SUM preservation, exact: folding (+) over l is exactly ll_sum l. *)
let total (l : Vlist.t) : int{ _ = ll_sum l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) 0 l

(* COUNT preservation, exact: folding (+1) is exactly the length. *)
let count (l : Vlist.t) : int{ _ = ll_len l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + 1) (fun acc x -> acc + 1) 0 l

(* PUSH: consing one element raises the length by exactly one (ll_len_cons). *)
let push_len (x : int) (l : Vlist.t) : int{ _ = 1 + ll_len l } =
  Vlist.length (Vlist.cons x l)

(* CONCAT: appending two lists adds their lengths (ll_len_app). *)
let concat_len (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_len a + ll_len b } =
  Vlist.length (Vlist.append a b)

(* SUM over a nonzero start, clamped -- a bounded aggregate (Vint at play). *)
let bounded_total (l : Vlist.t) : int =
  let t = total l in
  Vint.iclamp 0 1000 t
