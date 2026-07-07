(* Smoke client for Vlist: one goal per shipped law, each written so the
   solver MUST apply that law's grind_pattern to discharge it (blueprint
   6.7 dead-law check).  Verifies against Vlist.cmi + VoxSig_Vlist.olean
   only -- no view of the tree repr or ll_repr.  Nested op-call results
   are let-bound: a call result cannot be passed straight into a
   dependent parameter (C1 friction; see notes/vlist.md). *)

(* ll_len_nonneg: the length of any list is non-negative. *)
let nonneg (l : Vlist.t) : int{ _ >= 0 } = Vlist.length l

(* ll_len_cons: length after a cons is one more. *)
let len_cons (x : int) (l : Vlist.t) : int{ _ = 1 + ll_len l } =
  let l' = Vlist.cons x l in
  Vlist.length l'

(* ll_len_app: length of append is the sum of lengths. *)
let len_app (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_len a + ll_len b } =
  let ab = Vlist.append a b in
  Vlist.length ab

(* ll_mem_cons: x is a member of (cons x l) -- the (x = x) disjunct. *)
let mem_cons_self (x : int) (l : Vlist.t) : bool{ _ = true } =
  let l' = Vlist.cons x l in
  Vlist.mem x l'

(* ll_mem_app: membership in append is membership in either side. *)
let mem_app (x : int) (a : Vlist.t) (b : Vlist.t)
  : bool{ _ = (ll_mem x a || ll_mem x b) } =
  let ab = Vlist.append a b in
  Vlist.mem x ab

(* ll_isnil_nil: the empty list is nil (ll_isnil is opaque, so the law fires). *)
let empty_is_empty : bool{ _ = true } =
  let e = Vlist.empty () in
  Vlist.is_empty e

(* ll_nil_not_mem: nothing is a member of the empty list (eliminator base case). *)
let empty_no_mem (x : int) : bool{ _ = false } =
  let e = Vlist.empty () in
  Vlist.mem x e

(* ll_not_isnil_cons: a cons is never empty. *)
let cons_not_empty (x : int) (l : Vlist.t) : bool{ _ = false } =
  let l' = Vlist.cons x l in
  Vlist.is_empty l'

(* ll_head_cons: head of (cons x l) is x. Also exercises head's precondition
   not (ll_isnil (ll_cons x l)), discharged by ll_not_isnil_cons. *)
let head_cons (x : int) (l : Vlist.t) : int{ _ = x } =
  let l' = Vlist.cons x l in
  Vlist.head l'

(* ll_tail_cons: tail of (cons x l) has the same length as l. *)
let tail_cons_len (x : int) (l : Vlist.t) : int{ _ = ll_len l } =
  let l' = Vlist.cons x l in
  let tl = Vlist.tail l' in
  Vlist.length tl

(* ll_cons_head_tail: on an abstract non-empty list, rebuilding from head and
   tail recovers the list -- length of the rebuild equals length of l. The
   reconstruction law fires only when its trigger cons(head l)(tail l) is
   materialized, so the client rebuilds it explicitly. *)
let recon_rebuild (l : Vlist.t{ not (ll_isnil _) }) : int{ _ = ll_len l } =
  let h = Vlist.head l in
  let tl = Vlist.tail l in
  let rebuilt = Vlist.cons h tl in
  Vlist.length rebuilt
