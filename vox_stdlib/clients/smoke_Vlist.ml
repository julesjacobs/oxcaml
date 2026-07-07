(* Smoke client for Vlist: one goal per shipped law, each written so the
   solver MUST apply that law's grind_pattern to discharge it (blueprint
   6.7 dead-law check).  Verifies against Vlist.cmi + VoxSig_Vlist.olean
   only -- no view of the tree repr or ll_repr.  Post-#53 (finding C1):
   every Vlist op has an EQUATIONAL result contract ({ _ = ll_... }), so its
   call result now passes INLINE to a dependent parameter -- the C1 let-binds
   are removed here, including the reconstruction cons(head l)(tail l) whose
   trigger materializes inline (see notes/vlist.md). *)

(* ll_len_nonneg: the length of any list is non-negative. *)
let nonneg (l : Vlist.t) : int{ _ >= 0 } = Vlist.length l

(* ll_len_cons: length after a cons is one more. *)
let len_cons (x : int) (l : Vlist.t) : int{ _ = 1 + ll_len l } =
  Vlist.length (Vlist.cons x l)

(* ll_len_app: length of append is the sum of lengths. *)
let len_app (a : Vlist.t) (b : Vlist.t) : int{ _ = ll_len a + ll_len b } =
  Vlist.length (Vlist.append a b)

(* ll_mem_cons: x is a member of (cons x l) -- the (x = x) disjunct. *)
let mem_cons_self (x : int) (l : Vlist.t) : bool{ _ = true } =
  Vlist.mem x (Vlist.cons x l)

(* ll_mem_app: membership in append is membership in either side. *)
let mem_app (x : int) (a : Vlist.t) (b : Vlist.t)
  : bool{ _ = (ll_mem x a || ll_mem x b) } =
  Vlist.mem x (Vlist.append a b)

(* ll_isnil_nil: the empty list is nil (ll_isnil is opaque, so the law fires). *)
let empty_is_empty : bool{ _ = true } =
  Vlist.is_empty (Vlist.empty ())

(* ll_nil_not_mem: nothing is a member of the empty list (eliminator base case). *)
let empty_no_mem (x : int) : bool{ _ = false } =
  Vlist.mem x (Vlist.empty ())

(* ll_not_isnil_cons: a cons is never empty. *)
let cons_not_empty (x : int) (l : Vlist.t) : bool{ _ = false } =
  Vlist.is_empty (Vlist.cons x l)

(* ll_head_cons: head of (cons x l) is x. Also exercises head's precondition
   not (ll_isnil (ll_cons x l)), discharged by ll_not_isnil_cons. *)
let head_cons (x : int) (l : Vlist.t) : int{ _ = x } =
  Vlist.head (Vlist.cons x l)

(* ll_tail_cons: tail of (cons x l) has the same length as l. *)
let tail_cons_len (x : int) (l : Vlist.t) : int{ _ = ll_len l } =
  Vlist.length (Vlist.tail (Vlist.cons x l))

(* ll_cons_head_tail: on an abstract non-empty list, rebuilding from head and
   tail recovers the list -- length of the rebuild equals length of l. The
   reconstruction law fires only when its trigger cons(head l)(tail l) is
   materialized, so the client rebuilds it explicitly. *)
let recon_rebuild (l : Vlist.t{ not (ll_isnil _) }) : int{ _ = ll_len l } =
  Vlist.length (Vlist.cons (Vlist.head l) (Vlist.tail l))
