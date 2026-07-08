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

(* ============================ HOF surface (WP-0) ============================
   map / filter / fold_left / for_all / exists / rev / nth / find_opt. Relations
   and predicates are supplied at the CALL SITE as OCaml lambdas (reflected +
   substituted); refinement GOALS name block abbrevs (a lambda may not appear in
   refinement text). Verifies against Vlist.cmi + VoxSig_Vlist.olean AND
   Voption.cmi + VoxSig_Voption.olean (find_opt's result type). *)

[@@@warning "-6-32-26-27"]

[%%vox.lean {lean|
@[grind, expose] abbrev pPos : Int -> Prop := fun x => x > 0
@[grind, expose] abbrev pNn  : Int -> Prop := fun x => x >= 0
@[grind, expose] abbrev pGt5 : Int -> Prop := fun x => x > 5
|lean}]

(* rev: length + membership preserved (ll_len_rev / ll_mem_rev). *)
let rev_len (l : Vlist.t) : int{ _ = ll_len l } = Vlist.length (Vlist.rev l)
let rev_mem (x : int) (l : Vlist.t) : bool{ _ = ll_mem x l } = Vlist.mem x (Vlist.rev l)

(* nth: index 0 of a cons is the head element (ll_nth_cons). *)
let nth0_cons (x : int) (l : Vlist.t) : int{ _ = x } =
  let c = Vlist.cons x l in Vlist.nth 0 c

(* map: length preserved (ll_listRel_len fires on the listRel contract). *)
(* map preserves length (ll_listRel_len fires on the listRel contract); the
   relation + callback are supplied as call-site lambdas (total). *)
let map_len (l : Vlist.t) : int{ _ = ll_len l } =
  let m = Vlist.map (fun a b -> a <= b) (fun x -> x + 1) l in Vlist.length m

(* filter: every kept element satisfies p (allP). *)
let filter_pos (l : Vlist.t) : Vlist.t{ ll_allP pPos _ } =
  Vlist.filter (fun x -> x > 0) (fun x -> x > 0) l

(* fold_left, SUM step: exact result (ll_relFold_sum_exact, over abstract l). *)
let fold_sum (l : Vlist.t) : int{ _ = ll_sum l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) 0 l

(* fold_left, COUNT step: exact result = length (ll_relFold_count_exact). *)
let fold_count (l : Vlist.t) : int{ _ = ll_len l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + 1) (fun acc x -> acc + 1) 0 l

(* fold_left, SUM from a nonzero init. *)
let fold_sum_from (init : int) (l : Vlist.t) : int{ _ = init + ll_sum l } =
  Vlist.fold_left (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) init l

(* for_all / exists: bool result equals the lifted predicate. *)
let forall_nn (l : Vlist.t) : bool{ _ = ll_allP pNn l } =
  Vlist.for_all (fun x -> x >= 0) (fun x -> x >= 0) l
let exists_gt5 (l : Vlist.t) : bool{ _ = ll_exP pGt5 l } =
  Vlist.exists (fun x -> x > 5) (fun x -> x > 5) l

(* find_opt: the result carries the find spec for the reflected predicate
   (Some -> value satisfies p AND is a member; None -> nothing satisfies p). *)
let find_pos (l : Vlist.t) : Voption.t{ ll_find_result pPos l _ } =
  Vlist.find_opt (fun x -> x > 0) (fun x -> x > 0) l
