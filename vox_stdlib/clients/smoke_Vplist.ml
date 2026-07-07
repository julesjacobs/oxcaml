(* Smoke client for Vplist: one goal per shipped law, each forced so the
   solver MUST apply that law's grind_pattern (blueprint 6.7 dead-law check).
   EVERY law is exercised at BOTH [int Vplist.t] AND [string Vplist.t] -- the
   poly study's S_param-resolution check: a law proved at the abstract element
   sort must fire at each concrete instantiation (the parameter sort resolves
   through the [Tconstr] head [int]/[string]).  Verifies against Vplist.cmi +
   VoxSig_Vplist.olean only -- no view of the 'a tree repr or pl_repr.

   Membership is Prop-valued (Vplist ships no decidable Bool [mem], study
   F-B3), so its laws are forced by pure-Prop [unit{ ... }] goals rather than
   an op result.  The empty-algebra laws (pl_isnil_nil / pl_nil_not_mem) are
   forced under a [{ _ = pl_nil }] client HYPOTHESIS: [empty] is unspecced
   (study F-B2), so a client cannot derive [pl_nil] from [empty ()], but it
   CAN assert a list is empty as a precondition -- which is exactly how these
   two laws stay client-reachable.  Nested op-call results are let-bound (C1
   friction; see notes/vplist.md). *)

(* ===== int Vplist.t ===== *)

(* pl_len_nonneg *)
let i_nonneg (l : int Vplist.t) : int{ _ >= 0 } = Vplist.length l

(* pl_len_cons *)
let i_len_cons (x : int) (l : int Vplist.t) : int{ _ = 1 + pl_len l } =
  let l' = Vplist.cons x l in
  Vplist.length l'

(* pl_len_app *)
let i_len_app (p : int Vplist.t) (q : int Vplist.t) : int{ _ = pl_len p + pl_len q } =
  let pq = Vplist.append p q in
  Vplist.length pq

(* pl_not_isnil_cons *)
let i_cons_not_empty (x : int) (l : int Vplist.t) : bool{ _ = false } =
  let l' = Vplist.cons x l in
  Vplist.is_empty l'

(* pl_isnil_nil (forced under a { _ = pl_nil } hypothesis) *)
let i_nil_is_empty (l : int Vplist.t{ _ = pl_nil }) : bool{ _ = true } =
  Vplist.is_empty l


(* pl_mem_cons *)
let i_mem_cons (x : int) (y : int) (l : int Vplist.t)
  : unit{ pl_mem x (pl_cons y l) = (x = y || pl_mem x l) } = ()

(* pl_mem_app *)
let i_mem_app (x : int) (p : int Vplist.t) (q : int Vplist.t)
  : unit{ pl_mem x (pl_app p q) = (pl_mem x p || pl_mem x q) } = ()

(* ===== string Vplist.t (same laws, other instantiation) ===== *)

(* pl_len_nonneg *)
let s_nonneg (l : string Vplist.t) : int{ _ >= 0 } = Vplist.length l

(* pl_len_cons *)
let s_len_cons (x : string) (l : string Vplist.t) : int{ _ = 1 + pl_len l } =
  let l' = Vplist.cons x l in
  Vplist.length l'

(* pl_len_app *)
let s_len_app (p : string Vplist.t) (q : string Vplist.t)
  : int{ _ = pl_len p + pl_len q } =
  let pq = Vplist.append p q in
  Vplist.length pq

(* pl_not_isnil_cons *)
let s_cons_not_empty (x : string) (l : string Vplist.t) : bool{ _ = false } =
  let l' = Vplist.cons x l in
  Vplist.is_empty l'

(* pl_isnil_nil *)
let s_nil_is_empty (l : string Vplist.t{ _ = pl_nil }) : bool{ _ = true } =
  Vplist.is_empty l


(* pl_mem_cons *)
let s_mem_cons (x : string) (y : string) (l : string Vplist.t)
  : unit{ pl_mem x (pl_cons y l) = (x = y || pl_mem x l) } = ()

(* pl_mem_app *)
let s_mem_app (x : string) (p : string Vplist.t) (q : string Vplist.t)
  : unit{ pl_mem x (pl_app p q) = (pl_mem x p || pl_mem x q) } = ()
