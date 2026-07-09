(* Pays Vplist.mli's obligations over the EXPOSED poly cons-list repr, at the
   ABSTRACT element sort (so ONE proof serves every instantiation).  Because
   [t] is a native poly ADT, its Lean correspondent is the auto-derived
   parameterized inductive [Vox_Vplist_t a] and the ops recurse DIRECTLY over
   it -- native `match`, native PNil/PCons, top-level `let rec`.  The old via
   machinery (pl_repr, refine_ peels, #31 skeleton threading) is GONE.  The
   model defs are RESTATED here without [public] (the model-duplication tax);
   each .mli axiom is discharged by a same-named, same-typed theorem. *)
open Vhof
type 'a t = PNil | PCons of 'a * 'a t

[%%vox.lean {lean|
@[grind] def pl_cons {a : Type} (x : a) (l : Vox_Vplist_t a) : Vox_Vplist_t a := .PCons x l

@[grind] def pl_isnil {a : Type} : Vox_Vplist_t a -> Prop
  | .PNil => True
  | .PCons _ _ => False

@[grind] def pl_nil {a : Type} : Vox_Vplist_t a := .PNil

@[grind, expose] def pl_len {a : Type} : Vox_Vplist_t a -> Int
  | .PNil => 0
  | .PCons _ t => 1 + pl_len t

@[grind, expose] def pl_mem {a : Type} (x : a) : Vox_Vplist_t a -> Prop
  | .PNil => False
  | .PCons y t => x = y ∨ pl_mem x t

@[grind, expose] def pl_app {a : Type} : Vox_Vplist_t a -> Vox_Vplist_t a -> Vox_Vplist_t a
  | .PNil, m => m
  | .PCons x t, m => .PCons x (pl_app t m)

theorem pl_isnil_nil {a : Type} : pl_isnil (@pl_nil a) := by grind
grind_pattern pl_isnil_nil => pl_isnil (@pl_nil a)

@[grind] theorem pl_not_isnil_cons {a : Type} (x : a) (l : Vox_Vplist_t a) :
    ¬ pl_isnil (pl_cons x l) := by grind

theorem pl_len_nonneg {a : Type} (l : Vox_Vplist_t a) : pl_len l >= 0 := by
  induction l <;> grind
grind_pattern pl_len_nonneg => pl_len l

theorem pl_len_cons {a : Type} (x : a) (l : Vox_Vplist_t a) :
    pl_len (pl_cons x l) = 1 + pl_len l := by grind
grind_pattern pl_len_cons => pl_len (pl_cons x l)

theorem pl_len_app {a : Type} (p q : Vox_Vplist_t a) :
    pl_len (pl_app p q) = pl_len p + pl_len q := by
  induction p <;> grind
grind_pattern pl_len_app => pl_len (pl_app p q)

theorem pl_mem_cons {a : Type} (x y : a) (l : Vox_Vplist_t a) :
    pl_mem x (pl_cons y l) = (x = y ∨ pl_mem x l) := by grind
grind_pattern pl_mem_cons => pl_mem x (pl_cons y l)

theorem pl_mem_app {a : Type} (x : a) (p q : Vox_Vplist_t a) :
    pl_mem x (pl_app p q) = (pl_mem x p ∨ pl_mem x q) := by
  induction p <;> grind
grind_pattern pl_mem_app => pl_mem x (pl_app p q)
-- pl_memr: membership up to the client decider's equality (eqHolds e), the
-- eq-param route (probe3) around the missing DecidableEq at the abstract sort.
@[grind, expose] def pl_memr {a : Type} (e : a -> a -> Prop) (x : a) : Vox_Vplist_t a -> Prop
  | .PNil => False
  | .PCons y t => eqHolds e x y \/ pl_memr e x t
-- pl_dedup_sub: dedup's result is a SUBSET of its input (holds for ANY
-- decider e; a membership-EQUALITY spec would need e to be an equivalence).
@[grind, expose] def pl_dedup_sub {a : Type} (e : a -> a -> Prop) (l r : Vox_Vplist_t a) : Prop :=
  forall y, pl_memr e y r -> pl_memr e y l
-- pl_remove_ok: remove's honest spec for an ARBITRARY decider e -- x is not
-- a member of the result (up to e) AND the result is a subset of the input.
-- (The full membership-EQUALITY spec ∀y, mem y r <-> (¬e x y /\ mem y l) needs
-- e to be an EQUIVALENCE; it is NOT PROVABLE for an arbitrary decider -- see
-- notes/vplist.md. These two conjuncts hold for any e.)
@[grind, expose] def pl_remove_ok {a : Type} (e : a -> a -> Prop) (x : a) (l r : Vox_Vplist_t a) : Prop :=
  (¬ pl_memr e x r) /\ (forall y, pl_memr e y r -> pl_memr e y l)
|lean}]

let empty : (u : unit) -> 'a t =
  fun _ -> PNil

let cons : (x : 'a) -> (l : 'a t) -> 'a t{ _ = pl_cons x l } =
  fun x l -> (PCons (x, l) : 'a t{ _ = pl_cons x l })

let is_empty : (l : 'a t) -> bool{ _ = pl_isnil l } =
  fun l -> match l with PNil -> true | PCons (_, _) -> false

let rec length : (l : 'a t) -> int{ _ = pl_len l } =
  fun l -> match l with PNil -> 0 | PCons (_, r) -> let n = length r in 1 + n

let rec append : (p : 'a t) -> (q : 'a t) -> 'a t{ _ = pl_app p q } =
  fun p q -> match p with
    | PNil -> q
    | PCons (x, r) -> let rest = append r q in PCons (x, rest)

let mem :
      (e : (('a -> 'a -> bool) [@vox.total])) ->
      (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
      (x : 'a) -> (l : 'a t) -> bool{ _ = pl_memr e x l } =
  fun e eq x l ->
    ignore e;
    let rec go : (u : 'a t) -> bool{ _ = pl_memr e x u } =
      fun u -> match u with
        | PNil -> false
        | PCons (y, r) -> if eq x y then true else go r
    in
    go l

let dedup :
      (e : (('a -> 'a -> bool) [@vox.total])) ->
      (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
      (l : 'a t) -> 'a t{ pl_dedup_sub e l _ } =
  fun e eq l ->
    ignore e;
    let rec tmem : (x : 'a) -> (u : 'a t) -> bool{ _ = pl_memr e x u } =
      fun x u -> match u with
        | PNil -> false
        | PCons (y, r) -> if eq x y then true else tmem x r
    in
    let rec go : (u : 'a t) -> 'a t{ pl_dedup_sub e u _ } =
      fun u -> match u with
        | PNil -> (PNil : 'a t{ pl_dedup_sub e u _ })
        | PCons (x, r) ->
            let d = go r in
            if tmem x d then (d : 'a t{ pl_dedup_sub e u _ })
            else (PCons (x, d) : 'a t{ pl_dedup_sub e u _ })
    in
    go l

let remove :
      (e : (('a -> 'a -> bool) [@vox.total])) ->
      (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
      (x : 'a) -> (l : 'a t) -> 'a t{ pl_remove_ok e x l _ } =
  fun e eq x l ->
    ignore e;
    let rec go : (u : 'a t) -> 'a t{ pl_remove_ok e x u _ } =
      fun u -> match u with
        | PNil -> (PNil : 'a t{ pl_remove_ok e x u _ })
        | PCons (y, r) ->
            let t' = go r in
            if eq x y then (t' : 'a t{ pl_remove_ok e x u _ })
            else (PCons (y, t') : 'a t{ pl_remove_ok e x u _ })
    in
    go l
