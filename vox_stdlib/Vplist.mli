(* Vplist: the element-polymorphic verified list -- Vlist generalized to
   ['a] -- behind a via-ABSTRACTED interface.  [t] is [refines ('a plist)],
   so a client binds it at the PARAMETERIZED Lean list model [PList a] and
   reasons in list vocabulary (pl_len / pl_mem / pl_app / pl_cons /
   pl_isnil).  Genericity is carried by the parameterized ghost sort
   ['a plist [@@vox.sort lean "PList"]] over [inductive PList (a : Type)] --
   the poly study's sub-problem B, the ONLY sound route (the VoxU "opaque
   element" cheat is ill-typed, study F-B4).  Ops that only STORE / COUNT /
   CONCATENATE elements prove at the ABSTRACT element sort and so instantiate
   at any concrete element ([int t], [string t] alike, study F-B1).
   House rules as Vlist: specs mention DEFS, never PList CONSTRUCTORS; the
   [pl_] prefix keeps names globally unique; a recursive-over-arg def is
   [expose]d, a non-recursive one is opaque so its laws stay live (Amendment A).

   TWO study gaps are load-bearing here, both routed around, not fought:
   - F-B2 (unspecced empty): a polymorphic NULLARY via constructor cannot
     carry a refinement -- the via injection of [Nil] leaves the Lean type
     parameter [a] an unsolved metavariable, even at a concrete element type.
     So [empty] ships with NO spec; emptiness stays OBSERVABLE via [is_empty]
     (whose [_ : bool] argument pins [a]), and a client that KNOWS a list is
     empty asserts it as a [{ _ = pl_nil }] precondition.
   - F-B3 (no decidable mem): a Bool-valued [mem] is a DECIDER and needs
     [DecidableEq a], which the abstract element sort lacks.  So membership
     ships ONLY as the Prop-valued model predicate [pl_mem] and its laws
     (a client STATES membership in a spec; it cannot QUERY it at runtime). *)
open Vhof
type 'a plist [@@vox.sort lean "PList"]
type 'a t : value refines ('a plist)

[%%vox.lean {lean|
public inductive PList (a : Type) where
  | PNil : PList a
  | PCons : a -> PList a -> PList a

-- pl_cons / pl_isnil / pl_nil are non-recursive, so public WITHOUT expose
-- (Amendment A): a client names them in specs but grind treats them
-- opaquely, keeping the cons / isnil laws below LIVE.
@[grind] public def pl_cons {a : Type} (x : a) (l : PList a) : PList a := .PCons x l

@[grind] public def pl_isnil {a : Type} : PList a -> Prop
  | .PNil => True
  | .PCons _ _ => False

@[grind] public def pl_nil {a : Type} : PList a := .PNil

-- pl_len / pl_mem / pl_app recurse over the abstract PList arg, so exposing
-- them is sound: grind cannot unfold past the first step, so their inductive
-- laws stay live regardless.
@[grind, expose] public def pl_len {a : Type} : PList a -> Int
  | .PNil => 0
  | .PCons _ t => 1 + pl_len t

@[grind, expose] public def pl_mem {a : Type} (x : a) : PList a -> Prop
  | .PNil => False
  | .PCons y t => x = y ∨ pl_mem x t

@[grind, expose] public def pl_app {a : Type} : PList a -> PList a -> PList a
  | .PNil, m => m
  | .PCons x t, m => .PCons x (pl_app t m)

-- The algebra, shipped as obligations (axiom here, theorem in the .ml).  The
-- membership laws prove GENERICALLY: [pl_mem] is Prop-valued, so [x = y] is
-- Lean [Eq] (always available), never a [DecidableEq] obligation.
--
-- NB there is deliberately NO empty-non-membership law: on this compiler
-- [pl_mem x pl_nil] is discharged by grind reducing the nullary [pl_nil]
-- against the exposed [pl_mem] equations, so such a law would be DEAD (its
-- removal does not break any client goal -- verified).  [pl_isnil_nil]
-- survives only because [pl_isnil] is a NON-exposed match (grind has no
-- equations for it), which is the exact asymmetry Amendment A predicts.
public axiom pl_isnil_nil {a : Type} : pl_isnil (@pl_nil a)
grind_pattern pl_isnil_nil => pl_isnil (@pl_nil a)

@[grind] public axiom pl_not_isnil_cons {a : Type} (x : a) (l : PList a) :
    ¬ pl_isnil (pl_cons x l)

public axiom pl_len_nonneg {a : Type} (l : PList a) : pl_len l >= 0
grind_pattern pl_len_nonneg => pl_len l

public axiom pl_len_cons {a : Type} (x : a) (l : PList a) :
    pl_len (pl_cons x l) = 1 + pl_len l
grind_pattern pl_len_cons => pl_len (pl_cons x l)

public axiom pl_len_app {a : Type} (p q : PList a) :
    pl_len (pl_app p q) = pl_len p + pl_len q
grind_pattern pl_len_app => pl_len (pl_app p q)

public axiom pl_mem_cons {a : Type} (x y : a) (l : PList a) :
    pl_mem x (pl_cons y l) = (x = y ∨ pl_mem x l)
grind_pattern pl_mem_cons => pl_mem x (pl_cons y l)

public axiom pl_mem_app {a : Type} (x : a) (p q : PList a) :
    pl_mem x (pl_app p q) = (pl_mem x p ∨ pl_mem x q)
grind_pattern pl_mem_app => pl_mem x (pl_app p q)
-- pl_memr: membership up to the client decider's equality (eqHolds e), the
-- eq-param route (probe3) around the missing DecidableEq at the abstract sort.
@[grind, expose] public def pl_memr {a : Type} (e : a -> a -> Prop) (x : a) : PList a -> Prop
  | .PNil => False
  | .PCons y t => eqHolds e x y \/ pl_memr e x t
-- pl_dedup_sub: dedup's result is a SUBSET of its input (holds for ANY
-- decider e; a membership-EQUALITY spec would need e to be an equivalence).
@[grind, expose] public def pl_dedup_sub {a : Type} (e : a -> a -> Prop) (l r : PList a) : Prop :=
  forall y, pl_memr e y r -> pl_memr e y l
-- pl_remove_ok: remove's honest spec for an ARBITRARY decider e -- x is not
-- a member of the result (up to e) AND the result is a subset of the input.
-- (The full membership-EQUALITY spec ∀y, mem y r <-> (¬e x y /\ mem y l) needs
-- e to be an EQUIVALENCE; it is NOT PROVABLE for an arbitrary decider -- see
-- notes/vplist.md. These two conjuncts hold for any e.)
@[grind, expose] public def pl_remove_ok {a : Type} (e : a -> a -> Prop) (x : a) (l r : PList a) : Prop :=
  (¬ pl_memr e x r) /\ (forall y, pl_memr e y r -> pl_memr e y l)
|lean}]

(* empty ships UNSPECCED (study F-B2): the emptiness fact [_ = pl_nil] cannot
   be carried on a nullary via constructor's result.  A client observes
   emptiness by [is_empty (empty ()) = true] at runtime, or asserts it
   statically as a [{ _ = pl_nil }] precondition. *)
val empty : (u : unit) -> 'a t
val cons : (x : 'a) -> (l : 'a t) -> 'a t{ _ = pl_cons x l }
val is_empty : (l : 'a t) -> bool{ _ = pl_isnil l }
val length : (l : 'a t) -> int{ _ = pl_len l }
val append : (p : 'a t) -> (q : 'a t) -> 'a t{ _ = pl_app p q }

(* Bool membership via a client-supplied decider (eq-param, probe3): [eq] is a
   bool decider whose contract ties it to the Prop model equality [eqHolds e];
   membership is then decidable up to [e]. Escapes the DecidableEq-at-abstract
   wall at zero new TCB. *)
val mem :
  (e : (('a -> 'a -> bool) [@vox.total])) ->
  (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
  (x : 'a) -> (l : 'a t) -> bool{ _ = pl_memr e x l }

(* dedup: drop e-duplicates. Result is a SUBSET of the input (membership up to
   the decider e). The end-to-end demonstration that eq-param membership
   unblocks abstract-'a set work (WP-6-C). *)
val dedup :
  (e : (('a -> 'a -> bool) [@vox.total])) ->
  (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
  (l : 'a t) -> 'a t{ pl_dedup_sub e l _ }

(* remove x l: drop every element the decider e equates to x. For an arbitrary
   decider the result satisfies pl_remove_ok (x gone + subset); the stronger
   membership-equality spec needs e to be an equivalence (notes/vplist.md). *)
val remove :
  (e : (('a -> 'a -> bool) [@vox.total])) ->
  (eq : ((x : 'a) -> (y : 'a) -> bool{ _ = eqHolds e x y })) ->
  (x : 'a) -> (l : 'a t) -> 'a t{ pl_remove_ok e x l _ }
