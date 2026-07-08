(* Vlist: a verified list with an EXPOSED representation -- `type t = Nil
   | Cons of int * t`.  This is the deliberate exception among the stdlib
   containers: lists are how the compiler itself works (structural,
   canonical, one obvious shape), so a client should pattern-match and
   recurse over a Vlist exactly like a built-in list.  Vmap/Vset stay
   ABSTRACT (via/refines) because they have representation freedom worth
   hiding; a list does not.  See notes/vlist.md (transparency rationale).

   Consequence for the model: t's Lean correspondent is its NATIVE derived
   inductive `Vox_Vlist_t` (constructors .Nil/.Cons) -- there is NO via
   remap to a separate list model.  The model vocabulary
   (ll_len/ll_mem/ll_app/ll_cons/ll_isnil/...) is defined directly over
   `Vox_Vlist_t`, so it REDUCES on the .Nil/.Cons a client builds and
   matches: client-side structural recursion verifies (patterns mint facts,
   the deep-pattern machinery applies).  The recursive defs are `expose`d
   for exactly that reduction; the non-recursive wrappers (ll_cons / ll_nil
   / ll_isnil / ll_head / ll_tail) stay opaque so their reduction facts ship
   as the named laws below and stay LIVE for `Vlist.cons`/`empty`-built
   lists (the same dead-law discipline as Voption's accessors).
   House rules: specs mention DEFS (ll_cons), not the raw constructors,
   where a def exists; names carry the [ll_] prefix so they never collide
   with a co-imported unit's names. *)
open Vhof
open Voption
type t = Nil | Cons of int * t

[%%vox.lean {lean|
-- ll_cons is public (clients name it in specs) but NOT [expose]d: it is a
-- non-recursive constructor wrapper, so an exposed body would let grind unfold
-- ll_cons x l => .Cons x l and discharge ll_len_cons / ll_mem_cons WITHOUT the
-- laws, leaving them dead (Phase-C soundness finding).  Kept opaque, those two
-- laws are the only route to ll_len / ll_mem of a `Vlist.cons`-built list.  The
-- recursive defs below stay [expose]d: unfolding them one step never discharges
-- their inductive laws, so those laws remain live regardless -- AND that
-- exposure is what lets a client's own .Nil/.Cons reduce them (transparency).
@[grind] public def ll_cons (x : Int) (l : Vox_Vlist_t) : Vox_Vlist_t := .Cons x l

-- ll_isnil is public but NOT [expose]d, same reasoning as ll_cons.
@[grind] public def ll_isnil : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons _ _ => False

-- ll_nil is the empty-list model (opaque, like ll_cons); empty's spec is
-- structural (_ = ll_nil), so ll_isnil_nil is load-bearing at is_empty (empty ()).
@[grind] public def ll_nil : Vox_Vlist_t := .Nil

@[grind, expose] public def ll_len : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons _ t => 1 + ll_len t

-- ll_head / ll_tail: the destructor vocabulary for the head/tail eliminator.
-- Non-recursive, so opaque -- their reduction facts ship as ll_head_cons /
-- ll_tail_cons below.  (With the repr exposed a client can also just match.)
@[grind] public def ll_head : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons h _ => h

@[grind] public def ll_tail : Vox_Vlist_t -> Vox_Vlist_t
  | .Nil => .Nil
  | .Cons _ t => t

@[grind, expose] public def ll_mem (x : Int) : Vox_Vlist_t -> Prop
  | .Nil => False
  | .Cons y t => x = y ∨ ll_mem x t

@[grind, expose] public def ll_app : Vox_Vlist_t -> Vox_Vlist_t -> Vox_Vlist_t
  | .Nil, m => m
  | .Cons x t, m => .Cons x (ll_app t m)

-- The algebra, shipped as obligations (axiom here, theorem in the .ml).
public axiom ll_isnil_nil : ll_isnil ll_nil
grind_pattern ll_isnil_nil => ll_isnil ll_nil

@[grind] public axiom ll_not_isnil_cons (x : Int) (l : Vox_Vlist_t) :
    ¬ ll_isnil (ll_cons x l)

public axiom ll_len_nonneg (l : Vox_Vlist_t) : ll_len l >= 0
grind_pattern ll_len_nonneg => ll_len l

public axiom ll_len_cons (x : Int) (l : Vox_Vlist_t) :
    ll_len (ll_cons x l) = 1 + ll_len l
grind_pattern ll_len_cons => ll_len (ll_cons x l)

public axiom ll_len_app (a b : Vox_Vlist_t) :
    ll_len (ll_app a b) = ll_len a + ll_len b
grind_pattern ll_len_app => ll_len (ll_app a b)

public axiom ll_mem_cons (x y : Int) (l : Vox_Vlist_t) :
    ll_mem x (ll_cons y l) = (x = y ∨ ll_mem x l)
grind_pattern ll_mem_cons => ll_mem x (ll_cons y l)

public axiom ll_mem_app (x : Int) (a b : Vox_Vlist_t) :
    ll_mem x (ll_app a b) = (ll_mem x a ∨ ll_mem x b)
grind_pattern ll_mem_app => ll_mem x (ll_app a b)

-- Empty is member-free.  ll_nil is opaque, so this is the only route to
-- ll_mem x (empty ()) = False for a Mech-A eliminator (Vmap.keys /
-- Vset.elements) that builds a Vlist by recursion.
public axiom ll_nil_not_mem (x : Int) : ¬ ll_mem x ll_nil
grind_pattern ll_nil_not_mem => ll_mem x ll_nil

-- Eliminator algebra (head/tail).
public axiom ll_head_cons (x : Int) (l : Vox_Vlist_t) : ll_head (ll_cons x l) = x
grind_pattern ll_head_cons => ll_head (ll_cons x l)

public axiom ll_tail_cons (x : Int) (l : Vox_Vlist_t) : ll_tail (ll_cons x l) = l
grind_pattern ll_tail_cons => ll_tail (ll_cons x l)

public axiom ll_cons_head_tail (l : Vox_Vlist_t) (h : ¬ ll_isnil l) :
    ll_cons (ll_head l) (ll_tail l) = l
grind_pattern ll_cons_head_tail => ll_cons (ll_head l) (ll_tail l)


-- ===== HOF KIT: per-container relational lifts over Vox_Vlist_t =====
-- ll_listRel: b is pointwise r-related to a (same length) -- map's spec.
@[grind, expose] public def ll_listRel (r : IntRel) : Vox_Vlist_t -> Vox_Vlist_t -> Prop
  | .Nil, .Nil => True
  | .Cons a s, .Cons b t => r a b /\ ll_listRel r s t
  | _, _ => False
-- ll_allP / ll_exP: every / some element satisfies p -- filter/for_all/exists.
@[grind, expose] public def ll_allP (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons x t => pHolds p x /\ ll_allP p t
@[grind, expose] public def ll_exP (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => False
  | .Cons x t => pHolds p x \/ ll_exP p t
-- ll_relFold: fold_left with a TERNARY element-aware step (acc, elem, acc').
@[grind, expose] public def ll_relFold (r : IntRel3) : Vox_Vlist_t -> Int -> Int -> Prop
  | .Nil, init, final => init = final
  | .Cons x t, init, final => exists acc, r init x acc /\ ll_relFold r t acc final
-- ll_sum: list sum accessor for fold's exact sum-law.
@[grind, expose] public def ll_sum : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons x t => x + ll_sum t
-- ll_rev / ll_nth: first-order surface ops.
@[grind, expose] public def ll_rev : Vox_Vlist_t -> Vox_Vlist_t
  | .Nil => .Nil
  | .Cons x t => ll_app (ll_rev t) (.Cons x .Nil)
@[grind, expose] public def ll_nth : Int -> Vox_Vlist_t -> Int
  | _, .Nil => 0
  | i, .Cons x t => if i <= 0 then x else ll_nth (i-1) t
-- ll_nosat: no element satisfies p -- find_opt's None-case spec.
@[grind, expose] public def ll_nosat (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons x t => (¬ pHolds p x) /\ ll_nosat p t
-- ll_find_result: find_opt's spec (references imported Voption model). Some ->
-- the found value satisfies p AND is a member; None -> no element satisfies p.
@[grind, expose] public def ll_find_result (p : IntPred) (l : Vox_Vlist_t) (o : Vox_Voption_t) : Prop :=
  (vo_is_some o -> (pHolds p (vo_get o) /\ ll_mem (vo_get o) l)) /\
  ((¬ vo_is_some o) -> ll_nosat p l)

-- ===== HOF laws (obligations) =====
public axiom ll_listRel_len (r : IntRel) (a b : Vox_Vlist_t) :
    ll_listRel r a b -> ll_len a = ll_len b
grind_pattern ll_listRel_len => ll_listRel r a b
public axiom ll_len_rev (l : Vox_Vlist_t) : ll_len (ll_rev l) = ll_len l
grind_pattern ll_len_rev => ll_len (ll_rev l)
public axiom ll_mem_rev (x : Int) (l : Vox_Vlist_t) : ll_mem x (ll_rev l) = ll_mem x l
grind_pattern ll_mem_rev => ll_mem x (ll_rev l)
public axiom ll_nth_cons (i x : Int) (l : Vox_Vlist_t) :
    ll_nth i (ll_cons x l) = (if i <= 0 then x else ll_nth (i-1) l)
grind_pattern ll_nth_cons => ll_nth i (ll_cons x l)

-- ll_sum cons/append laws (F-B2): opaque ll_cons/ll_app-in-a-wrapper won't
-- reduce ll_sum, so these ship the sum algebra a `Vlist.cons`/`append`-built
-- list needs (native .Cons-built lists get it from exposed ll_sum by reduction).
public axiom ll_sum_cons (x : Int) (l : Vox_Vlist_t) :
    ll_sum (ll_cons x l) = x + ll_sum l
grind_pattern ll_sum_cons => ll_sum (ll_cons x l)
public axiom ll_sum_app (a b : Vox_Vlist_t) :
    ll_sum (ll_app a b) = ll_sum a + ll_sum b
grind_pattern ll_sum_app => ll_sum (ll_app a b)

-- ===== fold EXACT-output laws (.mli-only public theorems; ride VoxSig to clients) =====
public theorem ll_relFold_sum_exact (r : IntRel3) (hr : forall a x c, r a x c -> c = a + x) :
    forall (xs : Vox_Vlist_t) (init final : Int),
      ll_relFold r xs init final -> final = init + ll_sum xs := by
  intro xs
  induction xs with
  | Nil => intro init final h; simp only [ll_relFold, ll_sum] at *; omega
  | Cons x t ih =>
      intro init final h
      simp only [ll_relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [ll_sum]; omega
grind_pattern ll_relFold_sum_exact => ll_relFold r xs init final
public theorem ll_relFold_count_exact (r : IntRel3) (hr : forall a x c, r a x c -> c = a + 1) :
    forall (xs : Vox_Vlist_t) (init final : Int),
      ll_relFold r xs init final -> final = init + ll_len xs := by
  intro xs
  induction xs with
  | Nil => intro init final h; simp only [ll_relFold, ll_len] at *; omega
  | Cons x t ih =>
      intro init final h
      simp only [ll_relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [ll_len]; omega
grind_pattern ll_relFold_count_exact => ll_relFold r xs init final
|lean}]

val empty : (u : unit) -> t{ _ = ll_nil }
val cons : (x : int) -> (l : t) -> t{ _ = ll_cons x l }
val is_empty : (l : t) -> bool{ _ = ll_isnil l }
val length : (l : t) -> int{ _ = ll_len l }
val mem : (x : int) -> (l : t) -> bool{ _ = ll_mem x l }
val append : (a : t) -> (b : t) -> t{ _ = ll_app a b }

(* First-order destructors. With the repr EXPOSED a client can also just
   `match l with Nil -> .. | Cons (x, r) -> ..` and recurse structurally --
   head/tail are retained for callers that prefer the accessor form and for
   the ll_cons_head_tail reconstruction algebra. *)
val head : (l : t) -> int{ _ = ll_head l }
val tail : (l : t) -> t{ _ = ll_tail l }

(* ===== HOF surface (WP-0) ===== *)
val rev : (l : t) -> t{ _ = ll_rev l }
val nth : (i : int) -> (l : t) -> int{ _ = ll_nth i l }
val map :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (l : t) -> t{ ll_listRel r l _ }
val filter :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (l : t) -> t{ ll_allP p _ }
val fold_left :
  (r : ((int -> int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (l : t) -> int{ ll_relFold r l init _ }
val for_all :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (l : t) -> bool{ _ = ll_allP p l }
val exists :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (l : t) -> bool{ _ = ll_exP p l }
val find_opt :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (l : t) -> Voption.t{ ll_find_result p l _ }
