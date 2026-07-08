(* Vrel: relational specifications for higher-order functions -- the first
   genuinely higher-order entries in the verified stdlib.  A relation is a
   dependent parameter of function type [(r : (int -> int -> bool))]; a
   client supplies it at a CALL SITE as an ordinary OCaml lambda
   ([iter (fun x y -> y >= x) f n]) which is reflected to a Lean
   [fun .. => ..] and substituted at the binder (task #68), or as a named
   [@@vox.reflect] value.  The combinators (iter / map / fold / filter /
   compose2) are specified by RELATING their result to their input through
   the relation, which dodges function-argument modeling: the callback [f]
   is never modeled, only its per-element contract -- the relation [r] --
   which is passed WHOLE to the block-defined lifting operators (rHolds /
   relIter / listRel), never applied directly.  Fully checked, TCB-free:
   the relation lambda is a Lean term derived from the OCaml body, so the
   correspondence is checked, not assumed.  See
   docs/plans/2026-07-07-vox-higher-order-refinements-study.md (D1) and
   -vox-lambda-reflection.md.

   EXACT-OUTPUT ("complete") specs: a client that picks [r] to be the GRAPH
   of the callback (e.g. [fun x y -> y = x + 1]) pins the result EXACTLY.
   At a symbolic count the relational spec must be closed by an induction
   lemma; those are the [_exact] law family below, shipped as proved public
   theorems so a client's [_ = x0 + k] / [_ = il_sum xs] goal fires them via
   grind_pattern.  The lemmas are stated over an ABSTRACT relation with the
   graph property as a PREMISE (never a lambda in the pattern -- grind
   arithmetic-normalizes lambda bodies at indexing, so a lambda-containing
   grind_pattern never fires; a variable-[r] pattern fires and the premise
   discharges by beta against the reflected call-site lambda).

   Module-specific house notes (notes/vrel.md):
   - IntRel / IntPred / IntRel3 are [abbrev]s (reducible), NOT [def]s: the
     S_arrow binder [(r : (int -> int -> bool))] emits as [Int -> Int -> Prop]
     in the VC, and across the import boundary an opaque [def IntRel] does not
     unfold to unify against [IntRel]-typed defs (rHolds etc.) -- Lean
     "application type mismatch".  An [abbrev] is reducible, so it unifies.
   - the relation binder's function type MUST be parenthesised; the
     dependent-binder grammar accepts only an atomic inner type (parser
     limitation, study doc).  Holds for the ternary [(r : (int->int->int->bool))]
     of [fold3] too: 3-ary S_arrow reflects to [Int -> Int -> Int -> Prop].
   - the lifting / algebra defs are [expose]d so a client unfolds them at
     concrete OR symbolic relations; only the load-bearing length law
     listRel_len ships as an obligation (axiom here / theorem in the .ml). *)

open Vhof
type ilist = Inil | Icons of int * ilist

[%%vox.lean {lean|


-- The relation algebra, over ABSTRACT relations (combinators the study's F9
-- proves compose generically).  All exposed so a client unfolds them.
@[grind, expose] public def rcomp (r s : IntRel) : IntRel :=
  fun a c => exists b, r a b /\ s b c
@[grind, expose] public def rand (r s : IntRel) : IntRel := fun a b => r a b /\ s a b
@[grind, expose] public def ror  (r s : IntRel) : IntRel := fun a b => r a b \/ s a b
@[grind, expose] public def rconverse (r : IntRel) : IntRel := fun a b => r b a

-- relIter r n x y : y is reached from x by n r-steps.  relIterN is the
-- structural Nat fixpoint in PREPEND form, so a forward recursion matches it
-- definitionally (no induction lemma); relIter feeds it [n.toNat] so an
-- OCaml [n : int] contract type-checks.
@[grind, expose] public def relIterN (r : IntRel) : Nat -> Int -> Int -> Prop
  | 0,     x, y => x = y
  | (k+1), x, y => exists z, r x z /\ relIterN r k z y
@[grind, expose] public def relIter (r : IntRel) (n : Int) (x y : Int) : Prop :=
  relIterN r n.toNat x y

-- toNat bridges: reduce Int-fuel at the two shapes a forward recursion hits;
-- a client folding over a CONCRETE list needs these to evaluate
-- [(il_len l).toNat] and unfold relIterN.  Pure arithmetic, proved here.
@[grind] public theorem toNat_nonpos (m : Int) (h : m <= 0) : m.toNat = 0 := by omega
@[grind] public theorem toNat_succ (m : Int) (h : 1 <= m) :
    m.toNat = (m - 1).toNat + 1 := by omega

-- listRel r a b : b is pointwise r-related to a (same length).
@[grind, expose] public def listRel (r : IntRel) :
    Vox_Vrel_ilist -> Vox_Vrel_ilist -> Prop
  | .Inil, .Inil => True
  | .Icons a s, .Icons b t => r a b /\ listRel r s t
  | _, _ => False

@[grind, expose] public def il_len : Vox_Vrel_ilist -> Int
  | .Inil => 0
  | .Icons _ t => 1 + il_len t

-- il_sum / ihead / itail : list accessors a client names to state EXACT
-- element-level consequences (map's pointwise output, fold3's sum).  ihead /
-- itail return junk (0 / Inil) on Inil, so exact goals target known positions.
@[grind, expose] public def il_sum : Vox_Vrel_ilist -> Int
  | .Inil => 0
  | .Icons x t => x + il_sum t
@[grind, expose] public def ihead : Vox_Vrel_ilist -> Int
  | .Inil => 0
  | .Icons x _ => x
@[grind, expose] public def itail : Vox_Vrel_ilist -> Vox_Vrel_ilist
  | .Inil => .Inil
  | .Icons _ t => t

-- relFold r xs init final : folding xs from init with a TERNARY step relation
-- r (acc, elem, acc') reaches final.  Structural over the list, so fold3's
-- forward recursion matches it definitionally (no fuel / toNat, unlike iter).
@[grind, expose] public def relFold (r : IntRel3) :
    Vox_Vrel_ilist -> Int -> Int -> Prop
  | .Inil, init, final => init = final
  | .Icons x t, init, final => exists acc, r init x acc /\ relFold r t acc final

-- allP p a : every element of a satisfies p.
@[grind, expose] public def allP (p : IntPred) : Vox_Vrel_ilist -> Prop
  | .Inil => True
  | .Icons x t => pHolds p x /\ allP p t

-- The one load-bearing law: a pointwise relation preserves length.  This is
-- the plain consequence a [map] client names without mentioning the (lambda)
-- relation.  Shipped as an obligation (axiom here, theorem in the .ml).
public axiom listRel_len (r : IntRel) (a b : Vox_Vrel_ilist) :
    listRel r a b -> il_len a = il_len b
grind_pattern listRel_len => listRel r a b

-- EXACT-OUTPUT laws (proved public theorems -- they ride the VoxSig olean to
-- clients; grind_pattern on the relational-spec term fires them on a client's
-- exact [_ = ..] goal).  Each is stated over an ABSTRACT relation with the
-- callback's graph as a PREMISE (see the header note on the lambda-pattern
-- trap).  The [+1] / [+x] step is the shipped form; a general step constant
-- [c] is provable but its grind_pattern cannot bind [c] (notes/vrel.md).

-- iter: n [+1]-steps from x reach x + n.
public theorem relIterN_succ_exact (r : IntRel) (hr : ∀ a b, r a b → b = a + 1) :
    ∀ (n : Nat) (x y : Int), relIterN r n x y → y = x + n := by
  intro n
  induction n with
  | zero => intro x y h; simp [relIterN] at h; omega
  | succ m ih =>
      intro x y h
      simp only [relIterN] at h
      obtain ⟨z, hz, hrest⟩ := h
      have h1 := hr x z hz
      have h2 := ih z y hrest
      omega
public theorem relIter_succ_exact (r : IntRel) (k x y : Int)
    (hr : ∀ a b, r a b → b = a + 1) (hk : k ≥ 0)
    (h : relIter r k x y) : y = x + k := by
  have hn := relIterN_succ_exact r hr k.toNat x y (by simpa [relIter] using h)
  omega
grind_pattern relIter_succ_exact => relIter r k x y

-- fold3 sum: a [c = a + x] step over xs from init reaches init + il_sum xs.
public theorem relFold_sum_exact (r : IntRel3) (hr : ∀ a x c, r a x c → c = a + x) :
    ∀ (xs : Vox_Vrel_ilist) (init final : Int),
      relFold r xs init final → final = init + il_sum xs := by
  intro xs
  induction xs with
  | Inil => intro init final h; simp only [relFold, il_sum] at *; omega
  | Icons x t ih =>
      intro init final h
      simp only [relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [il_sum]; omega
grind_pattern relFold_sum_exact => relFold r xs init final

-- fold3 count: a [c = a + 1] step over xs from init reaches init + il_len xs.
public theorem relFold_count_exact (r : IntRel3) (hr : ∀ a x c, r a x c → c = a + 1) :
    ∀ (xs : Vox_Vrel_ilist) (init final : Int),
      relFold r xs init final → final = init + il_len xs := by
  intro xs
  induction xs with
  | Inil => intro init final h; simp only [relFold, il_len] at *; omega
  | Icons x t ih =>
      intro init final h
      simp only [relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [il_len]; omega
grind_pattern relFold_count_exact => relFold r xs init final
|lean}]

(* iter r f x0 n : apply f n times from x0.  The result is related to x0 by
   n r-steps.  f's per-element contract IS the relation r.  With r the
   callback's GRAPH, relIter_succ_exact pins the result: [_ = x0 + n]. *)
val iter :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (x0 : int) -> (n : int) -> int{ relIter r n x0 _ }

(* map r f xs : the result is pointwise r-related to xs (probe7 shape).  With
   r the callback's graph, ihead / itail name the exact element outputs. *)
val map :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (xs : ilist) -> ilist{ listRel r xs _ }

val length : (l : ilist) -> int{ _ = il_len l }

(* fold r f init xs : a left fold whose step f is r-related (acc to acc');
   over a list of length n the result is [init] after n r-steps. *)
val fold :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ rHolds r acc _ })) ->
  (init : int) -> (xs : ilist) -> int{ relIter r (il_len xs) init _ }

(* fold3 r f init xs : a left fold whose step relation r is TERNARY -- it
   relates (acc, element, acc'), so it can depend on the element (unlike the
   binary [fold] above).  The result is [init] relFold-ed over xs.  With r the
   step's graph, relFold_sum_exact / relFold_count_exact pin the result:
   sum ([c = a + x]) gives [_ = init + il_sum xs], count ([c = a + 1]) gives
   [_ = init + il_len xs]. *)
val fold3 :
  (r : ((int -> int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (xs : ilist) -> int{ relFold r xs init _ }

(* filter p test xs : keep the elements [test] accepts.  test's contract ties
   its bool result to [pHolds p x], so every KEPT element satisfies p. *)
val filter :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (xs : ilist) -> ilist{ allP p _ }

(* compose2 r s f g x : an r-step then an s-step is an (rcomp r s)-step. *)
val compose2 :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (s : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (g : ((y : int) -> int{ rHolds s y _ })) ->
  (x : int) -> int{ rHolds (rcomp r s) x _ }
