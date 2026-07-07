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

   Module-specific house notes (notes/vrel.md):
   - IntRel / IntPred are [abbrev]s (reducible), NOT [def]s: the S_arrow
     binder [(r : (int -> int -> bool))] emits as [Int -> Int -> Prop] in
     the VC, and across the import boundary an opaque [def IntRel] does not
     unfold to unify against [IntRel]-typed defs (rHolds etc.) -- Lean
     "application type mismatch".  An [abbrev] is reducible, so it unifies.
   - the relation binder's function type MUST be parenthesised; the
     dependent-binder grammar accepts only an atomic inner type (parser
     limitation, study doc).
   - the lifting / algebra defs are [expose]d so a client unfolds them at
     concrete OR symbolic relations; only the load-bearing length law
     listRel_len ships as an obligation (axiom here / theorem in the .ml). *)

type ilist = Inil | Icons of int * ilist

[%%vox.lean {lean|
public abbrev IntRel := Int -> Int -> Prop
public abbrev IntPred := Int -> Prop

-- rHolds / pHolds: the pass-whole idiom.  A relation parameter is only ever
-- APPLIED inside Lean, through these wrappers, so it appears in the VC as a
-- bare hypothesis and grind unfolds the wrapper against the substituted
-- lambda.  Exposed: a client beta-reduces [rHolds (fun p q => ..) a b].
@[grind, expose] public def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind, expose] public def pHolds (p : IntPred) (x : Int) : Prop := p x

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
|lean}]

(* iter r f x0 n : apply f n times from x0.  The result is related to x0 by
   n r-steps.  f's per-element contract IS the relation r. *)
val iter :
  (r : (int -> int -> bool)) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (x0 : int) -> (n : int) -> int{ relIter r n x0 _ }

(* map r f xs : the result is pointwise r-related to xs (probe7 shape). *)
val map :
  (r : (int -> int -> bool)) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (xs : ilist) -> ilist{ listRel r xs _ }

val length : (l : ilist) -> int{ _ = il_len l }

(* fold r f init xs : a left fold whose step f is r-related (acc to acc');
   over a list of length n the result is [init] after n r-steps. *)
val fold :
  (r : (int -> int -> bool)) ->
  (f : ((acc : int) -> (x : int) -> int{ rHolds r acc _ })) ->
  (init : int) -> (xs : ilist) -> int{ relIter r (il_len xs) init _ }

(* filter p test xs : keep the elements [test] accepts.  test's contract ties
   its bool result to [pHolds p x], so every KEPT element satisfies p. *)
val filter :
  (p : (int -> bool)) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (xs : ilist) -> ilist{ allP p _ }

(* compose2 r s f g x : an r-step then an s-step is an (rcomp r s)-step. *)
val compose2 :
  (r : (int -> int -> bool)) ->
  (s : (int -> int -> bool)) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (g : ((y : int) -> int{ rHolds s y _ })) ->
  (x : int) -> int{ rHolds (rcomp r s) x _ }
