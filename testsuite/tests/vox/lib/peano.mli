(* Peano numbers behind a via-abstracted sealed interface, specced as
   Lean's BUILT-IN [Nat].  The representation is a unary [pnat] ([Z]/[S]),
   but the .mli hides it: [t] is [refines (lnat)], where [lnat] is a ghost
   sort naming Lean's native [Nat] -- so a client binds [t] at [Nat] and
   reasons in ordinary ARITHMETIC vocabulary ([0], [+], [n + 1]).  There
   is NO [%%vox.lean] block: [Nat] is resolved natively by Lean, its
   theory (linear arithmetic, decidability) comes for free, and nothing
   about the vocabulary is authored here.  Under image-binder the .ml
   PROVES these specs honestly (no [assume_unchecked_]).

   THE THREE-WAY MODELING SPECTRUM (this is its third point):
   - lib/via_set.mli -- an INDUCTIVE model ([ISet := cons/nil]): decidable
     observables ([card]) and concrete counterexamples, but NOT
     extensional ([cons x (cons x s) <> cons x s]).
   - lib/xset.mli -- a PROP-SET model ([ISet := Int -> Prop], Lean's set
     type): EXTENSIONAL ([ins x (ins x s) = ins x s]), witness-free
     failures, but no [card].
   - HERE -- a BUILT-IN model ([Nat]): full arithmetic AUTOMATION (grind's
     linear arithmetic closes commutativity that the unary side would
     need induction for) AND witnesses (overclaims print concrete Nat
     counterexamples).  The carrier is a real Lean type the compiler
     never defines, so no block travels at all.

   See lib/bignum.mli for a DIFFERENT representation (binary) under this
   SAME interface: one arithmetic spec, two machines. *)
type lnat [@@vox.sort lean "Nat"]
type t : value refines (lnat)

val zero : t{ _ = 0 }
val succ : (n : t) -> t{ _ = n + 1 }
val add  : (a : t) -> (b : t) -> t{ _ = a + b }
