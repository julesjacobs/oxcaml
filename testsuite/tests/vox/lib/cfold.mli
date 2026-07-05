(* A verified constant folder behind a via-ABSTRACTED sealed interface.
   [t] is an arithmetic expression, but the .mli hides the tree: [t] is
   declared [refines (lint)] where [lint] names Lean's BUILT-IN [Int],
   so a client binds [t] AT the integer sort and every spec is stated in
   plain [Int] arithmetic ([+], [*], [=]).  Because the vocabulary is
   the native one, the interface needs NO [%%vox.lean] block at all.

   The star spec is [fold : (e : t) -> t{ _ = e }]: constant folding
   PROVED denotation-preserving in one refinement line, since a [t]
   binder denotes its Int and [_ = e] is equal-denotation.  [eval]
   returns that same Int.  Clients get [eval (fold e) = eval e] for
   free -- pure image algebra, no view of the representation. *)

type lint [@@vox.sort lean "Int"]
type t : value refines (lint)

(* Smart constructors: a client builds expressions directly at [t], so
   the tree behind [t] never escapes the unit. *)
val lit : (n : int) -> t{ _ = n }
val add : (a : t) -> (b : t) -> t{ _ = a + b }
val mul : (a : t) -> (b : t) -> t{ _ = a * b }

(* Constant folding, proved to preserve the denotation. *)
val fold : (e : t) -> t{ _ = e }

(* The evaluator agrees with the denotation. *)
val eval : (e : t) -> int{ _ = e }
