(** Core term layer: sorts, interned symbols, hash-consed terms, sort-checking smart
    constructors, and the theory view.

    Responsibility: own the [Term.t] / [Sort.t] representation (ADR-0003). Terms are a
    [private] type built only through {!Context}'s sort-checking, normalizing,
    hash-consing smart constructors, so "any [Term.t] in existence is well-sorted and
    hash-consed" (INVARIANTS.md I1/I2) holds by construction.

    This module re-exports the public surface. The construction machinery lives in the
    library-private [Node] module (dune [private_modules]) and is unreachable from
    outside; likewise the no-copy [Iarr_unsafe] cast. *)

module Iarr = Iarr
module Symbol = Symbol
module Sort = Sort
module Rank = Rank
module Env = Env
module Term = Term
module Context = Context
module Theory_view = Theory_view
module Trail = Trail

(* ADR-0005 THEORY vocabulary, re-exported so the M4 theory adapters (the first consumers
   of the seam) can name them as [Oxsmt_core.Atom] etc. Tranche-A froze
   atom/lit/explanation/theory; [Model] is Tranche-B (M2). The frozen .mlis are unchanged
   — this only widens the library's public surface. *)
module Atom = Atom
module Lit = Lit
module Explanation = Explanation
module Theory = Theory
module Model = Model

(* ADR-0014 theory-fabric justification currency. Deliberately outside the frozen THEORY
   seam (§B.2): the combinator and its children name it as [Oxsmt_core.Fabric] to widen
   the engine tokens to [justification] without touching any frozen .mli. *)
module Fabric = Fabric

(** Test-only whitebox hook into the hash-cons bucket primitives (R1). NOT part of the
    frozen public surface and NOT for solver code — it exists so the core test suite can
    exercise [Node.equal_node] / [Node.hash_node] in isolation, i.e. detect a scalar
    payload dropped from bucket comparison alone (which differing hashes would otherwise
    mask) or from hashing alone. Read-only: it cannot construct or mutate a term. *)
module For_test = struct
  let equal_node (a : Term.t) (b : Term.t) = Node.equal_node a.node b.node
  let hash_node (a : Term.t) = Node.hash_node a.node
end
