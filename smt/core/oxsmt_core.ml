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

(** Test-only whitebox hook into the hash-cons bucket primitives (R1). NOT part of the
    frozen public surface and NOT for solver code — it exists so the core test suite can
    exercise [Node.equal_node] / [Node.hash_node] in isolation, i.e. detect a scalar
    payload dropped from bucket comparison alone (which differing hashes would otherwise
    mask) or from hashing alone. Read-only: it cannot construct or mutate a term. *)
module For_test = struct
  let equal_node (a : Term.t) (b : Term.t) = Node.equal_node a.node b.node
  let hash_node (a : Term.t) = Node.hash_node a.node
end
