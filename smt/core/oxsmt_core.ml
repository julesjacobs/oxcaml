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
