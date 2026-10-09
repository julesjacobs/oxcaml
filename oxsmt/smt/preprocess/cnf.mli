(** Abstract CNF produced by the Tseitin clausifier (DESIGN.md §5).

    The types here are deliberately {b self-contained}: {!Lit.t}/{!Clause.t}/{!t} live
    over this module's own [int] variable ids, not over the SAT core's literal type. The
    clausifier never links [smt/solver]; the two are wired together at M1-end, where a
    trivial map turns these DIMACS-style signed ids into the SAT core's literals. This
    keeps the boolean-skeleton encoding and the search engine developed in parallel
    against disjoint surfaces.

    A variable is a positive [int] in [1 .. num_vars]. Each variable stands for one
    Bool-sorted subterm of the input (its {!subterm_of_var}). Two kinds of variable:

    - {b atom variables} — the opaque theory atoms of the boolean skeleton
      ([Theory_view.is_atom]: [Le], non-Bool [Eq], Bool-codomain [App], [Bool_const]). The
      solver hands these to the theories; {!atom_of_var}/{!var_of_atom} expose the
      mapping.
    - {b auxiliary variables} — Tseitin definitions for compound connectives
      ([And]/[Or]/[Eq]-as-iff/Bool-[Ite]). Not atoms; {!atom_of_var} is [None].

    [Not] never gets its own variable: the literal of [Not a] is the negation of the
    literal of [a] (plain literal sharing, not a polarity optimization).

    {b Determinism (INVARIANTS.md I6).} Variable numbering follows the terms' hash-cons
    {e tag} order (ascending), never hash-table iteration order, so a fixed input yields
    an identical CNF — same var ids, same clauses, same map — every run. *)

open Oxsmt_core

module Lit : sig
  (** A literal: a variable together with a polarity. *)
  type t

  (** [make v ~positive] is the literal on variable [v] ([v >= 1]); [positive] false is
      its negation. *)
  val make : int -> positive:bool -> t

  val var : t -> int
  val is_positive : t -> bool
  val negate : t -> t

  (** DIMACS-style signed encoding: [+v] for a positive literal on [v], [-v] for its
      negation. Injective; handy for printing and for equality in tests. *)
  val to_dimacs : t -> int

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Clause : sig
  (** A disjunction of literals. The empty clause is [false] (an immediate contradiction);
      a one-literal clause is a unit. *)
  type t = Lit.t list
end

type t

(** [clausify formula] Tseitin-encodes the Bool-sorted [formula] into CNF. [formula] is
    expected to be a preprocessed term (no Int-sorted [Ite], no reserved [div]/[mod] — see
    {!Preprocess}); the clausifier itself only reads structure and constructs no terms, so
    it needs no {!Context}. Theory atoms become opaque variables via the atom map; every
    connective is encoded with its full biconditional (plain Tseitin v1 —
    Plaisted-Greenbaum polarity pruning is a future optimization).

    The result is equisatisfiable with [formula]: for every assignment of the atom
    variables, [formula] is true under it iff the (uniquely determined) Tseitin extension
    satisfies all clauses. *)
val clausify : Term.t -> t

val num_vars : t -> int
val clauses : t -> Clause.t list
val iter_clauses : (Clause.t -> unit) -> t -> unit

(** [subterm_of_var t v] is the Bool-sorted subterm that variable [v] denotes (total for
    [1 <= v <= num_vars]); an atom for atom variables, a connective node for auxiliary
    ones. Raises [Invalid_argument] out of range. *)
val subterm_of_var : t -> int -> Term.t

(** [is_atom_var t v] is whether [v] is an opaque theory atom (as opposed to a Tseitin
    auxiliary variable). *)
val is_atom_var : t -> int -> bool

(** [atom_of_var t v] is [Some a] when [v] is an atom variable standing for atom [a], else
    [None]. *)
val atom_of_var : t -> int -> Term.t option

(** [var_of_atom t a] is [Some v] when atom [a] occurs in the formula as variable [v],
    else [None]. *)
val var_of_atom : t -> Term.t -> int option
