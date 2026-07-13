(** Eager bit-blasting of QF_BV terms into the propositional SAT core
    ({!Oxsmt_solver.Sat}). Each n-bit term becomes [n] SAT literals (a "bit vector", LSB
    at index 0); each operator becomes a Tseitin-encoded circuit whose defining clauses
    are added to the shared solver. Boolean structure (the connectives, and the bit-vector
    predicates/equalities) blasts to a single literal.

    This is a PRESOLVE-style whole-formula transform, NOT a theory client: nothing is
    added to the SAT core, no combinator/theory is involved. Two consequences the review
    turns on: (1) a [Sat] model reads straight back off the bit assignment as a concrete
    bit-vector value per variable; (2) the [Unsat] path is pure propositional, so the
    existing RUP certificate checker applies unchanged — bit-blasting adds no theory
    leaves.

    Every encoding is equisatisfiable: Tseitin introduces fresh variables but never drops
    a constraint. An operator this version cannot encode raises {!Unsupported_bv}; the
    driver turns that into [unknown] (never a wrong verdict). *)

open Oxsmt_core

(** How the blaster classifies an [App] symbol and recovers a sort's width. Supplied by
    the caller so the circuit library is independent of the term/registry representation:
    pre-handoff it is backed by {!Bv_defs_stub}; post-rebase by a thin adapter over
    bv-front's [Bitvec_defs]. [op_of_sym] returns [None] for a symbol that is not a
    bit-vector operator (e.g. a plain uninterpreted function). *)
type defs =
  { op_of_sym : Symbol.t -> Bv_op.t option
  ; width_of_sort : Sort.t -> int option
  }

type t

(** Raised on any QF_BV construct this version does not encode (an unsupported operator,
    an uninterpreted function over bit-vectors, or a non-BV/non-Bool atom). The driver
    catches it and degrades the query to [unknown]. Message names the construct. *)
exception Unsupported_bv of string

(** A fresh blaster over a fresh SAT solver. *)
val create : defs -> t

(** The underlying SAT solver (to add assertion units and to [solve]). *)
val sat : t -> Oxsmt_solver.Sat.t

(** Blast a Bool-sorted term to a single literal that is true iff the term holds. Memoized
    on the hash-consed term, so a shared subterm blasts once (DAG blasting). *)
val blast_bool : t -> Term.t -> Oxsmt_solver.Sat.lit

(** Blast a Bool-sorted assertion and add it as a unit clause (the term must hold). *)
val assert_term : t -> Term.t -> unit

(** The free bit-vector variables encountered, each with its bit literals (LSB first), in
    first-encounter order — the model read-back set. *)
val bv_vars : t -> (Term.t * Oxsmt_solver.Sat.lit array) list

(** [bits t term] blasts a bit-vector-sorted term to its bit literals (LSB first). Exposed
    for the exhaustive circuit oracle; the driver only needs {!assert_term}. *)
val bits : t -> Term.t -> Oxsmt_solver.Sat.lit array
