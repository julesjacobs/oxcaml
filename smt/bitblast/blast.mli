(** Eager bit-blasting of QF_BV terms into the propositional SAT core
    ({!Oxsmt_solver.Sat}). Each n-bit term becomes [n] SAT literals (a "bit vector", LSB
    at index 0); each operator becomes a Tseitin-encoded circuit whose defining clauses
    are added to the shared solver. Boolean structure (the connectives, and the bit-vector
    predicates/equalities) blasts to a single literal.

    This is a PRESOLVE-style whole-formula transform, NOT a theory client: nothing is
    added to the SAT core, no combinator/theory is involved. Two consequences the review
    turns on: (1) a [Sat] model reads straight back off the bit assignment as a concrete
    bit-vector value per variable; (2) the [Unsat] path is pure propositional, so the
    existing RUP certificate checker applies ARCHITECTURALLY unchanged — bit-blasting adds
    no theory leaves. (v1 caveat: certificate EMISSION for the bit-blasting path is not
    yet wired — the property is that the refutation is ordinary propositional resolution
    with nothing for the checker to special-case, not that a certificate is emitted
    today.)

    Every encoding is equisatisfiable: Tseitin introduces fresh variables but never drops
    a constraint. An operator this version cannot encode raises {!Unsupported_bv}; the
    driver turns that into [unknown] (never a wrong verdict). *)

open Oxsmt_core

(** The classification of a bit-vector term the blaster reads. [Const] is a literal
    ([value] canonical in [0, 2^width)). [Op] carries the operator, its operand terms
    (each carries its own bit-vector sort, so operand widths come off them), and
    [result_width] = [Some w] for a bit-vector result, [None] for a Bool-valued
    comparison. A free bit-vector variable is NOT classified here: it is a term for which
    {!field-classify} returns [None] while {!field-width_of_sort} returns [Some w]. *)
type view =
  | Const of Bigint.t * int
  | Op of Bv_op.t * Term.t list * int option

(** How the blaster classifies a term and recovers a sort's width. Supplied by the caller
    so the circuit library is independent of the term representation: pre-handoff it is
    backed by {!Bv_defs_stub}; post-rebase by a thin adapter over bv-front's [Bv.view].
    [classify] returns [None] for a term that is not a bit-vector operator/literal minted
    by the front end (a free bit-vector variable, or a non-bit-vector term). *)
type defs =
  { classify : Term.t -> view option
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
