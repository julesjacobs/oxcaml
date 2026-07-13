(** The QF_BV solve driver: bit-blast the assertions into a fresh SAT solver, solve, and
    on [Sat] read a concrete bit-vector model straight off the bit assignment.

    Soundness net: before returning [Sat] the driver re-evaluates every assertion under
    the recovered model with the independent {!Bv_eval}; a model that does not satisfy the
    formula degrades to [Unknown] rather than being reported. Combined with the
    fail-closed [Unknown] on any unencodable construct, the driver never returns a wrong
    verdict — the only emergency. (The [Unsat] path is pure propositional and needs no
    such net: it is exactly the SAT core's refutation.) *)

open Oxsmt_core

(** A model: each free variable with its value and width. *)
type model = (Term.t * (Bigint.t * int)) list

type verdict =
  | Sat of model
  | Unsat
  | Unknown of string

val solve : Blast.defs -> Term.t list -> verdict
