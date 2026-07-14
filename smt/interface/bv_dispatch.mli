(** The session's QF_BV entry: decide whether an assertion set is PURE bit-vector, and if
    so solve it by eager bit-blasting ({!Oxsmt_bitblast}) instead of the Nelson-Oppen
    combinator (which fail-closed degrades any live bit-vector term to [unknown]).

    Kept separate from {!Session} so the session edit is a couple of lines and the
    bit-blaster dependency lives here. Soundness rests on two things: the {!is_pure_bv}
    gate is conservative (anything it does not fully recognise as QF_BV — an uninterpreted
    application, arithmetic, a foreign sort — is NOT pure, so it never routes a formula
    the blaster would mis-handle), and {!solve} returns [Sat] only for a model {!Bv_solve}
    already re-checked with the independent evaluator. *)

open Oxsmt_core

(** [true] iff every assertion is built solely from bit-vector operators/literals,
    bit-vector or Boolean variables, (dis)equalities over those, and Boolean connectives,
    AND at least one bit-vector term is present. Conservative: an unrecognised construct
    makes it [false]. Deterministic; memoized over the hash-consed DAG. *)
val is_pure_bv : Term.t list -> bool

type result =
  | Unsat
  | Unknown
  | Sat of
      { bv_vars : (string * Bigint.t * int) list
        (** one entry per free bit-vector variable: [(name, unsigned_value, width)] *)
      ; bool_vars : (string * bool) list
        (** one entry per free Boolean variable: [(name, truth_value)] *)
      }

(** Eager-bit-blast the (pure-QF_BV) assertion set. Only call when {!is_pure_bv} holds. *)
val solve : Term.t list -> result
