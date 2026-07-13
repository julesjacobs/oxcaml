(** A direct, independent OCaml evaluator for QF_BV over concrete values — the circuit
    oracle's ground truth and the sat-model checker. It computes with {!Bigint} arithmetic
    and the exact SMT-LIB total-function semantics (including division/shift by zero /
    over-width), sharing NONE of {!Blast}'s Tseitin machinery, so agreement between the
    two is real evidence the circuits are right.

    [lookup] resolves a free bit-vector or Boolean variable to its value; on an unbound
    variable, or a construct outside QF_BV, {!Eval_error} is raised. *)

open Oxsmt_core

exception Eval_error of string

(** Value of a bit-vector-sorted term: [(v, width)] with [0 <= v < 2^width]. *)
val eval_bv : Blast.defs -> lookup:(Term.t -> Bigint.t option) -> Term.t -> Bigint.t * int

(** Truth of a Bool-sorted term. *)
val eval_bool : Blast.defs -> lookup:(Term.t -> Bigint.t option) -> Term.t -> bool
