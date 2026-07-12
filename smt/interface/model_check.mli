(** R1 (ADR-UF-models §3): the obligatory solver-side, in-process model self-check.
    [check model assertions] evaluates every ORIGINAL asserted term under the candidate
    [model] ({!Cdclt} sort cardinalities + bindings) and returns [true] iff all hold.
    Fail-closed: [false] on any missing binding / type error / overflow, so {!Session}
    degrades an un-self-checkable [sat] to [unknown]. [oxsmt_core] + {!Cdclt} vocabulary
    only; does NOT import [tests/eval] (the N-version external validator stays
    independent). A fail-closed witness/self-cert guard, not the verdict authority
    (soundness rests on the combination). *)

open Oxsmt_core

val check : Cdclt.sort_card list * Cdclt.binding list -> Term.t list -> bool

(** The R1 fold's overflow-guarded int add/mul (raise [Bad] on overflow, INCLUDING the
    [min_int * -1] / [-1 * min_int] wrap clause). Exposed only so the test suite can pin
    them in guard PARITY with {!Cdclt.add_ovf}/[mul_ovf]: {!Cdclt}'s §10-v2 gap-B
    structural fold must agree with this R1 [ev] fold on every table key, incl. the
    [min_int] edge no solver path reaches (task #117). Not used outside the self-check. *)
val add_ovf : int -> int -> int

val mul_ovf : int -> int -> int
