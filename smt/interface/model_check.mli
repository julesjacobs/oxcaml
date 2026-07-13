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

(** [eval_value model t] evaluates [t] under [model] with the SAME fail-closed /
    overflow-guarded evaluator as {!check}: [Some v] on success, [None] on any missing
    binding / type error / overflow. Exposed for the W1b equality-elimination presolve
    ({!Session}), which re-derives each eliminated variable's value from its definition at
    model-build time; a [None] leaves the variable unbound so R1 ({!check}) fails closed
    to [unknown] rather than admitting a wrong value. *)
val eval_value : Cdclt.sort_card list * Cdclt.binding list -> Term.t -> Cdclt.value option
