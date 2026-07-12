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
