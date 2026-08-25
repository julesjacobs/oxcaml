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
    to [unknown] rather than admitting a wrong value.

    Builds the lookup tables fresh on each call. A caller re-deriving MANY variables
    against an evolving model must instead hold one {!tables} ({!tables_of_bindings}) and
    use {!eval_in} / {!add_const}, so table construction is not repeated per variable. *)
val eval_value : Cdclt.sort_card list * Cdclt.binding list -> Term.t -> Cdclt.value option

(** Reusable, mutable evaluation tables (nullary consts + function tables, keyed by symbol
    name) for the same fail-closed evaluator as {!check}/{!eval_value}. Built once from a
    binding list and mutated in place, so re-deriving N variables against a model that
    grows by those N bindings is O(N + bindings) rather than O(N x bindings). *)
type tables

(** [tables_of_bindings bindings] builds the evaluation tables from [bindings]
    (O(bindings)). A repeated symbol name resolves to the LAST binding for it in list
    order, matching the former [build_tables]. *)
val tables_of_bindings : Cdclt.binding list -> tables

(** [add_const tbls name v] binds nullary [name] to [v] in [tbls], overwriting any prior
    binding for [name] (last-writer-wins, as in {!tables_of_bindings}). *)
val add_const : tables -> string -> Cdclt.value -> unit

(** [eval_in tbls t] evaluates [t] under [tbls]: [Some v] on success, [None] on any
    missing binding / type error / overflow. Identical semantics to {!eval_value}, reusing
    [tbls]. *)
val eval_in : tables -> Term.t -> Cdclt.value option
